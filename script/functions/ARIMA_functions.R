stationarity_tests <- function(data, vars = NULL, countries = NULL) {
  
  # Filtrer les pays si nécessaire
  if (!is.null(countries)) {
    data <- data %>% filter(Pays %in% countries)
  }
  
  # Si vars est NULL, prendre toutes les colonnes numériques sauf 'time' et 'Pays'
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric) & !names(data) %in% c("time")]
  }
  
  extract_test_results <- function(test_result, test_name, var_name, country_name) {
    stationarity <- if (grepl("KPSS", test_name)) {
      ifelse(test_result$p.value > 0.05, "Stationnaire", "Non-Stationnaire")
    } else {
      ifelse(test_result$p.value < 0.05, "Stationnaire", "Non-Stationnaire")
    }
    
    rowname <- paste0(test_name, " - ", var_name)  # juste test + variable
    
    df <- data.frame(
      Pays = country_name,
      Statistique = round(test_result$statistic, 4),
      P_Value = round(test_result$p.value, 4),
      Stationnarite = stationarity,
      Lag = ifelse("parameter" %in% names(test_result), test_result$parameter, NA),
      stringsAsFactors = FALSE
    )
    
    rownames(df) <- rowname
    return(df)
  }
  
  results_table <- data.frame()
  
  for (var in vars) {
    var_sym <- ensym(var)
    for (country in unique(data$Pays)) {
      series <- na.omit(data %>% filter(Pays == country) %>% pull(!!var_sym))
      
      if (sum(!is.na(series)) < 5) next
      
      adf_result <- adf.test(series, alternative = "stationary")
      kpss_result <- kpss.test(series, null = "Trend")
      pp_result <- pp.test(series)
      
      results_table <- rbind(
        results_table,
        extract_test_results(adf_result, "ADF", var, country),
        extract_test_results(kpss_result, "KPSS", var, country),
        extract_test_results(pp_result, "PP", var, country)
      )
    }
  }
  
  # Réordonner colonnes
  results_table <- results_table %>%
    dplyr::select(Pays, Statistique, P_Value, Stationnarite, Lag)
  
  return(results_table)
}


test_arima_models_aic_bic <- function(data = data, country, var_name = "var", p_max = 3, d = 1, q_max = 3) {
  
  data <- data[data$Pays == country, ]
  y <- data[[var_name]]
  
  
  grid <- expand.grid(p = 0:p_max, d = d, q = 0:q_max)
  models <- lapply(1:nrow(grid), function(i) as.numeric(grid[i, ]))
  model_names <- sapply(models, function(m) paste0("ARIMA", m[1], m[2], m[3]))
  
  results <- data.frame(
    Model = model_names,
    AIC = NA,
    BIC = NA
  )
  
  for (i in 1:length(models)) {
    m <- models[[i]]
    fit <- tryCatch({
      Arima(y, order = c(m[1], m[2], m[3]))
    }, error = function(e) {
      message("Modèle ", model_names[i], " n'a pas convergé : ", e$message)
      NULL
    })
    
    if (!is.null(fit)) {
      results$AIC[i] <- fit$aic
      results$BIC[i] <- AIC(fit, k = log(length(y)))
    }
  }
  
  # Trier par BIC
  results <- results[order(results$BIC), ]
  
  return(results)
}


test_arima_models_aic_bic_covid <- function(data, country, var_name = "var", xreg_var = NULL, p_max = 3, d = 1, q_max = 3) {
  
  # Filtrer le pays
  data <- data[data$Pays == country, ]
  y <- data[[var_name]]
  
  # Préparer la variable exogène si fournie
  xreg <- as.numeric(format(data$time, "%Y") %in% 2019:2021)
  xreg <- matrix(xreg, ncol = 1)
  
  
  # Créer la grille des modèles ARIMA(p,d,q)
  grid <- expand.grid(p = 0:p_max, d = d, q = 0:q_max)
  models <- lapply(1:nrow(grid), function(i) as.numeric(grid[i, ]))
  model_names <- sapply(models, function(m) paste0("ARIMAX", m[1], m[2], m[3]))
  
  # Préparer le dataframe de résultats
  results <- data.frame(
    Model = model_names,
    AIC = NA,
    BIC = NA
  )
  
  # Boucle pour estimer chaque modèle
  for (i in 1:length(models)) {
    m <- models[[i]]
    fit <- tryCatch({
      Arima(y, order = c(m[1], m[2], m[3]), xreg = xreg)
    }, error = function(e) {
      message("Modèle ", model_names[i], " n'a pas convergé : ", e$message)
      NULL
    })
    
    if (!is.null(fit)) {
      results$AIC[i] <- fit$aic
      results$BIC[i] <- AIC(fit, k = log(length(y)))
    }
  }
  
  # Trier par BIC croissant
  results <- results[order(results$BIC), ]
  
  return(results)
}


rolling_arima_errors <- function(
    data,
    var_name = "values_snf",
    p_max = 3,
    q_max = 3,
    d = 1,
    train_size = 60,
    test_size = 12,
    country = "France",
    step = 4,
    covid = FALSE,
    covid_start = NULL,
    covid_end = NULL,
    covid_windows = NULL
) {
  
  # -----------------------
  # Préparation des données
  # -----------------------
  data <- data[data$Pays == country, ]
  Z <- data[[var_name]]            # série originale
  n <- length(Z)
  
  # Grille de modèles
  grid <- expand.grid(p = 0:p_max, q = 0:q_max)
  models <- lapply(1:nrow(grid), function(i) c(grid$p[i], grid$q[i]))
  model_names <- sapply(models, function(m) paste0("ARIMA(", m[1], ",", d, ",", m[2], ")"))
  
  # Matrices d’erreurs
  errors  <- data.frame(matrix(nrow = length(models), ncol = 0))
  merrors <- data.frame(matrix(nrow = length(models), ncol = 0))
  rownames(errors)  <- model_names
  rownames(merrors) <- model_names
  
  predictions <- list()
  y_true <- list()
  
  # -----------------------
  # Fenêtre expansive (expanding window)
  # -----------------------
  start_train <- 1
  while ((start_train + train_size - 1 + test_size) <= n) {
    
    train <- Z[start_train:(start_train + train_size - 1)]
    test  <- Z[(start_train + train_size):(start_train + train_size + test_size - 1)]
    
    win_name <- paste0("window_", format(data$time[start_train + train_size + test_size - 1], "%Y"))
    errors[[win_name]]  <- NA
    merrors[[win_name]] <- NA
    
    # -----------------------
    # XREG COVID si demandé
    # -----------------------
    if (covid) {
      train_idx <- start_train:(start_train + train_size - 1)
      test_idx  <- (start_train + train_size):(start_train + train_size + test_size - 1)
      
      train_xreg <- as.numeric(train_idx >= covid_start & train_idx <= covid_end)
      test_xreg  <- as.numeric(test_idx  >= covid_start & test_idx  <= covid_end)
      
      train_xreg <- matrix(train_xreg, ncol = 1)
      test_xreg  <- matrix(test_xreg,  ncol = 1)
    }
    
    # -----------------------
    # Estimation des modèles
    # -----------------------
    for (i in seq_along(models)) {
      
      p <- models[[i]][1]
      q <- models[[i]][2]
      
      fit <- tryCatch({
        if (covid) {
          Arima(train, order = c(p, d, q), xreg = train_xreg)
        } else {
          Arima(train, order = c(p, d, q))
        }
      }, error = function(e) NULL)
      
      if (!is.null(fit)) {
        pred <- if (covid) {
          forecast(fit, xreg = test_xreg, h = length(test))$mean
        } else {
          forecast(fit, h = length(test))$mean
        }
        
        errors[i, ncol(errors)]  <- sqrt(mean((test - pred)^2))
        merrors[i, ncol(merrors)] <- sqrt(mean(((test - pred) / test)^2)) * 100
        
        predictions[[win_name]] <- pred
        y_true[[win_name]]      <- test
      }
    }
    
    # -----------------------
    # Agrandir la fenêtre de train
    # -----------------------
    train_size <- train_size + step
  }
  
  # -----------------------
  # Table de synthèse
  # -----------------------
  summary_table <- data.frame(
    Model = rownames(errors),
    Mean_RMSE = rowMeans(errors, na.rm = TRUE),
    Mean_RMSPE = rowMeans(merrors, na.rm = TRUE),
    Var_RMSE = apply(errors, 1, var, na.rm = TRUE)
  )
  
  if (!is.null(covid_windows)) {
    summary_table$Mean_RMSE_HC <- rowMeans(
      errors[, !colnames(errors) %in% covid_windows, drop = FALSE],
      na.rm = TRUE
    )
    summary_table$Var_RMSE_HC <- apply(
      errors[, !colnames(errors) %in% covid_windows, drop = FALSE],
      1, var, na.rm = TRUE
    )
    summary_table$Mean_RMSE_C <- rowMeans(
      errors[, colnames(errors) %in% covid_windows, drop = FALSE],
      na.rm = TRUE
    )
    summary_table$Var_RMSE_C <- apply(
      errors[, colnames(errors) %in% covid_windows, drop = FALSE],
      1, var, na.rm = TRUE
    )  
  }
  
  summary_table <- summary_table[order(summary_table$Mean_RMSE), ]
  
  return(list(
    errors = errors,
    merrors = merrors,
    predictions = predictions,
    y_true = y_true,
    summary_table = summary_table
  ))
}

compare_arima_errors <- function(errors_no_covid,
                                 errors_covid) {
  
  # Modèles communs
  common_models <- intersect(
    rownames(errors_no_covid),
    rownames(errors_covid)
  )
  
  errors_no_covid <- errors_no_covid[common_models, , drop = FALSE]
  errors_covid    <- errors_covid[common_models, , drop = FALSE]
  
  # Renommer les colonnes de Covid
  colnames(errors_covid) <- paste0("COVID_", colnames(errors_covid))
  
  # Fenêtres post-2020
  keep_windows <- grep("2021|2022|2023|2024|2025",
                       colnames(errors_no_covid),
                       value = TRUE)
  
  # Sous-ensembles
  no_covid_post <- as.data.frame(errors_no_covid[, keep_windows, drop = FALSE])
  covid_post    <- as.data.frame(errors_covid[, paste0("COVID_", keep_windows), drop = FALSE])
  
  # Calculer les moyennes
  RMSE_mean <- rowMeans(no_covid_post, na.rm = TRUE)
  RMSE_covid_mean <- rowMeans(covid_post, na.rm = TRUE)
  
  # Tableau final
  comparison_table <- cbind(
    no_covid_post,
    covid_post,
    RMSE_mean = RMSE_mean,
    RMSE_covid_mean = RMSE_covid_mean
  )
  
  # Forcer le data.frame et trier correctement par RMSE_mean
  comparison_table <- as.data.frame(comparison_table)
  comparison_table <- comparison_table[order(as.numeric(comparison_table$RMSE_covid_mean)), ]
  
  return(comparison_table)
}




##### ACF et PACF #####
plot_acf_pacf <- function(
    country = "France",
    data = data,
    var_name = "var",
    nom = "nom",
    ylim = c(-0.8, 1)
) {
  
  data <- subset(data, Pays == country)
  serie <- data[[var_name]]
  
  p_acf <- ggAcf(serie) +
    ggtitle(paste("ACF -", nom)) +
    ylim(ylim[1], ylim[2])
  
  p_pacf <- ggPacf(serie) +
    ggtitle(paste("PACF -", nom)) +
    ylim(ylim[1], ylim[2])
  
  gridExtra::grid.arrange(p_acf, p_pacf, ncol = 2)
}


arima_expanding_test_plot <- function(data,
                                      var_name   = "values_snf",
                                      country    = "France",
                                      train_size = 60,
                                      test_size  = 8,
                                      step       = 4,
                                      p = 1, d = 1, q = 2,
                                      covid = FALSE,
                                      covid_start = NULL,
                                      covid_end   = NULL) {
  
  # --- Filtrage pays ---
  data <- data[data$Pays == country, ]
  Z <- data[[var_name]]
  time_vals <- data$time
  n <- length(Z)
  
  if (train_size + test_size > n) {
    stop("train_size + test_size > longueur de la série")
  }
  
  df_orig <- data.frame(
    time  = time_vals,
    value = Z
  )
  
  pred_list <- list()
  
  # --- Fonction interne pour récupérer les intervalles en toute sécurité ---
  get_interval <- function(x, type = c("lower", "upper")) {
    type <- match.arg(type)
    if (is.matrix(x)) {
      if (type == "lower") return(as.numeric(x[,1]))
      if (type == "upper") return(as.numeric(x[,ncol(x)]))
    } else {
      return(as.numeric(x))
    }
  }
  
  # --- Rolling windows (expanding) ---
  for (end_train in seq(train_size, n - test_size, by = step)) {
    
    train <- Z[1:end_train]
    test  <- Z[(end_train + 1):(end_train + test_size)]
    
    # --- Déterminer si la fenêtre est post-Covid ---
    use_xreg <- covid && !is.null(covid_start) && end_train >= covid_start
    
    if (use_xreg) {
      train_xreg <- as.numeric(seq_len(end_train) >= covid_start & seq_len(end_train) <= covid_end)
      test_xreg  <- as.numeric((end_train + 1):(end_train + test_size) >= covid_start &
                                 (end_train + 1):(end_train + test_size) <= covid_end)
      train_xreg <- matrix(train_xreg, ncol = 1)
      test_xreg  <- matrix(test_xreg,  ncol = 1)
    }
    
    # --- Ajustement ARIMA / ARIMAX ---
    fit <- tryCatch({
      if (use_xreg) {
        Arima(train, order = c(p, d, q), xreg = train_xreg, method = "ML")
      } else {
        Arima(train, order = c(p, d, q), method = "ML")
      }
    }, error = function(e) NULL)
    
    if (is.null(fit)) next
    
    # --- Prévisions ---
    fc <- if (use_xreg) {
      forecast(fit, xreg = test_xreg, h = test_size, level = 95)
    } else {
      forecast(fit, h = test_size, level = 95)
    }
    
    # --- Concaténer la série connue + prévisions ---
    time_pred  <- time_vals[1:(end_train + test_size)]
    mean_pred  <- c(train, as.numeric(fc$mean))
    lower_pred <- c(train, get_interval(fc$lower, "lower"))
    upper_pred <- c(train, get_interval(fc$upper, "upper"))
    
    pred_list[[length(pred_list) + 1]] <- data.frame(
      time  = time_pred,
      mean  = mean_pred,
      lower = lower_pred,
      upper = upper_pred,
      window = paste0("Train jusqu'à ", format(time_vals[end_train], "%Y"))
    )
  }
  
  if (length(pred_list) == 0) {
    stop("Aucune prévision n'a été générée. Vérifie les paramètres ARIMA et indices COVID.")
  }
  
  # --- Fusion des prévisions ---
  pred_all <- do.call(rbind, pred_list)
  pred_all$time   <- as.Date(pred_all$time)  
  pred_all$mean   <- as.numeric(pred_all$mean)
  pred_all$lower  <- as.numeric(pred_all$lower)
  pred_all$upper  <- as.numeric(pred_all$upper)
  pred_all$window <- as.factor(pred_all$window)
  
  # --- Graphique ---
  ggplot() +
    geom_line(
      data = pred_all,
      aes(x = time, y = mean, color = window, group = window),
      linewidth = 1
    ) +
    geom_ribbon(
      data = pred_all,
      aes(x = time, ymin = lower, ymax = upper, fill = window, group = window),
      alpha = 0.15
    ) +
    geom_line(
      data = df_orig,
      aes(x = time, y = value),
      color = "black",
      linewidth = 1
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(
      title = paste(
        ifelse(covid, "ARIMAX", "ARIMA"), 
        "expanding window – test =", test_size, "trimestres –", country
      ),
      x = "Date",
      y = var_name
    ) +
    coord_cartesian(ylim = c(NA, 90))
}

#######################
arima_forecast_plot <- function(
    data,
    var_name,
    country,
    pred = 12,
    p = 1, d = 1, q = 1,
    covid = c("none", "covid", "both"),
    covid_start = 80,
    covid_end   = 92,
    start_plot = as.Date("2014-01-01"),
    freq_months = 3,
    y_min = NULL
) {
  
  covid <- match.arg(covid)
  
  # =========================
  # Filtrer le pays et trier par date
  # =========================
  data <- data[data$Pays == country, ]
  data <- data[order(data$time), ]
  
  Z    <- data[[var_name]]
  time <- data$time
  n    <- length(Z)
  
  last_date  <- tail(time, 1)
  last_value <- tail(Z, 1)
  
  # =========================
  # Fonction interne pour faire le forecast
  # =========================
  compute_forecast <- function(with_covid = FALSE) {
    
    if (with_covid) {
      xreg <- rep(0, n)
      xreg[covid_start:covid_end] <- 1
      xreg <- matrix(xreg, ncol = 1)
      
      # Dummy COVID dans le futur = 0
      xreg_future <- matrix(0, nrow = pred, ncol = 1)
      
      fit <- Arima(Z, order = c(p, d, q), xreg = xreg)
      fc  <- forecast(fit, h = pred, xreg = xreg_future)
      
    } else {
      fit <- Arima(Z, order = c(p, d, q))
      fc  <- forecast(fit, h = pred)
    }
    
    # Dates pour la prévision
    time_pred <- seq.Date(
      from = last_date + months(freq_months),
      by   = paste(freq_months, "months"),
      length.out = pred
    )
    
    # Fonction robuste pour récupérer les intervalles
    get_interval <- function(x, type = c("lower","upper")) {
      type <- match.arg(type)
      if (is.matrix(x)) {
        if (type == "lower") return(as.numeric(x[,1]))
        if (type == "upper") return(as.numeric(x[,ncol(x)]))
      } else {
        return(as.numeric(x))
      }
    }
    
    lower_pred <- c(last_value, get_interval(fc$lower, "lower"))
    upper_pred <- c(last_value, get_interval(fc$upper, "upper"))
    
    # Raccorder la dernière valeur connue à la prévision
    data.frame(
      time  = c(last_date, time_pred),
      value = c(last_value, as.numeric(fc$mean)),
      lower = lower_pred,
      upper = upper_pred
    )
  }
  
  # =========================
  # Calculer les prévisions
  # =========================
  prev_nocovid <- NULL
  prev_covid   <- NULL
  
  if (covid %in% c("none", "both")) {
    prev_nocovid <- compute_forecast(FALSE)
  }
  if (covid %in% c("covid", "both")) {
    prev_covid <- compute_forecast(TRUE)
  }
  
  # =========================
  # Graphique
  # =========================
  p_plot <- ggplot() +
    geom_line(aes(x = time, y = Z),
              color = "black", linewidth = 1) +
    theme_minimal() +
    scale_x_date(limits = c(start_plot, NA)) +
    labs(
      x = "Date",
      y = var_name,
      title = paste(country, "-", var_name)
    )
  
  if (!is.null(y_min)) {
    p_plot <- p_plot +
      coord_cartesian(ylim = c(y_min, NA))
  }
  
  if (!is.null(prev_nocovid)) {
    p_plot <- p_plot +
      geom_line(data = prev_nocovid,
                aes(x = time, y = value),
                color = "red", linewidth = 1) +
      geom_ribbon(data = prev_nocovid,
                  aes(x = time, ymin = lower, ymax = upper),
                  fill = "red", alpha = 0.2)
  }
  
  if (!is.null(prev_covid)) {
    p_plot <- p_plot +
      geom_line(data = prev_covid,
                aes(x = time, y = value),
                color = "blue", linewidth = 1) +
      geom_ribbon(data = prev_covid,
                  aes(x = time, ymin = lower, ymax = upper),
                  fill = "blue", alpha = 0.2)
  }
  
  return(p_plot)
}



check_residuals_2 <- function(res, lags = 20) {
  # Vérification que res est un vecteur numérique
  if (!is.numeric(res)) stop("res doit être un vecteur numérique de résidus")
  
  # Statistiques de base
  cat("Moyenne des résidus :", mean(res), "\n")
  cat("Écart-type des résidus :", sd(res), "\n\n")
  
  # Test Ljung-Box
  lb <- Box.test(res, lag = lags, type = "Ljung-Box")
  cat("Test Ljung-Box :\n")
  print(lb)
  
  # Graphiques
  par(mfrow = c(2, 2))
  
  # 1) Résidus dans le temps
  plot(res, type = "l", main = "Résidus dans le temps", ylab = "Résidus", xlab = "Index")
  abline(h = 0, col = "red")
  
  # 2) ACF des résidus
  acf(res, main = "ACF des résidus")
  
  # 3) Histogramme
  hist(res, breaks = 20, main = "Histogramme des résidus", xlab = "Résidus", col = "lightblue")
  
  # 4) Q-Q plot
  qqnorm(res)
  qqline(res, col = "red")
  
  par(mfrow = c(1, 1))
  
  invisible(res)  # renvoie les résidus si besoin
}
