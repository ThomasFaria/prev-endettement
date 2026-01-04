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
      Alternative = ifelse("alternative" %in% names(test_result), as.character(test_result$alternative), NA),
      stringsAsFactors = FALSE
    )
    
    rownames(df) <- rowname
    return(df)
  }
  
  results_table <- data.frame()
  
  for (var in vars) {
    var_sym <- ensym(var)
    for (country in unique(data$Pays)) {
      series <- data %>% filter(Pays == country) %>% pull(!!var_sym)
      
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
    select(Pays, Statistique, P_Value, Stationnarite, Lag, Alternative)
  
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





rolling_arima_errors <- function(
    data,
    var_name = "var",
    p_max = 3,
    q_max = 3,
    d = 1,
    train_size = 60,
    test_size = 12,
    country = "country",
    step = 4,
    covid_windows = c("window_17", "window_21", "window_25")
) {
  
  data <- data[data$Pays == country, ]
  z <- data[[var_name]]
  
  grid <- expand.grid(p = 0:p_max, d = d, q = 0:q_max)
  models <- lapply(1:nrow(grid), function(i) as.numeric(grid[i, ]))
  model_names <- sapply(
    models,
    function(m) paste0("ARIMA(", m[1], ",", m[2], ",", m[3], ")")
  )
  
  errors  <- data.frame(matrix(nrow = length(models), ncol = 0))
  merrors <- data.frame(matrix(nrow = length(models), ncol = 0))
  
  rownames(errors)  <- model_names
  rownames(merrors) <- model_names
  
  for (start in seq(1, length(z) - train_size - test_size, by = step)) {
    
    train <- z[start:(start + train_size - 1)]
    test  <- z[(start + train_size):(start + train_size + test_size - 1)]
    start2 = train_size + start
    
    errors[[paste0("window_", start2)]]  <- NA
    merrors[[paste0("window_", start2)]] <- NA
    
    for (i in seq_along(models)) {
      m <- models[[i]]
      
      fit <- tryCatch({
        Arima(train, order = c(m[1], m[2], m[3]), method = "ML")
      }, error = function(e) NULL)
      
      if (!is.null(fit)) {
        pred <- forecast(fit, h = length(test))$mean
        
        errors[i, ncol(errors)] <-
          sqrt(mean((test - pred)^2))
        
        merrors[i, ncol(merrors)] <-
          sqrt(mean(((test - pred) / test)^2)) * 100
      }
    }
  }
  
  summary_table <- data.frame(
    Model = rownames(errors),
    Mean_RMSE = rowMeans(errors, na.rm = TRUE),
    Mean_RMSE_HC = rowMeans(
      errors[, !colnames(errors) %in% covid_windows, drop = FALSE],
      na.rm = TRUE
    ),
    Mean_RMSPE = rowMeans(merrors, na.rm = TRUE),
    Var_RMSE = apply(errors, 1, var, na.rm = TRUE)
  )
  
  summary_table$Var_RMSE_HC <- apply(
    errors[, !colnames(errors) %in% covid_windows, drop = FALSE],
    1, var, na.rm = TRUE
  )
  
  summary_table <- summary_table[order(summary_table$Mean_RMSE_HC), ]
  
  return(list(
    errors = errors,
    merrors = merrors,
    summary_table = summary_table
  ))
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
  
  # Différenciation explicite
  Y <- Z
  if (d >= 1) Y <- diff(Y, differences = d)
  
  n <- length(Y)
  
  # Grille de modèles
  grid <- expand.grid(p = 0:p_max, q = 0:q_max)
  models <- lapply(1:nrow(grid), function(i) c(grid$p[i], grid$q[i]))
  model_names <- sapply(models, function(m) paste0("ARIMA(", m[1], ",", d, ",", m[2], ")"))
  
  # Matrices d’erreurs
  errors  <- data.frame(matrix(nrow = length(models), ncol = 0))
  merrors <- data.frame(matrix(nrow = length(models), ncol = 0))
  rownames(errors)  <- model_names
  rownames(merrors) <- model_names
  
  # -----------------------
  # Rolling windows
  # -----------------------
  for (start in seq(1, n - train_size - test_size, by = step)) {
    
    train <- Y[start:(start + train_size - 1)]
    test  <- Y[(start + train_size):(start + train_size + test_size - 1)]
    
    last_val <- Z[start + train_size - 1]
    test_original <- Z[(start + train_size):(start + train_size + test_size - 1)]
    
    win_name <- paste0("window_", start + train_size)
    errors[[win_name]]  <- NA
    merrors[[win_name]] <- NA
    
    # -----------------------
    # XREG COVID si demandé
    # -----------------------
    if (covid) {
      train_idx <- start:(start + train_size - 1)
      test_idx  <- (start + train_size):(start + train_size + test_size - 1)
      
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
          Arima(train, order = c(p, 0, q), xreg = train_xreg)
        } else {
          Arima(train, order = c(p, 0, q))
        }
      }, error = function(e) NULL)
      
      if (!is.null(fit)) {
        pred <- if (covid) {
          forecast(fit, xreg = test_xreg, h = length(test))$mean
        } else {
          forecast(fit, h = length(test))$mean
        }
        
        # Reconstruction série originale
        pred_mean <- last_val + cumsum(pred)
        
        errors[i, ncol(errors)] <-
          sqrt(mean((test_original - pred_mean)^2))
        
        merrors[i, ncol(merrors)] <-
          sqrt(mean(((test_original - pred_mean) / test_original)^2)) * 100
      }
    }
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
  }
  
  summary_table <- summary_table[order(summary_table$Mean_RMSE), ]
  
  return(list(
    errors = errors,
    merrors = merrors,
    summary_table = summary_table
  ))
}

compare_arima_errors <- function(errors_no_covid, errors_covid, covid_start_window = 77) {
  
  # Modèles communs
  common_models <- intersect(
    rownames(errors_no_covid),
    rownames(errors_covid)
  )
  
  errors_no_covid <- errors_no_covid[common_models, , drop = FALSE]
  errors_covid    <- errors_covid[common_models, , drop = FALSE]
  
  # Fenêtres communes
  common_cols <- intersect(
    colnames(errors_no_covid),
    colnames(errors_covid)
  )
  
  # Extraire le numéro de fenêtre
  window_numbers <- as.numeric(sub("window_", "", common_cols))
  
  # Garder UNIQUEMENT les fenêtres >= covid_start_window
  keep_cols <- common_cols[window_numbers >= covid_start_window]
  
  errors_no_covid <- errors_no_covid[, keep_cols, drop = FALSE]
  errors_covid    <- errors_covid[, keep_cols, drop = FALSE]
  
  # Table finale
  comparison_table <- cbind(
    errors_no_covid,
    setNames(errors_covid, paste0("COVID_", keep_cols))
  )
  
  return(comparison_table)
}



##### ACF et PACF #####
plot_acf_pacf <- function(
    country = "France",
    data = data,
    var_name = "var",
    nom = "nom",
    ylim = c(-0.8, 0.8)
) {
  
  data <- data[data$Pays == country, ]
  serie <- data[[var_name]]
  
  p_acf <- ggAcf(serie) +
    ggtitle(paste("ACF -", nom)) +
    ylim(ylim[1], ylim[2])
  
  p_pacf <- ggPacf(serie) +
    ggtitle(paste("PACF -", nom)) +
    ylim(ylim[1], ylim[2])
  
  gridExtra::grid.arrange(p_acf, p_pacf, ncol = 2)
}

####### Rowling #######
arima_rolling_plot <- function(data, var_name = "values_snf", country = "France",
                               train_size = 60, test_size = 8, step = 4,
                               p = 1, q = 2, d = 1) {
  
  # Filtrer le pays
  data <- data[data$Pays == country, ]
  time_vals <- data$time
  
  # Série originale
  Z <- data[[var_name]]
  
  # Différenciation si nécessaire
  Y <- Z
  if(d >= 1) Y <- diff(Z, differences = d)
  
  n <- length(Y)
  pred_list <- list()
  
  for(start in seq(1, n - train_size - test_size, by = step)) {
    
    # Valeur précédente connue pour reconstruire la série originale
    prev_val <- Z[start + train_size - 1]
    
    # Train sur la série différenciée
    train <- Y[start:(start + train_size - 1)]
    
    # Ajustement ARIMA
    fit <- Arima(train, order = c(p, 0, q), method = "ML")
    fc <- forecast(fit, h = test_size)
    
    # Reconstruction des valeurs originales
    pred_diff <- fc$mean
    if(d == 0) {
      pred_mean <- c(prev_val, pred_diff)
    } else if(d == 1) {
      pred_mean <- c(prev_val, prev_val + cumsum(pred_diff))
    } else if(d == 2) {
      # cumul double
      pred_mean <- c(prev_val, prev_val + cumsum(cumsum(pred_diff)))
    }
    
    # Intervalle de confiance approximatif
    fc_se <- (fc$upper[,2] - fc$mean)/1.96
    pred_var <- if(d == 0) fc_se^2 else cumsum(fc_se^2)
    upper <- pred_mean + 1.96 * sqrt(pred_var)
    lower <- pred_mean - 1.96 * sqrt(pred_var)
    
    # Indices de temps pour le plot
    time_pred <- time_vals[(start + train_size - 1):(start + train_size + test_size - 1)]
    
    # Stockage des résultats
    pred_list[[paste0("window_", start)]] <- data.frame(
      time = time_pred,
      value = pred_mean,
      upper = upper,
      lower = lower,
      window = paste0("window_", start)
    )
  }
  
  # Fusion de toutes les fenêtres
  pred_all <- do.call(rbind, pred_list)
  
  # Série originale en data frame pour ggplot
  df_orig <- data.frame(time = time_vals, value = Z)
  
  # Graphique
  ggplot() +
    geom_line(data = df_orig, aes(x = time, y = value), color = "black") +
    geom_line(data = pred_all, aes(x = time, y = value, color = window)) +
    geom_ribbon(data = pred_all, aes(x = time, ymin = lower, ymax = upper, fill = window), alpha = 0.2) +
    theme_minimal() +
    labs(x = "Date", y = var_name, color = "Fenêtre", fill = "Fenêtre") +
    theme(legend.position = "none")
  
}


#######################
arima_forecast_plot <- function(
    data,
    var_name,
    country,
    pred = 12,
    p = 1, d = 1, q = 2,
    covid = c("none", "covid", "both"),
    covid_start = 80,
    covid_end   = 92,
    start_plot = as.Date("2014-01-01"),
    freq_months = 3,
    y_min = NULL
) {
  
  covid <- match.arg(covid)
  
  # =========================
  # Filtrer le pays
  # =========================
  data <- data[data$Pays == country, ]
  data <- data[order(data$time), ]
  
  Z <- data[[var_name]]
  time <- data$time
  n <- length(Z)
  
  # =========================
  # Différenciation selon d
  # =========================
  Y <- Z
  if (d >= 1) {
    Y <- diff(Z, differences = d)
  }
  
  # Dernières valeurs pour reconstruction
  last_levels <- tail(Z, d)
  last_date <- tail(time, 1)
  
  # =========================
  # Fonction interne forecast
  # =========================
  compute_forecast <- function(with_covid = FALSE) {
    
    if (with_covid) {
      xreg <- rep(0, length(Y))
      xreg[covid_start:covid_end] <- 1
      xreg <- matrix(xreg, ncol = 1)
      
      xreg_future <- matrix(0, nrow = pred, ncol = 1)
      
      fit <- Arima(Y, order = c(p, 0, q), xreg = xreg)
      fc  <- forecast(fit, h = pred, xreg = xreg_future)
      
    } else {
      fit <- Arima(Y, order = c(p, 0, q))
      fc  <- forecast(fit, h = pred)
    }
    
    # =========================
    # Reconstruction du niveau
    # =========================
    mean_diff <- fc$mean
    
    if (d == 0) {
      pred_mean <- c(tail(Z, 1), mean_diff)
    } else {
      pred_mean <- c(tail(Z, 1),
                     tail(Z, 1) + cumsum(mean_diff))
    }
    
    # Variance cumulée
    fc_se   <- (fc$upper[,2] - fc$mean) / 1.96
    pred_var <- c(0, cumsum(fc_se^2))
    
    upper <- pred_mean + 1.96 * sqrt(pred_var)
    lower <- pred_mean - 1.96 * sqrt(pred_var)
    
    # Dates futures
    time_pred <- seq.Date(
      from = last_date + months(freq_months),
      by   = paste(freq_months, "months"),
      length.out = length(mean_diff)
    )
    time_pred <- c(last_date, time_pred)
    
    data.frame(
      time  = time_pred,
      value = pred_mean,
      upper = upper,
      lower = lower
    )
  }
  
  # =========================
  # Prévisions
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
              color = "black") +
    theme_minimal() +
    scale_x_date(limits = c(start_plot, NA)) +
    labs(
      x = "Date",
      y = var_name,
      title = paste(country, "-", var_name)
    )
  
  if (!is.null(y_min)) {
    p_plot <- p_plot +
      scale_y_continuous(limits = c(y_min, NA))
  }
  
  if (!is.null(prev_nocovid)) {
    p_plot <- p_plot +
      geom_line(aes(x = time, y = value),
                data = prev_nocovid,
                color = "red") +
      geom_ribbon(aes(x = time, ymin = lower, ymax = upper),
                  data = prev_nocovid,
                  fill = "red", alpha = 0.2)
  }
  
  if (!is.null(prev_covid)) {
    p_plot <- p_plot +
      geom_line(aes(x = time, y = value),
                data = prev_covid,
                color = "blue") +
      geom_ribbon(aes(x = time, ymin = lower, ymax = upper),
                  data = prev_covid,
                  fill = "blue", alpha = 0.2)
  }
  
  return(p_plot)
}



check_residuals_manual <- function(data, country = "France", var_name = "var", p = 1, d = 1, q = 1, lags = 20) {
 
  data = data[data$Pays == country, ]
  y <- data[[var_name]]
  fit <- Arima(y, order = c(p,d,q), method = "ML")
  

  res <- residuals(fit)
  
  cat("Moyenne des résidus :", mean(res), "\n")
  cat("Écart-type des résidus :", sd(res), "\n\n")
  
  # Test Ljung-Box
  lb <- Box.test(res, lag = lags, type = "Ljung-Box")
  print(lb)
  
  #  Graphiques
  par(mfrow=c(2,2))  # 2x2
  
  # a) Résidus dans le temps
  plot(res, type="l", main="Résidus dans le temps", ylab="Résidus", xlab="Index")
  abline(h=0, col="red")
  
  # b) ACF des résidus
  acf(res, main="ACF des résidus")
  
  # c) Histogramme des résidus
  hist(res, breaks=20, main="Histogramme des résidus", xlab="Résidus", col="lightblue")
  
  # d) Q-Q plot
  qqnorm(res)
  qqline(res, col="red")
  
  par(mfrow=c(1,1))
  
  return(invisible(res))  # renvoie les résidus si besoin
}
