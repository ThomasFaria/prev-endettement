test_cointegration <- function(data, y = "endettement_menage", vars){
  
  results <- data.frame()
  
  for (k in 2:5) {
    
    combs <- combn(vars, k, simplify = FALSE)
    
    for (c in combs) {
      
      x <- c
      
      data_sub <- data[, c(y, x), drop = FALSE]
      data <- na.omit(data)
      
      
      formula <- as.formula(
        paste(y, "~", paste(x, collapse = " + "))
      )
      
      reg <- lm(formula, data = data)
      
      r2 <- summary(reg)$r.squared
      
      # Test ADF sur les résidus (cointégration)
      adf <- adf.test(residuals(reg))
      
      # Test autocorrélation
      bg <- lmtest::bgtest(reg, order = 4)
      
      # Test hétéroscédasticité
      bp <- lmtest::bptest(reg)
      
      # Création de la ligne complète
      ligne <- data.frame(
        y = y,
        x = paste(x, collapse = ", "),
        k = k,
        R2 = r2,
        adf_pvalue = adf$p.value,
        bg_pvalue = bg$p.value,
        bp_pvalue = bp$p.value,
        stringsAsFactors = FALSE
      )
      
      # récupérer coef et pvalue pour chaque variable possible
      for (v in vars) {
        
        if (v %in% rownames(summary(reg)$coefficients)) {
          
          coef_var <- coef(reg)[v]
          pval_var <- summary(reg)$coefficients[v, "Pr(>|t|)"]
          
        } else {
          
          coef_var <- NA
          pval_var <- NA
          
        }
        
        ligne[[paste0("coef_", v)]] <- coef_var
        ligne[[paste0("pval_", v)]] <- pval_var
      }
      
      # ajouter la ligne au tableau
      results <- rbind(results, ligne)
      
    }
  }
  
  results <- results[order(results$adf_pvalue), ]
  
  return(results)
}

clean_name <- function(x) {
  x <- gsub("^lag[0-9]+_", "", x)
  return(x)
}

ECM_compute <- function(y = "endettement_menage", vars, I1_vars = NULL, I0_vars = NULL, data){ 
  ct_vars <- I1_vars 
  
  I1_varsB <- unique(unlist(sapply(I1_vars, clean_name)))
  I0_varsB <- unique(unlist(sapply(I0_vars, clean_name)))
  All <- unique(c(y, vars, I1_varsB, I0_varsB))
  data <- subset(data, select = All)
  data <- na.omit(data)
  
  
  # -----------------------------
  # Régression long terme
  # -----------------------------
  formula_lt <- as.formula(
    paste0("", y, " ~ ", paste(paste0("", vars, ""), collapse = " + "))
  ) 
  reg_lt <- lm(formula_lt, data = data) 
  ECT <- residuals(reg_lt) 
  
  # -----------------------------
  # Dataset différencié
  # -----------------------------
  data_diff <- data 
  
  # différence de Y
  data_diff[[paste0("diff_", y)]] <- c(NA, diff(data[[y]])) 
  
  # différences des variables CT (hors lag)
  if(!is.null(ct_vars)){
    ct_no_lag <- ct_vars[!grepl("^lag", ct_vars)] 
    for(v in ct_no_lag){
      diff_name <- paste0("diff_", v) 
      if(!(diff_name %in% names(data_diff))){
        data_diff[[diff_name]] <- c(NA, diff(data[[v]])) 
      } 
    } 
  } 
  
  # -----------------------------
  # Création des lags
  # -----------------------------
  lag_vars <- NULL 
  if(!is.null(ct_vars)){
    lag_vars <- ct_vars[grepl("^lag[0-9]+_", ct_vars)] 
    for(lv in lag_vars){
      lag_val <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      diff_name <- paste0("diff_", original_var) 
      
      # créer diff si elle n'existe pas
      if(!(diff_name %in% names(data_diff))){
        data_diff[[diff_name]] <- c(NA, diff(data[[original_var]])) 
      } 
      
      # créer la colonne lag
      data_diff[[lv]] <- c(
        rep(NA, lag_val),
        head(data_diff[[diff_name]], -lag_val)
      ) 
    } 
  } 
  
  lag_vars2 <- NULL 
  if(!is.null(I0_vars)){
    lag_vars2 <- I0_vars[grepl("^lag[0-9]+_", I0_vars)] 
    for(lv in lag_vars2){
      lag_val2 <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      
      # créer la colonne lag directement
      data_diff[[lv]] <- c(
        rep(NA, lag_val2),
        head(data_diff[[original_var]], -lag_val2)
      ) 
    } 
  }
  
  # -----------------------------
  # Alignement
  # -----------------------------
  data_diff <- data_diff[-1, ] 
  data_diff$ECT <- ECT[-1] 
  
  # -----------------------------
  # Variables ECM (inchangées)
  # -----------------------------
  diff_ct_vars <- NULL 
  if(!is.null(ct_vars)){
    ct_nolag <- ct_vars[!grepl("^lag", ct_vars)] 
    if(length(ct_nolag) > 0){
      diff_ct_vars <- paste0("diff_", ct_nolag) 
    } 
  } 
  
  
  all_vars <- c( diff_ct_vars, lag_vars, I0_vars, lag_vars2) 
  all_vars <- all_vars[!is.na(all_vars)] 
  all_vars <- all_vars[nchar(all_vars) > 0] 
  
  # -----------------------------
  # Formule ECM (inchangée)
  # -----------------------------
  formula_ecm <- as.formula(
    paste0(
      "diff_", y, " ~ ", 
      paste(all_vars, collapse = " + "), 
      " + lag(ECT,1)"
    ) 
  ) 
  
  # -----------------------------
  # Estimation
  # -----------------------------
  reg_ecm <- lm(formula_ecm, data = data_diff) 
  return(list( long_term = reg_lt, ECM = reg_ecm )) 
}




######################################
# ESSAIE D'UNE FONCTION PEU PERTINANTE 
######################################

test_ECM <- function(y = "endettement_menage", data, results, seuil_pval = 0.1) {
  
  models_valides <- data.frame()
  all_vars <- unique(trimws(unlist(strsplit(results$x, ","))))
  
  data <- subset(data, select = c(all_vars,y)) 
  data <- na.omit(data)
  
  for (i in 1:nrow(results)) {
    
    vars_raw <- as.character(results$x[i])
    vars_raw <- trimws(vars_raw)
    
    vars <- trimws(unlist(strsplit(vars_raw, ",")))
    
    if (length(vars) == 0 || all(vars == "")) {
      warning(paste("La ligne", i, "n’a pas de variables valides, elle est ignorée"))
      next
    }
    
    # -----------------------------
    # Modèle long terme
    # -----------------------------
    
    formula_lt <- as.formula(
      paste0("`", y, "` ~ ", paste(paste0("`", vars, "`"), collapse = " + "))
    )
    
    reg_lt <- lm(formula_lt, data = data)
    
    ECT <- residuals(reg_lt)
    
    # -----------------------------
    # différences
    # -----------------------------
    
    data_diff <- data
    
    for (v in vars) {
      data_diff[[paste0("diff_", v)]] <- c(NA, diff(data_diff[[v]]))
    }
    
    data_diff[[paste0("diff_", y)]] <- c(NA, diff(data_diff[[y]]))
    
    data_diff <- data_diff[-1, ]
    data_diff$ECT <- ECT[-1]
    
    # -----------------------------
    # ECM
    # -----------------------------
    
    formula_ecm <- as.formula(
      paste0(
        "`diff_", y, "` ~ ",
        paste0("diff_", vars, collapse = " + "),
        " + lag(ECT,1)"
      )
    )
    
    reg_ecm <- lm(formula_ecm, data = data_diff)
    
    # Filtre ECT
    
    lambda_ECT <- coef(reg_ecm)["lag(ECT, 1)"]
    pval_ECT <- summary(reg_ecm)$coefficients["lag(ECT, 1)", "Pr(>|t|)"]
    
    if (!is.na(lambda_ECT) && lambda_ECT < 0 && pval_ECT < seuil_pval) {
      
      aic <- AIC(reg_ecm)
      bic <- BIC(reg_ecm)
      adf <- adf.test(residuals(reg_ecm))
      bg <- lmtest::bgtest(reg_ecm, order = 4)
      bp <- lmtest::bptest(reg_ecm)
      
      
      ligne <- cbind(
        results[i, ],
        lambda_ECT = lambda_ECT,
        pval_ECT = pval_ECT,
        AIC = aic,
        BIC = bic,
        adf_pvalue_ecm = adf$p.value,
        bg_pvalue_ecm = bg$p.value,
        bp_pvalue_ecm = bp$p.value 
      )
      
      # récupérer coef et pvalue
      
      for (v in all_vars) {
        
        var_name <- paste0("diff_", v)
        
        if (var_name %in% rownames(summary(reg_ecm)$coefficients)) {
          
          coef_var <- coef(reg_ecm)[var_name]
          pval_var <- summary(reg_ecm)$coefficients[var_name, "Pr(>|t|)"]
          
        } else {
          
          coef_var <- NA
          pval_var <- NA
          
        }
        
        ligne[[paste0("coef_", v, "_ecm")]] <- coef_var
        ligne[[paste0("pval_", v, "_ecm")]] <- pval_var
      }
      
      models_valides <- rbind(models_valides, ligne)
      
    }
    
  }
  
  return(models_valides)
  
}


############################################


Lasso <- function(y = "endettement_menage", vars = I1_vars_menage, data, use_1se = FALSE){
  
  data <- subset(data, select = c(vars, y))
  data <- na.omit(data)
  x <- as.matrix(data[, vars])
  y <- data[[y]]
  
  lasso_model <- glmnet(x, y, alpha = 1)
  cv_model <- cv.glmnet(x, y, alpha = 1)
  
  lambda_final <- if(use_1se) cv_model$lambda.1se else cv_model$lambda.min
  final_model <- glmnet(x, y, alpha = 1, lambda = lambda_final)
  coefs <- as.matrix(coef(final_model))
  
  return(list(model = final_model, cv = cv_model,  coefs = coefs))
}



########################
#TEST Fonction ECM 
#######################

ECM_expanding_test_plot <- function(y,
                                    vars,
                                    I1_vars = NULL,
                                    I0_vars = NULL,
                                    train_size = 60,
                                    test_size  = 8,
                                    step = 4,
                                    data) {
  
  I1_varsB <- unique(unlist(sapply(I1_vars, clean_name)))
  I0_varsB <- unique(unlist(sapply(I0_vars, clean_name)))
  All <- unique(c(time, y, vars, I1_varsB, I0_varsB))
  data <- subset(data, select = All)
  data <- na.omit(data)
  n <- nrow(data)
  
  df_orig <- data.frame(
    time  = data$time,
    value = data[[y]]
  )
  
  pred_list <- list()
  
  for (end_train in seq(train_size, n - test_size, by = step)) {
    
    train <- data[1:end_train, ]
    test  <- data[(end_train + 1):(end_train + test_size), ]
    
    # --- Estimation ECM ---
    model <- tryCatch({
      ECM_compute(y = y, vars = vars,
                  I1_vars = I1_vars,
                  I0_vars = I0_vars,
                  data = train)
    }, error = function(e) NULL)
    
    if (is.null(model)) next
    
    reg_ecm <- model$ECM
    reg_lt  <- model$long_term
    
    # -----------------------------
    # SIMULATION DYNAMIQUE
    # -----------------------------
    
    y_hat <- numeric(test_size)
    
    # dernière valeur observée
    y_last <- train[[y]][end_train]
    
    # copie des données pour mise à jour dynamique
    sim_data <- rbind(train, test)
    
    for (h in 1:test_size) {
      
      t <- end_train + h
      
      row <- sim_data[t, ]
      
      # -------------------------
      # 1. recalcul ECT dynamique
      # -------------------------
      lt_pred <- predict(reg_lt, newdata = row)
      ect <- y_last - lt_pred
      
      # -------------------------
      # 2. construire variables ECM
      # -------------------------
      
      # diff Y
      dy_lag <- if (t > 1) {
        sim_data[[y]][t-1] - sim_data[[y]][t-2]
      } else NA
      
      # dataset temporaire
      temp <- row
      
      # diff variables I(1)
      if (!is.null(I1_vars)) {
        for (v in I1_vars) {
          temp[[paste0("diff_", v)]] <- sim_data[[v]][t] - sim_data[[v]][t-1]
        }
      }
      
      # ajouter ECT lag
      temp$ECT <- ect
      
      # -------------------------
      # 3. prédiction Δy
      # -------------------------
      dy_hat <- predict(reg_ecm, newdata = temp)
      
      # -------------------------
      # 4. reconstruction niveau
      # -------------------------
      y_new <- y_last + dy_hat
      
      y_hat[h] <- y_new
      
      # update dynamique
      sim_data[[y]][t] <- y_new
      y_last <- y_new
    }
    
    # -----------------------------
    # Série complète
    # -----------------------------
    time_pred <- data$time[1:(end_train + test_size)]
    mean_pred <- c(train[[y]], y_hat)
    
    pred_list[[length(pred_list) + 1]] <- data.frame(
      time  = time_pred,
      mean  = mean_pred,
      window = paste0("Train jusqu'à ", format(data$time[end_train], "%Y"))
    )
  }
  
  pred_all <- do.call(rbind, pred_list)
  pred_all$time <- as.Date(pred_all$time)
  
  # -----------------------------
  # Plot
  # -----------------------------
  ggplot() +
    geom_line(
      data = pred_all,
      aes(x = time, y = mean, color = window, group = window),
      linewidth = 1
    ) +
    geom_line(
      data = df_orig,
      aes(x = time, y = value),
      color = "black",
      linewidth = 1
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(
      title = paste("ECM expanding window – test =", test_size),
      x = "Date",
      y = y
    )
}



