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



########################
#TEST Fonction ECM 
#######################

ECM_plot <- function(y = "endettement_menage", vars, I1_vars = NULL, I0_vars = NULL, data, plot_LT = TRUE){ 
  
  ct_vars <- I1_vars 
  
  I1_varsB <- unique(unlist(sapply(I1_vars, clean_name)))
  I0_varsB <- unique(unlist(sapply(I0_vars, clean_name)))
  All <- unique(c(y, vars, I1_varsB, I0_varsB, "time"))
  
  data <- subset(data, select = All)
  data <- na.omit(data)
  
  # -----------------------------
  # LONG TERME
  # -----------------------------
  formula_lt <- as.formula(
    paste0(y, " ~ ", paste(vars, collapse = " + "))
  ) 
  
  reg_lt <- lm(formula_lt, data = data) 
  ECT <- residuals(reg_lt) 
  
  # -----------------------------
  # DATA DIFF
  # -----------------------------
  data_diff <- data 
  
  data_diff[[paste0("diff_", y)]] <- c(NA, diff(data[[y]])) 
  
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
  # LAGS I1
  # -----------------------------
  lag_vars <- NULL 
  if(!is.null(ct_vars)){
    lag_vars <- ct_vars[grepl("^lag[0-9]+_", ct_vars)] 
    for(lv in lag_vars){
      lag_val <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      diff_name <- paste0("diff_", original_var) 
      
      if(!(diff_name %in% names(data_diff))){
        data_diff[[diff_name]] <- c(NA, diff(data[[original_var]])) 
      } 
      
      data_diff[[lv]] <- c(
        rep(NA, lag_val),
        head(data_diff[[diff_name]], -lag_val)
      ) 
    } 
  } 
  
  # -----------------------------
  # LAGS I0
  # -----------------------------
  lag_vars2 <- NULL 
  if(!is.null(I0_vars)){
    lag_vars2 <- I0_vars[grepl("^lag[0-9]+_", I0_vars)] 
    for(lv in lag_vars2){
      lag_val2 <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      
      data_diff[[lv]] <- c(
        rep(NA, lag_val2),
        head(data_diff[[original_var]], -lag_val2)
      ) 
    } 
  }
  
  # -----------------------------
  # ALIGNEMENT FINAL
  # -----------------------------
  data_diff <- data_diff[-1, ] 
  data_diff$ECT <- ECT[-1]
  data_diff <- na.omit(data_diff)
  
  # -----------------------------
  # VARIABLES ECM
  # -----------------------------
  diff_ct_vars <- NULL 
  if(!is.null(ct_vars)){
    ct_nolag <- ct_vars[!grepl("^lag", ct_vars)] 
    if(length(ct_nolag) > 0){
      diff_ct_vars <- paste0("diff_", ct_nolag) 
    } 
  } 
  
  all_vars <- c(diff_ct_vars, lag_vars, I0_vars, lag_vars2) 
  all_vars <- all_vars[!is.na(all_vars)] 
  all_vars <- all_vars[nchar(all_vars) > 0] 
  
  # -----------------------------
  # ECM
  # -----------------------------
  formula_ecm <- as.formula(
    paste0("diff_", y, " ~ ", paste(all_vars, collapse = " + "), " + lag(ECT,1)")
  ) 
  
  reg_ecm <- lm(formula_ecm, data = data_diff) 
  
  ###############
  data$y_LT <- predict(reg_lt, newdata = data)
  data_diff$dy_hat <- predict(reg_ecm, newdata = data_diff)
  y_ecm <- rep(NA, nrow(data_diff))
  y_ecm[1] <- data_diff[[y]][1]
  
  
  # reconstruction dynamique
  for(t in 2:nrow(data_diff)){
    y_ecm[t] <- y_ecm[t-1] + data_diff$dy_hat[t]
  }
  
  data_diff$y_ECM <- y_ecm
  
  # =========================
  # ALIGNEMENT TEMPOREL
  # =========================
  
  
  plot_data <- data.frame(
    time = data$time,
    y_true = data[[y]],
    y_LT = data$y_LT
  )
  
  
  plot_data_ecm <- data.frame(
    time = data_diff$time,
    y_ECM = data_diff$y_ECM
  )
  
  rmspe <- sqrt(mean(((plot_data$y_true - plot_data$y_LT)/plot_data$y_true)^2, na.rm = TRUE))
  
  # merge
  plot_data <- merge(plot_data, plot_data_ecm, by = "time", all.x = TRUE)
  
  # =========================
  # PLOT GGPLOT
  # =========================
  
  plot_data$time <- as.Date(plot_data$time)
  
  p <- ggplot(plot_data, aes(x = time)) +
    geom_line(aes(y = y_true, color = "Observé"), size = 1) +
    geom_line(aes(y = y_LT, color = "Long terme"), linetype = "dotted", size = 1) +
    geom_line(aes(y = y_ECM, color = "ECM dynamique"), linetype = "dashed", size = 1) +
    scale_color_manual(
      values = c(
        "Observé" = "gray30",        # gris foncé
        "Long terme" = "blue",       # bleu
        "ECM dynamique" = "red2"     # rouge vif
      )
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
      title = paste("Reconstruction du modèle"),
      y = y,
      x = "Temps",
      color = ""
    ) +
    theme_minimal()
  
  print(p)
  print(rmspe)
}


data_forecast <- function(data, vars_cst, vars_inter, date) {
 year <- floor(date)
 q = date - year
 
 # nom de la feuille à sélectionner
 if (q > 0) {
   sheet_name <- paste0("Prev_j_", year)
 } else {
   sheet_name <- paste0("Prev_d_", year - 1)
 }
 
 # extraction
 df <- list_data[[sheet_name]]
 
 
 
 return(df)
}
 
  
ECM_expanding_test_plot <- function(y,
                                    vars,
                                    I1_vars = NULL,
                                    I0_vars = NULL,
                                    test_size  = 2,
                                    step = 1,
                                    data) {
  I1_varsB <- unique(unlist(sapply(I1_vars, clean_name)))
  I0_varsB <- unique(unlist(sapply(I0_vars, clean_name)))
  All <- unique(c(time, y, vars, I1_varsB, I0_varsB))
  data <- subset(data, select = All)
  data <- na.omit(data)
  
  data$t <- data$year + data$quarter/4
  start = 2015.5
  end <- max(data$t)
  end <- floor(end*2) /2
  pred_list <- list()
  
  for (end_train in seq(start, end - test_size, by = step)) {
    
    train <- data[data$t <= end_train, ]
    test  <- data[data$t > end_train, ]
    
    # -----------------------------
    # LONG TERME
    # -----------------------------
    formula_lt <- as.formula(
      paste0(y, " ~ ", paste(vars, collapse = " + "))
    ) 
    
    reg_lt <- lm(formula_lt, data = train) 
    ECT <- residuals(reg_lt) 
    
    # -----------------------------
    # DATA DIFF
    # -----------------------------
    data_diff <- train
    
    data_diff[[paste0("diff_", y)]] <- c(NA, diff(train[[y]])) 
    
    if(!is.null(ct_vars)){
      ct_no_lag <- ct_vars[!grepl("^lag", ct_vars)] 
      for(v in ct_no_lag){
        diff_name <- paste0("diff_", v) 
        if(!(diff_name %in% names(data_diff))){
          data_diff[[diff_name]] <- c(NA, diff(train[[v]])) 
        } 
      } 
    } 
    
    # -----------------------------
    # LAGS I1
    # -----------------------------
    lag_vars <- NULL 
    if(!is.null(ct_vars)){
      lag_vars <- ct_vars[grepl("^lag[0-9]+_", ct_vars)] 
      for(lv in lag_vars){
        lag_val <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
        original_var <- sub("^lag[0-9]+_", "", lv) 
        diff_name <- paste0("diff_", original_var) 
        
        if(!(diff_name %in% names(data_diff))){
          data_diff[[diff_name]] <- c(NA, diff(train[[original_var]])) 
        } 
        
        data_diff[[lv]] <- c(
          rep(NA, lag_val),
          head(data_diff[[diff_name]], -lag_val)
        ) 
      } 
    } 
    
    # -----------------------------
    # LAGS I0
    # -----------------------------
    lag_vars2 <- NULL 
    if(!is.null(I0_vars)){
      lag_vars2 <- I0_vars[grepl("^lag[0-9]+_", I0_vars)] 
      for(lv in lag_vars2){
        lag_val2 <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
        original_var <- sub("^lag[0-9]+_", "", lv) 
        
        data_diff[[lv]] <- c(
          rep(NA, lag_val2),
          head(data_diff[[original_var]], -lag_val2)
        ) 
      } 
    }
    
    # -----------------------------
    # ALIGNEMENT FINAL
    # -----------------------------
    data_diff <- data_diff[-1, ] 
    data_diff$ECT <- ECT[-1]
    data_diff <- na.omit(data_diff)
    
    # -----------------------------
    # VARIABLES ECM
    # -----------------------------
    diff_ct_vars <- NULL 
    if(!is.null(ct_vars)){
      ct_nolag <- ct_vars[!grepl("^lag", ct_vars)] 
      if(length(ct_nolag) > 0){
        diff_ct_vars <- paste0("diff_", ct_nolag) 
      } 
    } 
    
    all_vars <- c(diff_ct_vars, lag_vars, I0_vars, lag_vars2) 
    all_vars <- all_vars[!is.na(all_vars)] 
    all_vars <- all_vars[nchar(all_vars) > 0] 
    
    # -----------------------------
    # ECM
    # -----------------------------
    formula_ecm <- as.formula(
      paste0("diff_", y, " ~ ", paste(all_vars, collapse = " + "), " + lag(ECT,1)")
    ) 
    
    reg_ecm <- lm(formula_ecm, data = data_diff) 
    
    ###############
    #
    ###############
    
    
    
    data$y_LT <- predict(reg_lt, newdata = data)
    data_diff$dy_hat <- predict(reg_ecm, newdata = data_diff)
    y_ecm <- rep(NA, nrow(data_diff))
    y_ecm[1] <- data_diff[[y]][1]
    
    
    # reconstruction dynamique
    for(t in 2:nrow(data_diff)){
      y_ecm[t] <- y_ecm[t-1] + data_diff$dy_hat[t]
    }
    
    data_diff$y_ECM <- y_ecm
    
    # =========================
    # ALIGNEMENT TEMPOREL
    # =========================
    
    
    plot_data <- data.frame(
      time = data$time,
      y_true = data[[y]],
      y_LT = data$y_LT
    )
    
    
    plot_data_ecm <- data.frame(
      time = data_diff$time,
      y_ECM = data_diff$y_ECM
    )
    
    rmspe <- sqrt(mean(((plot_data$y_true - plot_data$y_LT)/plot_data$y_true)^2, na.rm = TRUE))
    
    # merge
    plot_data <- merge(plot_data, plot_data_ecm, by = "time", all.x = TRUE)
    
    # =========================
    # PLOT GGPLOT
    # =========================
    
    plot_data$time <- as.Date(plot_data$time)
    
    p <- ggplot(plot_data, aes(x = time)) +
      geom_line(aes(y = y_true, color = "Observé"), size = 1) +
      geom_line(aes(y = y_LT, color = "Long terme"), linetype = "dotted", size = 1) +
      geom_line(aes(y = y_ECM, color = "ECM dynamique"), linetype = "dashed", size = 1) +
      scale_color_manual(
        values = c(
          "Observé" = "gray30",        # gris foncé
          "Long terme" = "blue",       # bleu
          "ECM dynamique" = "red2"     # rouge vif
        )
      ) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(
        title = paste("Reconstruction du modèle"),
        y = y,
        x = "Temps",
        color = ""
      ) +
      theme_minimal()
    
    print(p)
    print(rmspe)
    
 }
}


ECM_plot <- function(y = "endettement_menage", vars, I1_vars = NULL, I0_vars = NULL, data, plot_LT = TRUE){ 
  
  ct_vars <- I1_vars 
  
  I1_varsB <- unique(unlist(sapply(I1_vars, clean_name)))
  I0_varsB <- unique(unlist(sapply(I0_vars, clean_name)))
  All <- unique(c(y, vars, I1_varsB, I0_varsB, "time"))
  
  data <- subset(data, select = All)
  data <- na.omit(data)
  
  # -----------------------------
  # LONG TERME
  # -----------------------------
  formula_lt <- as.formula(
    paste0(y, " ~ ", paste(vars, collapse = " + "))
  ) 
  
  reg_lt <- lm(formula_lt, data = data) 
  ECT <- residuals(reg_lt) 
  
  # -----------------------------
  # DATA DIFF
  # -----------------------------
  data_diff <- data 
  
  data_diff[[paste0("diff_", y)]] <- c(NA, diff(data[[y]])) 
  
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
  # LAGS I1
  # -----------------------------
  lag_vars <- NULL 
  if(!is.null(ct_vars)){
    lag_vars <- ct_vars[grepl("^lag[0-9]+_", ct_vars)] 
    for(lv in lag_vars){
      lag_val <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      diff_name <- paste0("diff_", original_var) 
      
      if(!(diff_name %in% names(data_diff))){
        data_diff[[diff_name]] <- c(NA, diff(data[[original_var]])) 
      } 
      
      data_diff[[lv]] <- c(
        rep(NA, lag_val),
        head(data_diff[[diff_name]], -lag_val)
      ) 
    } 
  } 
  
  # -----------------------------
  # LAGS I0
  # -----------------------------
  lag_vars2 <- NULL 
  if(!is.null(I0_vars)){
    lag_vars2 <- I0_vars[grepl("^lag[0-9]+_", I0_vars)] 
    for(lv in lag_vars2){
      lag_val2 <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      
      data_diff[[lv]] <- c(
        rep(NA, lag_val2),
        head(data_diff[[original_var]], -lag_val2)
      ) 
    } 
  }
  
  # -----------------------------
  # ALIGNEMENT FINAL
  # -----------------------------
  data_diff <- data_diff[-1, ] 
  data_diff$ECT <- ECT[-1]
  data_diff <- na.omit(data_diff)
  
  # -----------------------------
  # VARIABLES ECM
  # -----------------------------
  diff_ct_vars <- NULL 
  if(!is.null(ct_vars)){
    ct_nolag <- ct_vars[!grepl("^lag", ct_vars)] 
    if(length(ct_nolag) > 0){
      diff_ct_vars <- paste0("diff_", ct_nolag) 
    } 
  } 
  
  all_vars <- c(diff_ct_vars, lag_vars, I0_vars, lag_vars2) 
  all_vars <- all_vars[!is.na(all_vars)] 
  all_vars <- all_vars[nchar(all_vars) > 0] 
  
  # -----------------------------
  # ECM
  # -----------------------------
  formula_ecm <- as.formula(
    paste0("diff_", y, " ~ ", paste(all_vars, collapse = " + "), " + lag(ECT,1)")
  ) 
  
  reg_ecm <- lm(formula_ecm, data = data_diff) 

###############
  data$y_LT <- predict(reg_lt, newdata = data)
  data_diff$dy_hat <- predict(reg_ecm, newdata = data_diff)
  y_ecm <- rep(NA, nrow(data_diff))
  y_ecm[1] <- data_diff[[y]][1]
  
  
  # reconstruction dynamique
  for(t in 2:nrow(data_diff)){
    y_ecm[t] <- y_ecm[t-1] + data_diff$dy_hat[t]
  }
  
  data_diff$y_ECM <- y_ecm
  
  # =========================
  # ALIGNEMENT TEMPOREL
  # =========================
  

  plot_data <- data.frame(
    time = data$time,
    y_true = data[[y]],
    y_LT = data$y_LT
  )
  
  
  plot_data_ecm <- data.frame(
    time = data_diff$time,
    y_ECM = data_diff$y_ECM
  )
  
  rmspe <- sqrt(mean(((plot_data$y_true - plot_data$y_LT)/plot_data$y_true)^2, na.rm = TRUE))
  
  # merge
  plot_data <- merge(plot_data, plot_data_ecm, by = "time", all.x = TRUE)
  
  # =========================
  # PLOT GGPLOT
  # =========================
  
  plot_data$time <- as.Date(plot_data$time)
  
  p <- ggplot(plot_data, aes(x = time)) +
    geom_line(aes(y = y_true, color = "Observé"), size = 1) +
    geom_line(aes(y = y_LT, color = "Long terme"), linetype = "dotted", size = 1) +
    geom_line(aes(y = y_ECM, color = "ECM dynamique"), linetype = "dashed", size = 1) +
    scale_color_manual(
      values = c(
        "Observé" = "gray30",        # gris foncé
        "Long terme" = "blue",       # bleu
        "ECM dynamique" = "red2"     # rouge vif
      )
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
      title = paste("Reconstruction du modèle"),
      y = y,
      x = "Temps",
      color = ""
    ) +
    theme_minimal()
  
  print(p)
  print(rmspe)
}

