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

colnames(data)

data_forecast <- function(data, list_data, vars_cst, vars_inter, tx_var, date) {
 year <- floor(date)
 q = date - year
 
 # nom de la feuille à sélectionner
 if (q > 0.25) {
   sheet_name <- paste0("Prev_j_", year)
 } else {
   sheet_name <- paste0("Prev_d_", year - 1)
 }
 
 # extraction
 df <- list_data[[sheet_name]]
 n_years <- nrow(df)
 names(df)[names(df) == "salaires_brut"] <- "salaires"
 
 # -------------------------
 # 1. Construire t_out EN PREMIER
 # -------------------------
 t_out <- c()
 
 for (i in 1:n_years) {
   current_year <- year + i - 1
   
   if (i == 1) {
     if (q == 0) {
       t_out <- c(t_out, current_year + 1)
     } else {
       t_out <- c(t_out, current_year + c(0.5, 0.75, 1))
     }
   } else {
     t_out <- c(t_out, current_year + c(0.25, 0.5, 0.75, 1))
   }
 }
 
 # -------------------------
 # 2. Initialiser res avec la bonne taille
 # -------------------------
 res <- data.frame(t = t_out)
 n_rows <- length(t_out)
  

 for (v in vars_cst) {
   
   out <- c()
   
   for (i in 1:n_years) {
     
     val <- df[[v]][i]
     
     if (i == 1) {
       # première année → dépend de q
       
       if (q == 0) {
         # seulement Q4
         out <- c(out, val)
         
       } else {
         out <- c(out, rep(val, 3))
       }
       
     } else {
       # années suivantes, 4 trimestres
       out <- c(out, rep(val, 4))
     }
   }
   res[[v]] <- out
 }
 
 # -------------------------
 # Variables interpolées
 # -------------------------

 for (v in vars_inter) {
   
   out <- c()
   
   for (i in 1:n_years) {
     
     val <- df[[v]][i]   # valeur annuelle (Q4)
     
     if (i == 1) {
       
       if (q == 0) {
         # cas début d'année → seulement Q4
         out <- c(out, val)
         
       } else {
         # dernière valeur observée (t - 0.25)
         idx <- which.min(abs(data$t - (date - 0.25)))
         last_val <- data[[v]][idx]
         
         # nombre de trimestres à prévoir
         n_q <- 3
         
         # interpolation last_val → val
         path <- seq(last_val, val, length.out = n_q)
         
         out <- c(out, path)
       }
       
     } else {
       # années suivantes = interpolation complète
       
       # point de départ = dernier point déjà construit
       start_val <- out[length(out)]
       
       path <- seq(start_val, val, length.out = 5)[-1]
       
       out <- c(out, path)
     }
   }
   
   res[[v]] <- out
 }
 
 # -------------------------
 # Taux
 # -------------------------
 
 for (v in tx_var) { 
   
   out <- c()
   
  v_base <- sub("_.*", "", v)
   
   for (i in 1:n_years) {
     
     taux <- df[[v]][i]   # taux annuel
     
     if (i == 1) {
       
       if (q == 0) {
         # -------------------------
         # début d'année
         # -------------------------
         idx_ref <- which.min(abs(data$t - (year - 1)))
         ref_val <- data[[v_base]][idx_ref]
         
         Q4 <- ref_val * (1 + (taux/100))
         
         out <- c(out, Q4)
         
       } else {
         # -------------------------
         # intra-annuel
         # -------------------------
         idx_last <- which.min(abs(data$t - (date - 0.25)))
         idx_ref  <- which.min(abs(data$t - (date - 0.5)))
         
         last_val <- data[[v_base]][idx_last]
         ref_val  <- data[[v_base]][idx_ref]
         
         Q4 <- ref_val * (1 + (taux/100))
         
         # nb de trimestres restants
         n_q <- 3
         
         path <- seq(last_val, Q4, length.out = n_q)
         
         out <- c(out, path)
       }
       
     } else {
       # -------------------------
       # années suivantes
       # -------------------------
       
       ref_val <- out[length(out)]
       Q4 <- ref_val * (1 + (taux/100))
       
       path <- seq(ref_val, Q4, length.out = 5)[-1] 
       
       out <- c(out, path)
     }
   }
   
   res[[v_base]] <- out
 }
 
 
 return(res)
}



data_forecast(data, list_data, c("EURIBOR", "Taux_long", "salaires"),
                         c("chomage", "taux_epargne"), c("PIB_variation", "FBCF_variation")
                         , 2015.5)
 
ECM_expanding_test_plot(y = "log_end_snf", vars = c("FBCF", "EURIBOR", "salaires", "chomage"),
                        I1_vars = c("lag1_log_end_snf", "FBCF"),
                        I0_vars = c(), test_size  = 2,
                        step = 1,
                        data)



ECM_expanding_test_plot <- function(y,
                                    vars,
                                    I1_vars = NULL,
                                    I0_vars = NULL,
                                    test_size  = 2,
                                    step = 1,
                                    data) {
  res_list <- list()
  ct_vars <- I1_vars
  
  data$t <- data$year + data$quarter/4
  data <- na.omit(data)
  data_2 <- data 
  
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
    # Forecasting
    ###############
    
    data_fc <- data_forecast(data_2, list_data, c("EURIBOR", "Taux_long", "salaires"),
                             c("chomage", "taux_epargne"), c("PIB_variation", "FBCF_variation"),
                             end_train)
    
    
    data_fc$y_LT <- predict(reg_lt, newdata = data_fc)
    
    last_train <- tail(train, 1)
    
    for (v in ct_no_lag) {
      diff_name <- paste0("diff_", v)
      vals <- c(last_train[[v]], data_fc[[v]])
      data_fc[[diff_name]] <- diff(vals)
    }
    
    for (lv in lag_vars) {
      lag_val      <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv))
      original_var <- sub("^lag[0-9]+_", "", lv)
      diff_name    <- paste0("diff_", original_var)
      data_fc[[lv]] <- c(rep(NA, lag_val), head(data_fc[[diff_name]], -lag_val))
    }
    
    for (lv in lag_vars2) {
      lag_val2     <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv))
      original_var <- sub("^lag[0-9]+_", "", lv)
      data_fc[[lv]] <- c(rep(NA, lag_val2), head(data_fc[[original_var]], -lag_val2))
    }
    
    last_ECT <- tail(data_diff$ECT, 1)
    
    n_fc     <- test_size * 4
    y_fc     <- rep(NA, n_fc)
    ECT_fc   <- rep(NA, n_fc)
    data_fc  <- data_fc[1:n_fc, ]
    y_prev   <- tail(train[[y]], 1)
    ECT_prev <- last_ECT
    
    x <- paste0("lag1_", y)
    data_fc[[x]]  <- as.numeric(NA)
    data_fc$ECT   <- as.numeric(NA)
    
    for (t in 1:n_fc) {
      data_fc[t, x]     <- y_prev
      data_fc[t, "ECT"] <- ECT_prev
      
      dy_hat    <- predict(reg_ecm, newdata = data_fc[t, , drop = FALSE])
      y_fc[t]   <- y_prev + dy_hat
      ECT_fc[t] <- y_fc[t] - data_fc$y_LT[t]
      
      y_prev   <- y_fc[t]
      ECT_prev <- ECT_fc[t]
    }
    
    data_fc[[y]]    <- y_fc
    data_fc$y_LT_fc <- data_fc$y_LT
    data_fc$ECT     <- ECT_fc
    
    res_list[[as.character(end_train)]] <- data_fc
  }
  return(res_list)
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

