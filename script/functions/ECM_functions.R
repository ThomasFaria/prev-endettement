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



data_forecast <- function(data, list_data, vars_cst, vars_inter, tx_var, date) {
 year <- floor(date)
 q = date - year
 q = as.numeric(q)
 
 # nom de la feuille à sélectionner
 if (q > 0.25) {
   sheet_name <- paste0("Prev_j_", year)
 } else {
   sheet_name <- paste0("Prev_d_", year-1)
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
       t_out <- c(t_out, current_year)
       
     } else {
     
       nbr_q <- q 
       
       t_seq <- seq(q, 1, by = 0.25)
       
       t_out <- c(t_out, current_year + t_seq)
     }
     
   } else {
     if (q == 0) {
       t_out <- c(t_out, current_year - 1 + c(0.25, 0.5, 0.75, 1))
     }
     else{
     # années suivantes complètes
     t_out <- c(t_out, current_year + c(0.25, 0.5, 0.75, 1))
     }
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
         idx <- match((date - 0.25), data$t)
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
     
     taux <- df[[v]][i]   # taux annuel en %
     facteur_annuel <- 1 + taux / 100
     taux_trim <- facteur_annuel^(1/4) - 1
     
     if (i == 1) {
       
       if (q == 0) {
         # -------------------------
         # début d'année : 1 trimestre à produire (Q4)
         # -------------------------
         idx_last <- match((date - 0.25), data$t) # Q3 année précédente
         last_val <- data[[v_base]][idx_last]
         
         out <- c(out, last_val * (1 + taux_trim))
         
       } else {
         # -------------------------
         # intra-annuel : n_remaining trimestres à produire
         # -------------------------
         n_elapsed   <- round(q / 0.25)
         n_remaining <- 5 - n_elapsed
         
         idx_last <- match((date - 0.25), data$t)
         last_val <- data[[v_base]][idx_last]
         
         path <- last_val * cumprod(rep(1 + taux_trim, n_remaining))
         out <- c(out, path)
       }
       
     } else {
       # -------------------------
       # années suivantes : 4 trimestres
       # -------------------------
       last_val <- out[length(out)]
       
       path <- last_val * cumprod(rep(1 + taux_trim, 4))
       out <- c(out, path)
     }
   }
   res[[v_base]] <- out
 }
   
 return(res)
}


ECM_expanding_test_plot <- function(y,
                                    vars,
                                    I1_vars = NULL,
                                    I0_vars = NULL,
                                    test_size  = 2,
                                    start,
                                    step = 1,
                                    salaire_adj = T,
                                    Immo_adj = T,
                                    data) {
  res_list <- list()
  ct_vars <- I1_vars
  
  data$t <- data$year + data$quarter/4
  data <- na.omit(data)
  data_2 <- data 
  
  start = max(start, 2015.5)
  start <- floor(start*2) /2
  end <- max(data$t)
  end <- floor(end*2) /2
  pred_list <- list()
  
  for (end_train in seq(start, end - test_size + 1, by = step)) {
    
    
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
    # ECM - estimation
    # -----------------------------
    formula_ecm <- as.formula(
      paste0("diff_", y, " ~ ", paste(all_vars, collapse = " + "), " + lag(ECT,1)")
    ) 
    reg_ecm <- lm(formula_ecm, data = data_diff)
    
    # -----------------------------
    # ECM - formule pour prévision
    # -----------------------------
    formula_ecm_fc <- as.formula(
      paste0("diff_", y, " ~ ", paste(all_vars, collapse = " + "), " + ECT")
    )
    reg_ecm_fc <- lm(formula_ecm_fc, data = data_diff %>% dplyr::mutate(ECT = lag(ECT, 1)))
    
    ###############
    # Forecasting
    ###############
    last_idx   <- nrow(train)
    last_train <- train[last_idx, ]
    
    y_prev   <- last_train[[y]]
    ECT_prev <- ECT[length(ECT)] 
    

    
    data_fc <- data_forecast(data_2, list_data, 
                             vars_cst = c("salaires","taux_epargne"),
                             vars_inter = c("EURIBOR","chomage", "Taux_long"), tx_var=c("PIB_variation", "FBCF_variation"),
                             end_train)
    
    data_fc <- as.data.frame(data_fc)
    data_fc <- data_fc[data_fc$t > end_train, ] 
    data_orig_t <- data_2[data_2$t <= end_train, ] 
    m_spread = mean(data_orig_t$Taux_immo - data_orig_t$Taux_long)
    data_fc$Taux_immo <- data_fc$Taux_long + m_spread
    
    if (salaire_adj == TRUE) {
      salaires3 <- na.omit(data_orig_t$salaires3)
      sal_complet <- c(salaires3, data_fc$salaires)
      sal_ma <- stats::filter(sal_complet, rep(1/4, 4), sides = 1)
      
      # --- Lag de 1 (Décalage temporel) ---
      # On décale pour que la valeur en t soit celle observée/calculée en t-1
      sal_final <- c(NA, head(sal_ma, -1))
      
      # --- 4. Rescaling (Recentrage sur l'historique uniquement) ---
      # On calcule les paramètres sur la partie historique pour ne pas "tricher"
      x_hist <- data_orig_t$salaires
      y_hist <- data_orig_t$salaires3
      
      sd_x <- sd(x_hist, na.rm = TRUE)
      sd_y <- sd(y_hist, na.rm = TRUE)
      mean_x <- mean(x_hist, na.rm = TRUE)
      mean_y <- mean(y_hist, na.rm = TRUE)
      
     
      sal_transformed <- (sal_final - mean_y) * (sd_x / sd_y) + mean_x
      n_hist <- nrow(data_orig_t)
      sal_forecast_only <- sal_final[(n_hist + 1):length(sal_final)]
      
      data_fc$salaires <- sal_forecast_only
    }
    
    
    if (Immo_adj == TRUE) {
      
      # 1. Calcul du spread et définition des régimes sur l'historique
      data_orig_t$spreads <- data_orig_t$Taux_immo - data_orig_t$Taux_long
      regimes_possibles <- c("R1_pre_crise", "R2_crise", "R3_taux_bas", "R4_remontee")
      
      data_orig_t <- data_orig_t %>%
        mutate(regime = case_when(
          t < 2008.75 ~ "R1_pre_crise",
          t < 2011.95 ~ "R2_crise",
          t < 2021.95 ~ "R3_taux_bas",
          TRUE        ~ "R4_remontee"
        )) %>%
        mutate(regime = factor(regime, levels = regimes_possibles))
      
      # Création de la matrice xreg complète
      xreg_train_full <- model.matrix(~ regime, data = data_orig_t)[, -1]
      
      # --- SÉCURITÉ OPTIM : Nettoyage des colonnes sans variation (que des 0) ---
      # On ne garde que les colonnes qui ont des valeurs dans le passé pour l'Arima
      cols_actives <- colSums(xreg_train_full != 0) > 0
      xreg_train <- xreg_train_full[, cols_actives, drop = FALSE]
      
      # 2. Préparation du futur (data_fc)
      data_fc_p <- data_fc %>%
        mutate(regime = case_when(
          t < 2008.75 ~ "R1_pre_crise",
          t < 2011.95 ~ "R2_crise",
          t < 2021.95 ~ "R3_taux_bas",
          TRUE        ~ "R4_remontee"
        )) %>%
        mutate(regime = factor(regime, levels = regimes_possibles))
      
      # Création de xreg_futur et alignement sur les colonnes de l'entraînement
      xreg_futur_full <- model.matrix(~ regime, data = data_fc_p)[, -1]
      xreg_futur <- xreg_futur_full[, colnames(xreg_train), drop = FALSE]
      
    ## 3. Estimation de l'ARIMA(1,0,1)
      mod_1 <- Arima(data_orig_t$spreads,
                     order = c(1,0,1),
                     xreg = xreg_train,
                    method = "CSS-ML") 
      
       # Estimation de l'ARIMA(1,0,1) sans reg
       # mod_1 <- Arima(data_orig_t$spreads,
       #                order = c(2,1,0))
      
      # 4. Prévision
      h <- nrow(data_fc)
      fc <- forecast(mod_1, h = h, xreg = xreg_futur)
     # fc <- forecast(mod_1, h = h)
      
      # 5. Reconstruction du Taux Immo final
      spread_forecast <- as.numeric(fc$mean)
      data_fc$Taux_immo <- data_fc$Taux_long + spread_forecast
    }
   
    n_fc <- test_size * 4
    if (nrow(data_fc) < n_fc) {
      stop(paste0("Période demandée trop longue : data_forecast ne fournit que ", 
                  nrow(data_fc), " périodes alors que n_fc = ", n_fc))
    } else {
      data_fc <- data_fc[1:n_fc, ]
    }
    
    rownames(data_fc) <- NULL
    
    data_fc$y_LT <- predict(reg_lt, newdata = data_fc)

    for (v in ct_no_lag) {
      diff_name <- paste0("diff_", v)
      vals <- c(last_train[[v]], data_fc[[v]])
      data_fc[[diff_name]] <- diff(vals)
    }
    
    # --- LAGS I1 (Variables différenciées) ---
    for (lv in lag_vars) {
      lag_val      <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv))
      original_var <- sub("^lag[0-9]+_", "", lv)
      diff_name    <- paste0("diff_", original_var)
      
      # On récupère les dernières différences réelles du train
      # (car diff_name existe dans data_diff)
      history <- tail(data_diff[[diff_name]], lag_val)
      all_vals <- c(history, data_fc[[diff_name]])
      
      # On décale et on ne garde que les n_fc premières valeurs
      # head(..., -lag_val) supprime les dernières pour garder la même longueur
      data_fc[[lv]] <- as.numeric(head(all_vals, n_fc))
    }
  
    for (lv in lag_vars2) {
      lag_val2     <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv))
      original_var <- sub("^lag[0-9]+_", "", lv)
      
      # On récupère les dernières valeurs réelles du train
      history <- tail(train[[original_var]], lag_val2)
      all_vals <- c(history, data_fc[[original_var]])
      
      # On décale et on ne garde que les n_fc premières valeurs
      data_fc[[lv]] <- as.numeric(head(all_vals, n_fc))
    }
    
    y_fc   <- rep(NA, n_fc)
    ECT_fc <- rep(NA, n_fc)
    
    for (t in 1:n_fc) {
      # Injection de l'ECT de la période précédente (t-1)
      data_fc[t, "ECT"] <- ECT_prev
      
      # Prédiction de la variation pour la période t
      dy_hat <- predict(reg_ecm_fc, newdata = data_fc[t, , drop = FALSE])
      
      # Calcul du niveau pour t
      y_fc[t] <- y_prev + dy_hat
      
      # Calcul du nouvel ECT (pour l'étape t+1)
      ECT_prev <- y_fc[t] - data_fc$y_LT[t]
      
      # Mise à jour pour l'itération suivante
      y_prev   <- y_fc[t]
      ECT_fc[t] <- ECT_prev
    }
    
    # Stockage final
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



ECM_eval_plot <- function(data, res_list, target_name = "log_end_snf", use_exp = TRUE) {
  
  # 1. Préparation du réel (appliqué conditionnellement)
  y_real <- data[[target_name]]
  if(use_exp) y_real <- exp(y_real)
  
  df_real <- data.frame(t = data$t, value = y_real)
  
  all_preds <- data.frame()
  errors_rmse <- c()
  errors_rmspe <- c()
  
  # 2. Boucle sur les fenêtres de prévision
  for (n in names(res_list)) {
    df_fc <- res_list[[n]]
    t_start_fc <- min(df_fc$t)
    
    # Récupération du point de jonction T
    last_point_train <- data[data$t < t_start_fc, ]
    last_point_train <- last_point_train[nrow(last_point_train), ]
    
    # Valeurs pour cette fenêtre
    y_fc <- df_fc[[target_name]]
    y_junction <- last_point_train[[target_name]]
    
    # Passage à l'exponentielle si demandé
    if(use_exp) {
      y_fc <- exp(y_fc)
      y_junction <- exp(y_junction)
    }
    
    # Construction du bloc de données pour ggplot
    df_plot <- data.frame(
      t = c(last_point_train$t, df_fc$t),
      value_fc = c(y_junction, y_fc),
      origin = n
    )
    
    all_preds <- rbind(all_preds, df_plot)
    
    # --- Calcul d'erreur (toujours cohérent avec use_exp) ---
    # On compare les points où le temps concorde
    real_vals <- y_real[data$t %in% df_fc$t]
    pred_vals <- y_fc[df_fc$t %in% data$t]
    
    if(length(real_vals) > 0) {
      e <- real_vals - pred_vals
      errors_rmse  <- c(errors_rmse, sqrt(mean(e^2)))
      errors_rmspe <- c(errors_rmspe, sqrt(mean((e/real_vals)^2)) * 100)
    }
  }
  
  # --- 3. Graphique ggplot ---
  p <- ggplot() +
    # Données historiques
    geom_line(data = df_real, aes(x = t, y = value), 
              color = "black", linewidth = 1.2) +
    # Segments de prévision (lignes pleines, connectées)
    geom_line(data = all_preds, aes(x = t, y = value_fc, group = origin, color = origin), 
              linewidth = 1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 7),
      legend.key.width = unit(0.4, "cm"),
      legend.key.height = unit(0.3, "cm"),
      guides(color = guide_legend(nrow = 3)),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(breaks = seq(floor(min(df_real$t)), ceiling(max(all_preds$t)), by = 1)) +
    scale_y_continuous(breaks = seq(30, 90, by = 2.5)) +
    coord_cartesian(ylim = c(30, 90)) +
    labs(
      title = paste("Backtesting ECM :", target_name, if(use_exp) "(Niveau)" else "(Log)"),
      subtitle = paste0("RMSE moyen : ", round(mean(errors_rmse), 4), 
                        " | RMSPE moyen : ", round(mean(errors_rmspe), 2), "%"),
      x = "Date", 
      y = if(use_exp) paste("exp(", target_name, ")") else target_name
    ) +
    guides(color = guide_legend(nrow = 3))
    
  
  return(p)
}



ECM_prevision <- function(y = "log_end_snf", vars, I1_vars = NULL, I0_vars = NULL, test_size, data, list_data, window, use_exp = TRUE, salaire_adj = T, Immo_adj = F) {
  
  # --- Préparation initiale ---
  ct_vars <- I1_vars 
  data_orig <- data 
  
  I1_varsB <- unique(unlist(sapply(I1_vars, clean_name)))
  I0_varsB <- unique(unlist(sapply(I0_vars, clean_name)))
  All <- unique(c(y, vars, I1_varsB, I0_varsB, "time"))
  data <- subset(data, select = All)
  data <- na.omit(data)
  

  # -----------------------------
  # 1. ÉSTIMATION LONG TERME (LT)
  # -----------------------------
  formula_lt <- as.formula(paste0(y, " ~ ", paste(vars, collapse = " + "))) 
  reg_lt <- lm(formula_lt, data = data) 
  ECT <- residuals(reg_lt) 
  
  # -----------------------------
  # 2. PRÉPARATION DATA DIFF (TRAIN)
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
  
  # --- Lags I1 ---
  lag_vars <- NULL 
  if(!is.null(ct_vars)){
    lag_vars <- ct_vars[grepl("^lag[0-9]+_", ct_vars)] 
    for(lv in lag_vars){
      lag_val <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      diff_name <- paste0("diff_", original_var) 
      if(!(diff_name %in% names(data_diff))) data_diff[[diff_name]] <- c(NA, diff(data[[original_var]])) 
      
      data_diff[[lv]] <- c(rep(NA, lag_val), head(data_diff[[diff_name]], -lag_val)) 
    } 
  } 
  
  # --- Lags I0 ---
  lag_vars2 <- NULL 
  if(!is.null(I0_vars)){
    lag_vars2 <- I0_vars[grepl("^lag[0-9]+_", I0_vars)] 
    for(lv in lag_vars2){
      lag_val2 <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv)) 
      original_var <- sub("^lag[0-9]+_", "", lv) 
      data_diff[[lv]] <- c(rep(NA, lag_val2), head(data_diff[[original_var]], -lag_val2)) 
    } 
  }
  
  # Alignement et Estimation ECM
  data_diff <- data_diff[-1, ] 
  data_diff$ECT_lag <- ECT[1:(length(ECT)-1)] # ECT en t-1
  data_diff <- na.omit(data_diff)
  
  diff_ct_vars <- if(!is.null(ct_vars)) paste0("diff_", ct_vars[!grepl("^lag", ct_vars)]) else NULL
  all_vars_ecm <- c(diff_ct_vars, lag_vars, I0_vars, lag_vars2)
  all_vars_ecm <- all_vars_ecm[all_vars_ecm != "" & !is.na(all_vars_ecm)]
  
  formula_ecm_fc <- as.formula(paste0("diff_", y, " ~ ", paste(all_vars_ecm, collapse = " + "), " + ECT"))
  reg_ecm_fc <- lm(formula_ecm_fc, data = data_diff %>% dplyr::rename(ECT = ECT_lag))
  
  ###############
  # Forecasting 
  ###############
  last_idx   <- nrow(data_orig)
  last_train <- data_orig[last_idx, ]
  
  y_prev     <- last_train[[y]]
  ECT_prev   <- ECT[length(ECT)] # Dernier ECT du train
  TIME_val   <- as.numeric(last_train[["t"]])
  # Récupération des données futures
  data_fc <- data_forecast(data_orig, list_data, 
                           c("EURIBOR", "Taux_long", "salaires", "taux_epargne"),
                           c("chomage"), c("PIB_variation", "FBCF_variation"),
                           TIME_val)
  data_fc <- as.data.frame(data_fc)
  data_fc <- data_fc[data_fc$t > TIME_val, ] 
  data_orig_t <- data_orig
  m_spread = mean(data_orig_t$Taux_immo - data_orig_t$Taux_long)
  data_fc$Taux_immo <- data_fc$Taux_long + m_spread
  
  if (salaire_adj == TRUE) {
    salaires3 <- na.omit(data_orig_t$salaires3)
    sal_complet <- c(salaires3, data_fc$salaires)
    sal_ma <- stats::filter(sal_complet, rep(1/4, 4), sides = 1)
    
    # --- Lag de 1 (Décalage temporel) ---
    # On décale pour que la valeur en t soit celle observée/calculée en t-1
    sal_final <- c(NA, head(sal_ma, -1))
    
    # --- 4. Rescaling (Recentrage sur l'historique uniquement) ---
    # On calcule les paramètres sur la partie historique pour ne pas "tricher"
    x_hist <- data_orig_t$salaires
    y_hist <- data_orig_t$salaires3
    
    sd_x <- sd(x_hist, na.rm = TRUE)
    sd_y <- sd(y_hist, na.rm = TRUE)
    mean_x <- mean(x_hist, na.rm = TRUE)
    mean_y <- mean(y_hist, na.rm = TRUE)
    
    
    sal_transformed <- (sal_final - mean_y) * (sd_x / sd_y) + mean_x
    n_hist <- nrow(data_orig_t)
    sal_forecast_only <- sal_final[(n_hist):length(sal_final)]
    
    data_fc$salaires <- sal_forecast_only
  }
  
  
  if (Immo_adj == TRUE) {
    
    # 1. Calcul du spread et définition des régimes sur l'historique
    data_orig_t$spreads <- data_orig_t$Taux_immo - data_orig_t$Taux_long
    regimes_possibles <- c("R1_pre_crise", "R2_crise", "R3_taux_bas", "R4_remontee")
    
    data_orig_t <- data_orig_t %>%
      mutate(regime = case_when(
        t < 2008.75 ~ "R1_pre_crise",
        t < 2011.95 ~ "R2_crise",
        t < 2021.95 ~ "R3_taux_bas",
        TRUE        ~ "R4_remontee"
      )) %>%
      mutate(regime = factor(regime, levels = regimes_possibles))
    
    # Création de la matrice xreg complète
    xreg_train_full <- model.matrix(~ regime, data = data_orig_t)[, -1]
    
    # --- SÉCURITÉ OPTIM : Nettoyage des colonnes sans variation (que des 0) ---
    # On ne garde que les colonnes qui ont des valeurs dans le passé pour l'Arima
    cols_actives <- colSums(xreg_train_full != 0) > 0
    xreg_train <- xreg_train_full[, cols_actives, drop = FALSE]
    
    # 2. Préparation du futur (data_fc)
    data_fc_p <- data_fc %>%
      mutate(regime = case_when(
        t < 2008.75 ~ "R1_pre_crise",
        t < 2011.95 ~ "R2_crise",
        t < 2021.95 ~ "R3_taux_bas",
        TRUE        ~ "R4_remontee"
      )) %>%
      mutate(regime = factor(regime, levels = regimes_possibles))
    
    # Création de xreg_futur et alignement sur les colonnes de l'entraînement
    xreg_futur_full <- model.matrix(~ regime, data = data_fc_p)[, -1]
    xreg_futur <- xreg_futur_full[, colnames(xreg_train), drop = FALSE]
    
    ## 3. Estimation de l'ARIMA(1,0,1)
    mod_1 <- Arima(data_orig_t$spreads,
                   order = c(1,0,1),
                   xreg = xreg_train,
                   method = "CSS-ML") 
    
    # Estimation de l'ARIMA(1,0,1) sans reg
    # mod_1 <- Arima(data_orig_t$spreads,
    #                order = c(2,1,0))
    
    # 4. Prévision
    h <- nrow(data_fc)
    fc <- forecast(mod_1, h = h, xreg = xreg_futur)
    # fc <- forecast(mod_1, h = h)
    
    # 5. Reconstruction du Taux Immo final
    spread_forecast <- as.numeric(fc$mean)
    data_fc$Taux_immo <- data_fc$Taux_long + spread_forecast
  }
  
  
  n_fc <- test_size * 4
  
  if (nrow(data_fc) < n_fc) {
    stop("Pas assez de données dans data_fc")
  }
  
  data_fc <- data.frame(data_fc[1:n_fc, ])
  rownames(data_fc) <- NULL

  
  # 1. Calcul du LT
  data_fc$y_LT <- predict(reg_lt, newdata = data_fc)
  
  # 2. Calcul des différences (doit faire 8 lignes)
  for (v in ct_no_lag) {
    diff_name <- paste0("diff_", v)
    # On concatène le dernier point réel avec les 8 points futurs = 9 valeurs
    # diff() de 9 valeurs donne exactement les 8 variations attendues
    vals <- c(last_train[[v]], data_fc[[v]])
    data_fc[[diff_name]] <- diff(vals)
  }
  
  # 3. Gestion des LAGS I1 (Doit faire 8 lignes)
  for (lv in lag_vars) {
    lag_val      <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv))
    original_var <- sub("^lag[0-9]+_", "", lv)
    diff_name    <- paste0("diff_", original_var)
    
    # On récupère le passé dans data_diff (train)
    history <- tail(data_diff[[diff_name]], lag_val)
    all_vals <- c(history, data_fc[[diff_name]])
    
    # On tronque pour garder uniquement les n_fc premières valeurs décalées
    data_fc[[lv]] <- as.numeric(head(all_vals, n_fc))
  }
  
  # 4. Gestion des LAGS I0
  for (lv in lag_vars2) {
    lag_val2     <- as.numeric(sub("^lag([0-9]+)_.*", "\\1", lv))
    original_var <- sub("^lag[0-9]+_", "", lv)
    
    history <- tail(data[[original_var]], lag_val2)
    all_vals <- c(history, data_fc[[original_var]])
    
    data_fc[[lv]] <- as.numeric(head(all_vals, n_fc))
  }
  
  # 5. Boucle récursive de 1 à 8
  y_fc   <- rep(NA, n_fc)
  ECT_fc <- rep(NA, n_fc)
  
  for (t in 1:n_fc) {
    # On injecte l'ECT calculé au tour précédent (ou celui du train pour t=1)
    data_fc[t, "ECT"] <- ECT_prev
    
    # Prédiction de la variation
    dy_hat <- predict(reg_ecm_fc, newdata = data_fc[t, , drop = FALSE])
    
    # Niveau = niveau précédent + variation
    y_fc[t] <- y_prev + dy_hat
    
    # Calcul du nouvel ECT (pour le prochain tour)
    ECT_prev <- y_fc[t] - data_fc$y_LT[t]
    
    # Update pour t+1
    y_prev   <- y_fc[t]
    ECT_fc[t] <- ECT_prev
  }
  
  data_fc[[y]]    <- y_fc
  
  df_hist <- data_orig[, c("t", y)]
  df_hist$type <- "Historique"
  
  df_prev <- data_fc[, c("t", y)]
  df_prev$type <- "Prévision"
  
  # Fusion
  df_total <- rbind(df_hist, df_prev)
  
  if(use_exp) {
    df_total[[y]] <- exp(df_total[[y]])
    data_fc$prévisions <- data_fc[[y]]
  }
  
  
  
  # 2. Création du graphique
  p <- ggplot(df_total, aes(x = t, y = .data[[y]])) +
    # Ligne Historique
    geom_line(data = df_total %>% filter(t <= TIME_val), 
              aes(color = "Historique"), size = 1) +
    # Ligne Prévision (on part de TIME pour la jonction)
    geom_line(data = df_total %>% filter(t >= TIME_val), 
              aes(color = "Prévision"), size = 1) +
    # Esthétique
    scale_color_manual(values = c("Historique" = "black", "Prévision" = "red2")) +
    geom_vline(xintercept = TIME_val, linetype = "dotted", color = "grey40") +
    labs(
      title = paste("Projection du modèle ECM :", y),
      subtitle = paste("Prévision à t =", TIME_val),
      x = "Temps",
      y = y,
      color = ""
    ) +
    theme_minimal() +
    scale_y_continuous(limits = c(50, NA)) +
    scale_x_continuous(limits = c(window, NA)) +
    theme(legend.position = "bottom")
  
 return(list(plot = p, forecast = data_fc))
}
