

test_cointegration <- function(data, y = "endettement_menage", vars){
  
  results <- data.frame()
  
  for (k in 2:5) {
    
    combs <- combn(vars, k, simplify = FALSE)
    
    for (c in combs) {
      
      x <- c
      
      formula <- as.formula(
        paste(y, "~", paste(x, collapse = " + "))
      )
      
      reg <- lm(formula, data = data)
      
      r2 <- summary(reg)$r.squared
      
      # Test ADF sur les résidus (cointégration)
      adf <- adf.test(residuals(reg))
      
      # Test autocorrélation
      bg <- bgtest(reg, order = 4)
      
      # Test hétéroscédasticité
      bp <- bptest(reg)
      
      
      results <- rbind(
        results,
        data.frame(
          y = y,
          x = paste(x, collapse = ", "),
          k = k,
          R2 = r2,
          adf_pvalue = adf$p.value,
          bg_pvalue = bg$p.value,
          bp_pvalue = bp$p.value
        )
      )
      
      ligne <- NA
      
      # récupérer coef et pvalue
      
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
      
      results <- rbind(results, ligne)
    }
  }
 
  results <- results[order(results$adf_pvalue), ]
  return(results)
}



ECM_compute <- function(y = "endettement_menage", vars, I1_vars = NULL, I0_vars = NULL, data){ 
  ct_vars <- I1_vars 
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
  
  # différences des variables LT
  for(v in vars){
    diff_name <- paste0("diff_", v) 
    if(!(diff_name %in% names(data_diff))){
      data_diff[[diff_name]] <- c(NA, diff(data[[v]])) 
    } 
  } 
  
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
  diff_vars <- paste0("diff_", vars) 
  diff_ct_vars <- NULL 
  if(!is.null(ct_vars)){
    ct_nolag <- ct_vars[!grepl("^lag", ct_vars)] 
    if(length(ct_nolag) > 0){
      diff_ct_vars <- paste0("diff_", ct_nolag) 
    } 
  } 
  
  
  all_vars <- c(diff_vars, diff_ct_vars, lag_vars, I0_vars, lag_vars2) 
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
      bg <- bgtest(reg_ecm, order = 4)
      bp <- bptest(reg_ecm)
      
      
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
        
        ligne[[paste0("coef_", v)]] <- coef_var
        ligne[[paste0("pval_", v)]] <- pval_var
      }
      
      models_valides <- rbind(models_valides, ligne)
      
    }
    
  }
  
  return(models_valides)
  
}


############################################
