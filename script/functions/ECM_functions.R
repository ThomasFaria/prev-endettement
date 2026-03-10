

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
    }
  }
  
  results <- results[order(results$adf_pvalue), ]
  return(results)
}


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
        " + ECT"
      )
    )
    
    reg_ecm <- lm(formula_ecm, data = data_diff)
    
    # Filtre ECT
    
    lambda_ECT <- coef(reg_ecm)["ECT"]
    pval_ECT <- summary(reg_ecm)$coefficients["ECT", "Pr(>|t|)"]
    
    if (!is.na(lambda_ECT) && lambda_ECT < 0 && pval_ECT < seuil_pval) {
      
      aic <- AIC(reg_ecm)
      bic <- BIC(reg_ecm)
      
      ligne <- cbind(
        results[i, ],
        lambda_ECT = lambda_ECT,
        pval_ECT = pval_ECT,
        AIC = aic,
        BIC = bic
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







ECM_compute <- function(y = "endettement_menage", vars, data){
  
  formula_lt <- as.formula(
    paste0("`", y, "` ~ ", paste(paste0("`", vars, "`"), collapse = " + "))
  )
  
  reg_lt <- lm(formula_lt, data = data)
  
  # calcul du ECT
  reg_lt <- lm(formula_lt, data = data)
  ECT <- residuals(reg_lt)
  
  # création des différences
  data_diff <- data
  for (v in vars) {
    data_diff[[paste0("diff_", v)]] <- c(NA, diff(data_diff[[v]]))
  }
  data_diff[[paste0("diff_", y)]] <- c(NA, diff(data_diff[[y]]))
  
  # aligner ECT avec les différences (en supprimant la première ligne)
  data_diff <- data_diff[-1, ]   # on enlève la première ligne
  data_diff$ECT <- ECT[-1]  
  
  # -----------------------------
  # Modèle ECM
  # -----------------------------
  
  formula_ecm <- as.formula(
    paste0(
      "`diff_", y, "` ~ ",
      paste0("diff_", vars, collapse = " + "),
      " + ECT"
    )
  )
  
  reg_ecm <- lm(formula_ecm, data = data_diff)
  return(reg_ecm)
}





