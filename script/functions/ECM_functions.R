

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
  
  models_valides <- data.frame()  # stockage des modèles valides
  
  for (i in 1:nrow(results)) {
    
    # Récupération des variables explicatives
    vars_raw <- as.character(results$x[i])
    vars_raw <- trimws(vars_raw)
    
    # Découper les variables par virgule
    vars <- trimws(unlist(strsplit(vars_raw, ",")))
    
    # Vérification que la ligne contient au moins une variable
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
    
    # Filtre sur ECT : signe négatif et p-value < seuil
    lambda <- coef(reg_ecm)["ECT"]
    pval <- summary(reg_ecm)$coefficients["ECT", "Pr(>|t|)"]
    
    if (!is.na(lambda) && lambda < 0 && pval < seuil_pval) {
      models_valides <- rbind(
        models_valides,
        cbind(results[i, ], lambda = lambda, pval = pval)
      )
    }
  }
  
  return(models_valides)
}