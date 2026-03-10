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
      
      adf <- adf.test(residuals(reg))
      
      results <- rbind(
        results,
        data.frame(
          y = y,
          x = paste(x, collapse = ", "),
          k = k,
          R2 = r2,
          adf_pvalue = adf$p.value
        )
      )
    }
  }
  
  results <- results[order(results$adf_pvalue), ]
  return(results)
}
