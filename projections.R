## ESTIMATIONS VARIBALES EXPLICATIVES POUR ECM ##


# Estimations BdF PIB réel et inflation pour 2026, 2027 et 2028 (par an) : 
forecast_macro <- data.frame(
  year = c(2026, 2027, 2028),
  pib = c(0.9, 0.8, 1.2),
  inflation = c(1.7, 1.4, 1.6)
)

# expansion trimestrielle simple
forecast_macro_q <- data.frame(
  time = seq(as.Date("2026-03-01"), as.Date("2028-12-01"), by = "quarter")
)

forecast_macro_q$pib <- rep(forecast_macro$pib, each = 4)
forecast_macro_q$inflation <- rep(forecast_macro$inflation, each = 4)

# Estimations taux longs État français à 10 ans 
taux_long_data <- data.frame(
  year = c(2025, 2026, 2027, 2028),
  taux_10ans = c(3.4, 3.6, 3.9, 4.1)
)

taux_long_q <- data.frame(
  time = seq(as.Date("2025-03-01"), as.Date("2028-12-01"), by = "quarter")
)

taux_long_q$taux_10ans <- rep(taux_long_data$taux_10ans, each = 4)

# Estimations euribor 3 mois 

euribor_data <- data.frame(
  year = c(2025, 2026, 2027, 2028),
  euribor_3m = c(2.2, 2.3, 2.6, 2.6)
)

euribor_q <- data.frame(
  time = seq(as.Date("2025-03-01"), as.Date("2028-12-01"), by = "quarter")
)

euribor_q$euribor_3m <- rep(euribor_data$euribor_3m, each = 4)



