
source("script/functions/retrieval_webstat.R")
source("script/functions/retrieval_insee.R")
source("script/functions/utilitaires.R")
source("script/functions/descriptives_functions.R")
source("script/functions/ARIMA_functions.R")
source("script/functions/ECM_functions.R")
source("script/collect_data_webstat&descriptive.R")
source("script/get_data_ECM.R")

data <- read.csv("cache/data_insee_bdf.csv", stringsAsFactors = FALSE)

data$taux_marge <- data$EBE / data$PIB
data$taux_epargne <- data$epargne2 / data$RDB * 100
data$Pays <- "France"
data <- data %>% dplyr::select(-epargne, -Taux_snf)
data$salaires3 <- ((data$salaires2 - lag(data$salaires2)) / lag(data$salaires2,1)) * 100 + data$inflation


########### Stationarité #############

col <- colnames(data)
print(col)
col <- col[!col %in% c("time","Pays")]
col <- col[sapply(data[col], is.numeric)]

result_stationarity <- stationarity_tests(data, col, countries = c("France"))
print(result_stationarity)

############## Co-intégrité ###########

##récupérer variables non-stationnaires####
vars <- rownames(result_stationarity)
vars_ns <- vars[result_stationarity$Stationnarite == "Non-Stationnaire"]
I1_vars <- unique(sub(".* - ", "", vars_ns))

print(col)

I1_vars_menage <-  I1_vars[!I1_vars %in% c("defaillances", "EBE", "FBCF", "endettement_menage", "endettement_snf","endettement_agent_nonfinancie_privee", "part_menage", "demande_credit_snf", "taux_marge", "year","quarter")]
print(I1_vars_menage)

I1_vars_snf <- I1_vars[!I1_vars %in% c("epargne2","taux_epargne","RDB", "credit_aplusunan", "endettement_snf", "endettement_menage","endettement_agent_nonfinancie_privee", "part_menage","Taux_immo", "Duree_immo", "prix_logement", "year","quarter")]
print(I1_vars_snf)


############################
#Ménages 
############################


ECMEXP <- ECM_compute(y = "endettement_menage", vars = c("Taux_immo", "salaires", "taux_epargne" ), I1_vars = c("lag1_endettement_menage", "PIB", "lag1_Taux_immo"), I0_vars = c("octroi_credit"), data)
summary(ECMEXP$long_term)
reg <- ECMEXP$long_term
adf.test(reg$residuals)

summary(ECMEXP$ECM)
reg <- ECMEXP$ECM
print(I1_vars_menage)
adf.test(reg$residuals)
lmtest::bgtest(reg, 4)
lmtest::bptest(reg)
AIC(reg)
BIC(reg)
cor(model.matrix(reg)[, -1])

ECM_plot(y = "endettement_menage", vars = c("Taux_immo", "salaires", "taux_epargne" ),
         I1_vars = c("lag1_endettement_menage", "PIB", "lag1_Taux_immo"),
         I0_vars = c("octroi_credit"), data, plot_LT = T)


#######################

ECMEXP <- ECM_compute(y = "endettement_menage", vars = c("Taux_immo", "salaires", "taux_epargne" ), I1_vars = c("lag1_endettement_menage", "PIB", "lag1_Taux_immo","EURIBOR"), I0_vars = c(), data)
summary(ECMEXP$long_term)
reg <- ECMEXP$long_term
adf.test(reg$residuals)

summary(ECMEXP$ECM)
reg <- ECMEXP$ECM

adf.test(reg$residuals)
lmtest::bgtest(reg, 4)
lmtest::bptest(reg)
AIC(reg)
BIC(reg)
cor(model.matrix(reg)[, -1])

ECM_plot(y = "endettement_menage", vars = c("Taux_immo", "salaires", "taux_epargne" ),
         I1_vars = c("lag1_endettement_menage", "PIB", "lag1_Taux_immo","EURIBOR"),
         I0_vars = c(), data, plot_LT = T)


############################
#SNF 
############################

data$log_end_snf <- log(data$endettement_snf)

ECMEXP <- ECM_compute(y = "endettement_snf", vars = c("FBCF", "EURIBOR", "salaires", "chomage"), I1_vars = c("lag1_endettement_snf", "FBCF"), I0_vars = c(), data)
summary(ECMEXP$long_term)
reg <- ECMEXP$long_term
adf.test(reg$residuals)
summary(ECMEXP$ECM)
reg <- ECMEXP$ECM

adf.test(reg$residuals)
lmtest::bgtest(reg, 4)
lmtest::bptest(reg)
AIC(reg)
BIC(reg)
cor(model.matrix(reg)[, -1])

ECM_plot(y = "endettement_snf", vars = c("FBCF", "EURIBOR", "salaires", "chomage"),
         I1_vars = c("lag1_endettement_snf", "FBCF", "defaillances"),
         I0_vars = c(), data, plot_LT = T)


print(col)

ECMEXP <- ECM_compute(y = "log_end_snf", vars = c("FBCF", "EURIBOR", "salaires", "chomage"), I1_vars = c("lag1_log_end_snf","FBCF"), I0_vars = c(), data)
summary(ECMEXP$long_term)
reg <- ECMEXP$long_term
adf.test(reg$residuals)
summary(ECMEXP$ECM)
reg <- ECMEXP$ECM

adf.test(reg$residuals)
check_residuals_2(reg$residuals, lags = 20)
AIC(reg)
BIC(reg)
cor(model.matrix(reg)[, -1])

lmtest::coeftest(reg, vcov = sandwich::vcovHAC(reg))


ECM_plot(y = "log_end_snf", vars = c("FBCF", "EURIBOR", "salaires", "chomage"),
         I1_vars = c("lag1_log_end_snf", "FBCF", "defaillances"),
         I0_vars = c(), data, plot_LT = T)



##############################################
# Taux Immo et salaires 
###############################################


data$spreads = data$Taux_immo - data$Taux_long
data$time <- as.Date(data$time)

data_f <- data[data$time >= as.Date("2006-01-01"), ]

data_f$salaires3_ma4 <- stats::filter(data_f$salaires3, rep(1/4, 4), sides = 1)

plot(data_f$time, data_f$salaires, 
     type = "l", 
     col = "steelblue",
     lwd = 2,
     main = "Taux de croissance",
     xlab = "Date",
     ylab = "Spread (en points)",
     ylim = range(c(data_f$salaires, data_f$salaires3, salaires3_ma4), na.rm = TRUE))

# Série 2
lines(data_f$time, data_f$salaires3, 
      col = "grey", 
      lwd = 2)

# Moyenne mobile (alignée correctement)
lines(data_f$time, data_f$salaires3_ma4, 
      col = "red", 
      lwd = 2)

grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
abline(h = 0, col = "black", lty = 2, lwd = 1.5)

legend("bottomleft", 
       legend = c("Salaires négociés", "Salaires réel", "MA(4) salaires réel"),
       col = c("steelblue", "grey", "red"),
       lwd = 2)


x <- data_f$salaires
y <- stats::lag(data_f$salaires3_ma4,1)


y_rescaled <- (y - mean(y, na.rm=TRUE)) * 
  (sd(x, na.rm=TRUE) / sd(y, na.rm=TRUE)) + 
  mean(x, na.rm=TRUE)


plot(data_f$time, data_f$salaires, 
     type = "l", 
     col = "steelblue",
     lwd = 2,
     main = "Taux de croissance",
     xlab = "Date",
     ylab = "Spread (en points)",
     ylim = range(c(data_f$salaires, data_f$salaires3, salaires3_ma4), na.rm = TRUE))


# Moyenne mobile (alignée correctement)
lines(data_f$time, y_rescaled, 
      col = "red", 
      lwd = 2)

grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
abline(h = 0, col = "black", lty = 2, lwd = 1.5)

legend("bottomleft", 
       legend = c("Salaires négociés", "MA(4) salaires réel réajusté"),
       col = c("steelblue", "red"),
       lwd = 2)



#########################

plot(data_f$time, data_f$spreads, 
     type = "l", 
     col = "steelblue",
     lwd = 2,
     main = "Spread : Taux immobilier - Taux long terme",
     xlab = "Date",
     ylab = "Spread (en points)")

grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

adf.test(na.omit(data_f$spreads))
kpss.test(na.omit(data_f$spreads))

check_residuals_2(na.omit(data_f$spreads) - mean(data_f$spreads), lags = 20)



##########################################

bp <- strucchange::breakpoints(spreads ~ 1, data = data_f)
summary(bp)
plot(bp)


data_f$time[bp$breakpoints]  

data_f <- data_f %>%
  mutate(regime = case_when(
    time < "2008-09-01"  ~ "R1_pre_crise",
    time < "2011-12-01"  ~ "R2_crise",
    time < "2021-12-01"  ~ "R3_taux_bas",
    TRUE                 ~ "R4_remontee"
  ))


data_f %>%
  group_by(regime) %>%
  summarise(spread_moyen = mean(spreads), spread_sd = sd(spreads))

anova(lm(spreads ~ regime, data = data_f))

ggplot(data_f, aes(x = time, y = spreads, color = regime)) +
  geom_line() +
  geom_hline(data = data_f %>% group_by(regime) %>% summarise(m = mean(spreads)),
             aes(yintercept = m, color = regime), linetype = "dashed", linewidth = 1) +
  labs(title = "Spreads par régime avec moyenne", y = "Spread", x = "") +
  theme_minimal()

xreg <- model.matrix(~ regime, data = data_f)[, -1]  # enlève l'intercept
#################################


results_all <- rbind(
  test_arima_models_aic_bic(data_f, "France", var_name = "spreads", p_max = 2, d = 0, q_max = 2, xreg = xreg),
  test_arima_models_aic_bic(data_f, "France", var_name = "spreads", p_max = 2, d = 1, q_max = 2, xreg = xreg),
  test_arima_models_aic_bic(data_f, "France", var_name = "spreads", p_max = 2, d = 1, q_max = 2, xreg = NULL)
)

results_all <- results_all[order(results_all$AIC), ]

results_all

mod_1 <- Arima(data_f$spreads,
             order = c(1,0,1),
             xreg = xreg)

mod_2 <- Arima(data_f$spreads,
               order = c(2,1,0))

check_residuals_2(mod_1$residuals, lags = 20)
adf.test(mod_1$residuals)

check_residuals_2(mod_2$residuals, lags = 20)
adf.test(mod_2$residuals)

################################

h_ahead <- 10
xreg_fore <- matrix(0, nrow = h_ahead, ncol = ncol(xreg))
colnames(xreg_fore) <- colnames(xreg)
xreg_fore[, "regimeR4_remontee"] <- 1

preds <- forecast(mod_1, h = h_ahead, xreg = xreg_fore)

# 2. Tracer directement
autoplot(preds) +
  ggtitle("Prévision du Spread à 18 mois") +
  xlab("Temps") + ylab("Spread") +
  theme_minimal()


################################
# Forecasting 
################################


sheets <- excel_sheets("data/Data_forecasting.xlsx")

list_data <- lapply(sheets, function(s) {
  read_excel("data/Data_forecasting.xlsx", sheet = s)
})

names(list_data) <- sheets

list_data










