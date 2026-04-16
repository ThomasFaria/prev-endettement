
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

print(I1_vars)

I1_vars_menage <-  I1_vars[!I1_vars %in% c("defaillances", "EBE", "FBCF", "endettement_menage", "endettement_snf","endettement_agent_nonfinancie_privee", "part_menage", "demande_credit_snf", "taux_marge", "year","quarter")]
print(I1_vars_menage)

I1_vars_snf <- I1_vars[!I1_vars %in% c("epargne2","taux_epargne","RDB", "credit_aplusunan", "endettement_snf", "endettement_menage","endettement_agent_nonfinancie_privee", "part_menage","Taux_immo", "Duree_immo", "prix_logement", "year","quarter")]
print(I1_vars_snf)


############################
#Versions Expert 
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
print(I1_vars_menage)

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



data$spreads = data$Taux_immo - data$Taux_long
data$time <- as.Date(data$time)

data_f <- data[data$time >= as.Date("2006-01-01"), ]

plot(data_f$time, data_f$spreads - mean(data_f$spreads), 
     type = "l", 
     col = "steelblue",
     lwd = 2,
     main = "Spread : Taux immobilier - Taux long terme",
     xlab = "Date",
     ylab = "Spread (en points)")

grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

adf.test(na.omit(data$spreads))
kpss.test(na.omit(data$spreads))

ma_2ans <- rollmean(data_f$spreads, k = 8, fill = NA, align = "center")
data_f$spreads_centered <- data_f$spreads - ma_2ans

plot(data_f$time, data_f$spreads_centered, 
     type = "l", 
     col = "steelblue",
     lwd = 2,
     main = "Spread : Taux immobilier - Taux long terme",
     xlab = "Date",
     ylab = "Spread (en points)")

grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

adf.test(data_f$spreads_centered)
kpss.test(data_f$spreads_centered)





plot(data_f$time, data_f$octroi_credit, 
     type = "l", 
     col = "steelblue",
     lwd = 2,
     main = "Octroi de crédit",
     xlab = "Date",
     ylab = "Valeure")

grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)
