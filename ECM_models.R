library(dplyr)
library(tseries)
library(urca)

source("script/functions/retrieval_webstat.R")
source("script/functions/retrieval_insee.R")
source("script/functions/utilitaires.R")
source("script/functions/descriptives_functions.R")
source("script/functions/ARIMA_functions.R")
source("script/collect_data_webstat&descriptive.R")
source("script/get_data_ECM.R")

data <- read.csv("cache/data_insee_bdf.csv", stringsAsFactors = FALSE)

data$taux_marge <- data$EBE / data$PIB
data$taux_epargne <- data$epargne2 / data$RDB
data$Pays <- "France"
data <- data %>% select(-epargne)

data <- data %>% select(-one_of("epargne"))
colnames(data)
View(data)
################################################
View(data)

########### Stationarité #############

col <- colnames(data)
print(col)
col <- col[!col %in% c("time","Pays")]
col <- col[sapply(data[col], is.numeric)]

result_stationarity <- stationarity_tests(data, col, countries = c("France"))

print(result_stationarity)
View(result_stationarity)

############## Co-intégrité ###########

##récupérer variables non-stationnaires####
vars <- rownames(result_stationarity)
vars_ns <- vars[result_stationarity$Stationnarite == "Non-Stationnaire"]
I1_vars <- unique(sub(".* - ", "", vars_ns))

print(I1_vars)

###### VAR ####
library(vars)
VAR_vars_key <- c("PIB", "RDB", "FBCF", "chomage") 

VAR_data <- data.frame(lapply(data[VAR_vars_key], diff))
colnames(VAR_data) <- paste0("d_", VAR_vars_key)
VAR_model <- VAR(VAR_data, p = 1)

summary(VAR_model)

### tester différentes combinaisons####

reg_long <- lm(PIB ~ RDB + chomage, data = data)
adf.test(residuals(reg_long))  # p-value de 0.05 donc co-intégration valide 

### code ECM avec combinaison précédente ###

# calculer l'erreur de correction
data$ECM_term <- residuals(lm(PIB ~ RDB + chomage, data = data))

# créer les Δvariables
data$d_PIB <- c(NA, diff(data$PIB))
data$d_RDB <- c(NA, diff(data$RDB))
data$d_chomage <- c(NA, diff(data$chomage))

# ECM : ΔPIB dépend de ΔRDB, Δchomage + terme de correction
ecm_model <- lm(d_PIB ~ d_RDB + d_chomage + lag(ECM_term, 1), data = data)
summary(ecm_model)
### Résultat : ΔPIB dépend de :d_RDB → effet court terme positif et très significatif (p < 0.001)
### d_chomage → effet court terme positif et significatif (p ≈ 1.6e-5)





