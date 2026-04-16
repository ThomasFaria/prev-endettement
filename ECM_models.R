
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
#Versions STAT Lasso
############################

lasso <- Lasso(y = "endettement_menage", vars = I1_vars_menage, data, use_1se = TRUE)
coefs <- lasso$coefs
print(coefs)
non_zero_vars <- rownames(coefs)[coefs[,1] != 0]
non_zero_vars <- setdiff(non_zero_vars, "(Intercept)")  # enlever l'intercept

ECM_lasso1 <- ECM_compute(y = "endettement_menage", vars = non_zero_vars, I1_vars = non_zero_vars, I0_vars = c(), data)
summary(ECM_lasso1$long_term)
reglt <- ECM_lasso1$long_term
summary(ECM_lasso1$ECM)
reg <- ECM_lasso1$ECM
adf.test(reg$residuals)
lmtest::bgtest(reg, 4)
lmtest::bptest(reg)
AIC(reg)
BIC(reg)


ECM_lasso2 <- ECM_compute(y = "endettement_menage", vars = c("DP", "Taux_long", "EURIBOR","Duree_immo","prix_logement"), I1_vars = c("lag1_endettement_menage", "prix_logement", "EURIBOR"), I0_vars = c(), data)
summary(ECM_lasso2$long_term)
reglt <- ECM_lasso2$long_term
summary(ECM_lasso2$ECM)
reg <- ECM_lasso2$ECM
adf.test(reg$residuals)
lmtest::bgtest(reg, 4)
lmtest::bptest(reg)
AIC(reg)
BIC(reg)

adf.test(reglt$residuals)

ECM_plot(y = "endettement_menage", vars = c("DP", "Taux_long", "EURIBOR","Duree_immo","prix_logement"), I1_vars = c("lag1_endettement_menage", "Taux_long", "EURIBOR"), I0_vars = c(), data, plot_LT = T)

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


############################


############################
#Versions Expert 
############################


ECMEXP <- ECM_compute(y = "endettement_menage", vars = c("Taux_long", "salaires", "taux_epargne" ), I1_vars = c("lag1_endettement_menage", "PIB"), I0_vars = c(), data)
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



ECM_plot(y = "endettement_menage", vars = c("Taux_long", "salaires", "taux_epargne" ),
         I1_vars = c("lag1_endettement_menage", "PIB"),
         I0_vars = c("octroi_credit"), data, plot_LT = T)








############################


### combinaison suivante ne fonctionne pas###
#reg_long <- lm(endettement_menage ~ PIB + RDB + FBCF, data = data)
#summary(reg_long)
#adf.test(residuals(reg_long))

############################

View(data)

#### autre combinaison :### 
reg_long <- lm(endettement_menage ~ prix_logement + Taux_long + EURIBOR + chomage, data = data)
summary(reg_long)
adf.test(residuals(reg_long))
## résultat : on trouve p-value de 0.07 donc acceptable et R^2 de 0.94##
## on teste ECM avec cette combinaison### 
# calculer le terme de correction
data$ECM_term <- residuals(reg_long)

# créer les Δvariables pour le court terme
data$d_endettement <- c(NA, diff(data$endettement_menage))
data$d_prix_logement <- c(NA, diff(data$prix_logement))
data$d_Taux_long <- c(NA, diff(data$Taux_long))
data$d_EURIBOR <- c(NA, diff(data$EURIBOR))
data$d_chomage <- c(NA, diff(data$chomage))

# ECM : Δendettement dépend des Δvariables + terme de correction lagué
ecm_model <- lm(d_endettement ~ d_prix_logement + d_Taux_long + d_EURIBOR + d_chomage + lag(ECM_term, 1), data = data)
summary(ecm_model)

#### Prix logement (+) → si le prix augmente, les ménages s’endettent plus (logique).
####Taux_long (-) et EURIBOR (-) → hausse des taux → endettement freiné (effet classique du coût du crédit).
####ECM_term (-) → l’endettement revient vers l’équilibre de long terme après un choc.




