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
data <- data %>% dplyr::select(-epargne)


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


I1_vars_menage <-  I1_vars[!I1_vars %in% c("defaillances", "EBE", "FBCF", "endettement_menage", "endettement_snf","endettement_agent_nonfinancie_privee", "part_menage", "Taux_snf", "demande_credit_snf", "taux_marge")]
print(I1_vars_menage)

I1_vars_snf <- I1_vars[!I1_vars %in% c("epargne2","taux_epargne","RDB", "credit_aplusunan", "endettement_snf", "endettement_menage","endettement_agent_nonfinancie_privee", "part_menage","Taux_immo", "Duree_immo", "prix_logement")]
print(I1_vars_snf)


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
  return(result)
}


test_cointegration(data, y = "endettement_menage", I1_vars_menage )







### combinaison suivante ne fonctionne pas###
reg_long <- lm(endettement_menage ~ PIB + RDB + FBCF, data = data)
summary(reg_long)
adf.test(residuals(reg_long))

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

### combinaison suivante ne fonctionne pas###
reg_long <- lm(endettement_menage ~ PIB + RDB + FBCF, data = data)
summary(reg_long)
adf.test(residuals(reg_long))

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




