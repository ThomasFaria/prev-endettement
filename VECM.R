library(tsDyn)
library(urca)
library(vars)
library(dplyr)

# importer data 

file_path <- "cache/data_insee_bdf.csv" 

if (!file.exists(file_path)) {
  stop("Le fichier data_insee_bdf.csv est introuvable dans /cache")
}

data <- read.csv(file_path)

# format date
data$time <- as.Date(data$time)

# Variables 

# MENAGES
vars_menage <- c(
  "endettement_menage",
  "inflation",
  "taux_credit",
  "revenu_reel",
  "credit_menage"
)

# SNF
vars_snf <- c(
  "endettement_snf",
  "inflation",
  "taux_credit_entreprise",
  "gdp",
  "credit_snf",
  "fbcf"
)

# CHOIX DU SYSTEME (MENAGES OU SNF)

# choisir ici : soit on travaille pour les ménages soit SNF (2 modèles séparés)
vars_vecm <- vars_menage
# vars_vecm <- vars_snf   # <- décommenter pour SNF


# clean data

# vérifier variables manquantes
missing_vars <- vars_vecm[!vars_vecm %in% names(data)]

if (length(missing_vars) > 0) {
  stop(paste("Variables manquantes :", paste(missing_vars, collapse = ", ")))
}

data_vecm <- data %>%
  select(all_of(vars_vecm)) %>%
  na.omit()

# log transformation (standard macro)
data_vecm <- log(data_vecm)


# Sélection du lag (AIC)

lag_select <- VARselect(
  data_vecm,
  lag.max = 8,
  type = "const"
)

print(lag_select$selection)

lag_opt <- lag_select$selection["AIC(n)"]

if (is.na(lag_opt) || lag_opt < 1) {
  lag_opt <- 2
}

cat("\n Lag optimal choisi :", lag_opt, "\n")


# JOHANSEN COINTEGRATION TEST

jo_test <- ca.jo(
  data_vecm,
  type = "trace",
  ecdet = "const",
  K = lag_opt
)

summary(jo_test)


# Choix du r (COINTEGRATION RANK)
r <- 1   # à ajuster selon summary(jo_test), nombre de relations de long terme entre les variables

# Estimation du VECM

vecm_model <- VECM(
  data_vecm,
  lag = lag_opt,
  r = r,
  estim = "ML"
)

summary(vecm_model)

# Estimation

forecast_horizon <- 4

pred <- predict(vecm_model, n.ahead = forecast_horizon)
 
print(pred)

# EXTRACTION VARIABLE PRINCIPALE

target <- if ("endettement_menage" %in% colnames(data_vecm)) {
  "endettement_menage"
} else {
  "endettement_snf"
}

if (target %in% names(pred$fcst)) {
  cat("\nPrévision :", target, "\n")
  print(pred$fcst[[target]])
}

