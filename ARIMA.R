source("docs/retrieval_webstat.R")

library(portes)
library(astsa)
library(dplyr)
library(zoo) 
library(forecast)
library(tseries)
library(lubridate)


secrets <- yaml::read_yaml("secret.yaml")

##############
#DATA.      ##
##############

series <- c(
  "CNFSI.Q.S.FR.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T"
)
series2 <- c(
  "CNFSI.Q.S.FR.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T" 
)

data <- get_webstat(series_keys = series, api_key = secrets$api_key)
data2 <- get_webstat(series_keys = series2, api_key = secrets$api_key)

merged_data <- merge(data, data2, by = c("time"), suffixes = c("_menages", "_snf"))
merged_data$DNF <- merged_data$values_menages + merged_data$values_snf
merged_data$part_menage <-  merged_data$values_menages / merged_data$DNF * 100

######################
#STATIONNARITE.     ##
######################

#Test Stationnarité séries 

adf_result_DNF <- adf.test(merged_data$DNF, alternative = "stationary")
adf_result_menages <- adf.test(merged_data$values_menages, alternative = "stationary")
adf_result_snf <- adf.test(merged_data$values_snf, alternative = "stationary")

kpss_result_DNF <- kpss.test(merged_data$DNF, null = "Trend")
kpss_result_menages <- kpss.test(merged_data$values_menages, null = "Trend")
kpss_result_snf <- kpss.test(merged_data$values_snf, null = "Trend")

pp_result_DNF <- pp.test(merged_data$DNF)
pp_result_menages <- pp.test(merged_data$values_menages)
pp_result_snf <- pp.test(merged_data$values_snf)


extract_test_results <- function(test_result, test_name) {
  if (grepl("KPSS", test_name)) {
    # Pour KPSS, p-value < 0.05 signifie non-stationnaire
    stationarity <- ifelse(test_result$p.value > 0.05, "Stationnaire", "Non-Stationnaire")
  } else {
    # Pour ADF et PP, p-value < 0.05 signifie stationnaire
    stationarity <- ifelse(test_result$p.value < 0.05, "Stationnaire", "Non-Stationnaire")
  }
  
  return(data.frame(
    Test = test_name,
    Statistique = round(test_result$statistic, 4),
    P_Value = round(test_result$p.value, 4),
    Stationnarite = stationarity,
    Lag = ifelse("parameter" %in% names(test_result), test_result$parameter, NA),
    Alternative = ifelse("alternative" %in% names(test_result), 
                         as.character(test_result$alternative), NA)
  ))
}

# Création du tableau combiné
results_table <- rbind(
  extract_test_results(adf_result_DNF, "ADF - DNF"),
  extract_test_results(kpss_result_DNF, "KPSS - DNF"),
  extract_test_results(pp_result_DNF, "PP - DNF"),
  
  extract_test_results(adf_result_menages, "ADF - Ménages"),
  extract_test_results(kpss_result_menages, "KPSS - Ménages"),
  extract_test_results(pp_result_menages, "PP - Ménages"),
  
  extract_test_results(adf_result_snf, "ADF - SNF"),
  extract_test_results(kpss_result_snf, "KPSS - SNF"),
  extract_test_results(pp_result_snf, "PP - SNF")
)

# Affichage du tableau
print("RÉSULTATS DES TESTS DE STATIONNARITÉ")
print("======================================")
print(results_table)


# Différentiation
df <- merged_data
merged_data <- merged_data %>%
  mutate(
    DNF_diff = c(NA, diff(DNF, differences = 1)),
    values_menages_diff = c(NA, diff(values_menages, differences = 1)),
    values_snf_diff = c(NA, diff(values_snf, differences = 1))
  )

merged_data <- na.omit(merged_data)


#Visualisation graphique série différentié avec moyenne mobile de 3 ans

merged_data$DNF_diff_ma <- rollmean(merged_data$DNF_diff, k = 12, fill = NA, align = "right")
merged_data$values_menages_diff_ma <- rollmean(merged_data$values_menages_diff, k = 12, fill = NA, align = "right")
merged_data$values_snf_diff_ma <- rollmean(merged_data$values_snf_diff, k = 12, fill = NA, align = "right")


ggplot(merged_data, aes(x = time)) +
  geom_line(aes(y = DNF_diff), color = "blue") +
  geom_point(aes(y = DNF_diff), color = "blue") +
  geom_line(aes(y = DNF_diff_ma), color = "red", size = 1.2) +
  ggtitle("DNF différencié avec moyenne mobile de 3 ans") +
  theme_minimal()


ggplot(merged_data, aes(x = time)) +
  geom_line(aes(y = values_snf_diff), color = "blue") +
  geom_point(aes(y = values_snf_diff), color = "blue") +
  geom_line(aes(y = values_snf_diff_ma), color = "red", size = 1.2) +
  ggtitle("SNF différencié avec moyenne mobile de 3 ans") +
  theme_minimal()

#Test Stationnarité séries différentiées


adf_result_DNF_diff <- adf.test(merged_data$DNF_diff, alternative = "stationary")
adf_result_menages_diff <- adf.test(merged_data$values_menages_diff, alternative = "stationary")
adf_result_snf_diff <- adf.test(merged_data$values_snf_diff, alternative = "stationary")

kpss_result_DNF_diff <- kpss.test(merged_data$DNF_diff, null = "Level")
kpss_result_menages_diff <- kpss.test(merged_data$values_menages_diff, null = "Level")
kpss_result_snf_diff <- kpss.test(merged_data$values_snf_diff, null = "Level")

pp_result_DNF_diff <- pp.test(merged_data$DNF_diff)
pp_result_menages_diff <- pp.test(merged_data$values_menages_diff)
pp_result_snf_diff <- pp.test(merged_data$values_snf_diff)


# Création du tableau combiné
results_table_DIFF <- rbind(
  extract_test_results(adf_result_DNF_diff, "ADF - DNF"),
  extract_test_results(kpss_result_DNF_diff, "KPSS - DNF"),
  extract_test_results(pp_result_DNF_diff, "PP - DNF"),
  
  extract_test_results(adf_result_menages_diff, "ADF - Ménages"),
  extract_test_results(kpss_result_menages_diff, "KPSS - Ménages"),
  extract_test_results(pp_result_menages_diff, "PP - Ménages"),
  
  extract_test_results(adf_result_snf_diff, "ADF - SNF"),
  extract_test_results(kpss_result_snf_diff, "KPSS - SNF"),
  extract_test_results(pp_result_snf_diff, "PP - SNF")
)

# Affichage du tableau
print("RÉSULTATS DES TESTS DE STATIONNARITÉ ARÈS DIFFÉRENTIATION")
print("======================================")
print(results_table_DIFF)

#####
#Les séries différentiés semblent stationnaires, un doute existes pour la série endettement des ménages  
#####


# Tranformation double-difference ménage

merged_data2 <- merged_data %>%
  mutate(
    values_menages_double_diff = c(NA, diff(values_menages_diff, differences = 1))
  )

merged_data2 <- na.omit(merged_data2)


#Visualisation graphique série différentié avec moyenne mobile de 3 ans

merged_data2$values_menages_double_diff_ma <- rollmean(merged_data2$values_menages_double_diff, k = 12, fill = NA, align = "right")


ggplot(merged_data2, aes(x = time)) +
  geom_line(aes(y = values_menages_double_diff), color = "blue") +
  geom_point(aes(y = values_menages_double_diff), color = "blue") +
  geom_line(aes(y = values_menages_double_diff_ma), color = "red", size = 1.2) +
  ggtitle("Ménages double-différencié avec moyenne mobile de 3 ans") +
  theme_minimal()

#Test Stationnarité séries Log - Différentiées

adf_result_menages_double_diff <- adf.test(merged_data2$values_menages_double_diff, alternative = "stationary")
kpss_result_menages_double_diff <- kpss.test(merged_data2$values_menages_double_diff, null = "Level")
pp_result_menages_double_diff <- pp.test(merged_data2$values_menages_double_diff)


# Création du tableau combiné
results_table_double_DIFF <- rbind(
  extract_test_results(adf_result_DNF_diff, "ADF - DNF"),
  extract_test_results(kpss_result_DNF_diff, "KPSS - DNF"),
  extract_test_results(pp_result_DNF_diff, "PP - DNF"),
  
  extract_test_results(adf_result_menages_double_diff, "ADF - Ménages"),
  extract_test_results(kpss_result_menages_double_diff, "KPSS - Ménages"),
  extract_test_results(pp_result_menages_double_diff, "PP - Ménages"),
  
  extract_test_results(adf_result_snf_diff, "ADF - SNF"),
  extract_test_results(kpss_result_snf_diff, "KPSS - SNF"),
  extract_test_results(pp_result_snf_diff, "PP - SNF")
)

# Affichage du tableau
print("RÉSULTATS DES TESTS DE STATIONNARITÉ ARÈS DOUBLE-DIFFÉRENTIATION MENAGES")
print("======================================")
print(results_table_double_DIFF)


# Tranformation log difference 

df$DNF <- log(df$DNF)
df$values_menages <- log(df$values_menages)
df$values_snf <- log(df$values_snf)

merged_data3 <-df %>%
  mutate(
    DNF_diff = c(NA, diff(DNF, differences = 1)),
    values_menages_diff = c(NA, diff(values_menages, differences = 1)),
    values_snf_diff = c(NA, diff(values_snf, differences = 1))
  )

merged_data3 <- na.omit(merged_data3)


#Visualisation graphique série différentié avec moyenne mobile de 3 ans

merged_data3$DNF_diff_ma <- rollmean(merged_data3$DNF_diff, k = 12, fill = NA, align = "right")
merged_data3$values_menages_diff_ma <- rollmean(merged_data3$values_menages_diff, k = 12, fill = NA, align = "right")
merged_data3$values_snf_diff_ma <- rollmean(merged_data3$values_snf_diff, k = 12, fill = NA, align = "right")


ggplot(merged_data3, aes(x = time)) +
  geom_line(aes(y = DNF_diff), color = "blue") +
  geom_point(aes(y = DNF_diff), color = "blue") +
  geom_line(aes(y = DNF_diff_ma), color = "red", size = 1.2) +
  ggtitle("DNF différencié avec moyenne mobile de 3 ans") +
  theme_minimal()


ggplot(merged_data3, aes(x = time)) +
  geom_line(aes(y = values_menages_diff), color = "blue") +
  geom_point(aes(y = values_menages_diff), color = "blue") +
  geom_line(aes(y = values_menages_diff_ma), color = "red", size = 1.2) +
  ggtitle("Ménages différencié avec moyenne mobile de 3 ans") +
  theme_minimal()


ggplot(merged_data3, aes(x = time)) +
  geom_line(aes(y = values_snf_diff), color = "blue") +
  geom_point(aes(y = values_snf_diff), color = "blue") +
  geom_line(aes(y = values_snf_diff_ma), color = "red", size = 1.2) +
  ggtitle("SNF différencié avec moyenne mobile de 3 ans") +
  theme_minimal()

#Test Stationnarité séries Log - Différentiées


adf_result_DNF_log_diff <- adf.test(merged_data3$DNF_diff, alternative = "stationary")
adf_result_menages_log_diff <- adf.test(merged_data3$values_menages_diff, alternative = "stationary")
adf_result_snf_log_diff <- adf.test(merged_data3$values_snf_diff, alternative = "stationary")

kpss_result_DNF_log_diff <- kpss.test(merged_data3$DNF_diff, null = "Level")
kpss_result_menages_log_diff <- kpss.test(merged_data3$values_menages_diff, null = "Level")
kpss_result_snf_log_diff <- kpss.test(merged_data3$values_snf_diff, null = "Level")

pp_result_DNF_log_diff <- pp.test(merged_data3$DNF_diff)
pp_result_menages_log_diff <- pp.test(merged_data3$values_menages_diff)
pp_result_snf_log_diff <- pp.test(merged_data3$values_snf_diff)


# Création du tableau combiné
results_table_log_DIFF <- rbind(
  extract_test_results(adf_result_DNF_diff, "ADF - DNF"),
  extract_test_results(kpss_result_DNF_diff, "KPSS - DNF"),
  extract_test_results(pp_result_DNF_diff, "PP - DNF"),
  
  extract_test_results(adf_result_menages_diff, "ADF - Ménages"),
  extract_test_results(kpss_result_menages_diff, "KPSS - Ménages"),
  extract_test_results(pp_result_menages_diff, "PP - Ménages"),
  
  extract_test_results(adf_result_snf_diff, "ADF - SNF"),
  extract_test_results(kpss_result_snf_diff, "KPSS - SNF"),
  extract_test_results(pp_result_snf_diff, "PP - SNF")
)

# Affichage du tableau
print("RÉSULTATS DES TESTS DE STATIONNARITÉ ARÈS LOG-DIFFÉRENTIATION")
print("======================================")
print(results_table_log_DIFF)


#Etude temporalité impact covid

ggplot(merged_data, aes(x = time)) +
  geom_line(aes(y = values_menages_diff), color = "blue") +
  geom_point(aes(y = values_menages_diff), color = "blue") +
  geom_line(aes(y = rollmean(values_menages_diff_ma, k = 4, fill = NA, align = "right")), 
            color = "red", size = 1.2) +
  ggtitle("Ménages différencié de 3 ans") +
  scale_x_date(limits = as.Date(c("2016-01-01", max(merged_data$time)))) +
  theme_minimal()

ggplot(merged_data, aes(x = time)) +
  geom_line(aes(y = DNF_diff), color = "blue") +
  geom_point(aes(y = DNF_diff), color = "blue") +
  geom_line(aes(y = rollmean(DNF_diff_ma, k = 4, fill = NA, align = "right")), 
            color = "red", size = 1.2) +
  ggtitle("DNF différencié de 3 ans") +
  scale_x_date(limits = as.Date(c("2016-01-01", max(merged_data$time)))) +
  theme_minimal()


ggplot(merged_data, aes(x = time)) +
  geom_line(aes(y = values_snf_diff), color = "blue") +
  geom_point(aes(y = values_snf_diff), color = "blue") +
  geom_line(aes(y = rollmean(values_snf_diff_ma, k = 4, fill = NA, align = "right")), 
            color = "red", size = 1.2) +
  ggtitle("SNF différencié de 3 ans") +
  scale_x_date(limits = as.Date(c("2016-01-01", max(merged_data$time)))) +
  theme_minimal()


#####
#Le covid représente un choc éxogène sur 2019-2021 ce qui créer des valeurs abbérante. Y à il un effet permanant du choc ? Possible avec la hausse des taux. 
#####

merged_data$covid <- ifelse(merged_data$time >= as.Date("2020-01-01") & 
                              merged_data$time <= as.Date("2021-12-31"), 1, 0)

merged_data2$covid <- ifelse(merged_data2$time >= as.Date("2020-01-01") & 
                              merged_data2$time <= as.Date("2021-12-31"), 1, 0)

#############################################################
# Vérification Autocorrelation et Partial Autocorrelation ###
#############################################################

DNF_diff <- merged_data$DNF_diff
menages_diff <- merged_data$values_menages_diff
menages_double_diff <- merged_data2$values_menages_double_diff
snf_diff <- merged_data$values_snf_diff

DNF_adj <- ifelse(merged_data$covid == 1, NA, merged_data$DNF_diff)

menages_adj <- ifelse(merged_data$covid == 1, NA, merged_data$values_menages_diff)

menages_double_adj <- ifelse(merged_data$covid == 1, NA, merged_data2$values_menages_double_diff)

snf_adj <- ifelse(merged_data$covid == 1, NA, merged_data$values_snf_diff)

###############################################################


ggAcf(DNF_diff, main = "ACF - DNF différencié") + ylim(-0.8, 0.8)
ggPacf(DNF_diff, main = "PACF - DNF différencié") + ylim(-0.8, 0.8)

#####
#POUR DNF AR(1) semble être privilégié et MA(0,1)
#####

ggAcf(menages_diff, main = "ACF - Ménages différencié") +  ylim(-0.8, 0.8)
ggPacf(menages_diff, main = "PACF - Ménages différencié") +  ylim(-0.8, 0.8)

#####
#POUR MÉNAGE AR(1) semble être privilégié et MA(0,1,2,3)
#####

ggAcf(menages_double_diff, main = "ACF - Ménages double différencié") +  ylim(-0.8, 0.8)
ggPacf(menages_double_diff, main = "PACF - Ménages double différencié") + ylim(-0.8, 0.8)

#####
# POUR DD MÉNAGE AR(1) et MA(1) semble être privilégié
#####

ggAcf(snf_diff, main = "ACF - SNF différencié") + ylim(-0.8, 0.8)
ggPacf(snf_diff, main = "PACF - SNF différencié") +  ylim(-0.8, 0.8)

#####
#POUR SNF le modèle AR(1) et MA(0,1,2) semble être privilégié
#####


##############SANS COVID######################

ggAcf(DNF_adj, main = "ACF - DNF différencié ADJ") + ylim(-0.8, 0.8)
ggPacf(DNF_adj, main = "PACF - DNF différencié ADJ") + ylim(-0.8, 0.8)

#####
#AR(1) semble être privilégié et MA(0)
#####

ggAcf(menages_adj, main = "ACF - Ménages différencié ADJ") +  ylim(-0.8, 0.8)
ggPacf(menages_adj, main = "PACF - Ménages différencié ADJ") +  ylim(-0.8, 0.8)

#####
#AR(1,2) semble être privilégié et MA(0) 
#####

ggAcf(menages_double_adj, main = "ACF - Ménages double différencié ADJ") +  ylim(-0.8, 0.8)
ggPacf(menages_double_adj, main = "PACF - Ménages double différencié ADJ") + ylim(-0.8, 0.8)

#####
# AR(1) , MA(1)
#####

ggAcf(snf_adj, main = "ACF - SNF différencié ADJ") + ylim(-0.8, 0.8)
ggPacf(snf_adj, main = "PACF - SNF différencié ADJ") +  ylim(-0.8, 0.8)

#####
#le modèle AR(0,1,2) et MA(0) semble être privilégié
#####


###############
# AIC et BIC  #
###############

y = merged_data$values_snf

test_arima_models <- function(y, p_max = 3, d = 1, q_max = 3) {
  
  grid <- expand.grid(p = 0:p_max, d = d, q = 0:q_max)
  models <- lapply(1:nrow(grid), function(i) as.numeric(grid[i, ]))
  model_names <- sapply(models, function(m) paste0("ARIMA", m[1], m[2], m[3]))

  results <- data.frame(
    Model = model_names,
    AIC = NA,
    BIC = NA
  )
  
  for (i in 1:length(models)) {
    m <- models[[i]]
    fit <- tryCatch({
      Arima(y, order = c(m[1], m[2], m[3]))
    }, error = function(e) {
      message("Modèle ", model_names[i], " n'a pas convergé : ", e$message)
      NULL
    })
    
    if (!is.null(fit)) {
      results$AIC[i] <- fit$aic
      results$BIC[i] <- AIC(fit, k = log(length(y)))
    }
  }
  
  # Trier par BIC
  results <- results[order(results$BIC), ]
  
  return(results)
}

y <- merged_data$values_snf_diff
summary_table <- test_arima_models(y)
summary_table

######################
#MODEL ARIMA        ##              
######################

############# SNF SANS COVID ###############

y <- merged_data$values_snf_diff
z <- merged_data$values_snf

length(y)
mean(y)
train_size <- 60  
test_size <- 12   
step <- 4       

grid <- expand.grid(p = 0:3, d = 1, q = 0:3)
models <- lapply(1:nrow(grid), function(i) as.numeric(grid[i, ]))
model_names <- sapply(models, function(m) paste0("ARIMA", m[1], m[2], m[3]))

errors  <- data.frame(matrix(nrow = length(models), ncol = 0))
merrors  <- data.frame(matrix(nrow = length(models), ncol = 0))
aic_mat <- data.frame(matrix(nrow = length(models), ncol = 0))
bic_mat <- data.frame(matrix(nrow = length(models), ncol = 0))
rownames(errors)  <- model_names
rownames(merrors)  <- model_names
rownames(aic_mat) <- model_names
rownames(bic_mat) <- model_names

window_info <- c()

for (start in seq(1, length(y) - train_size - test_size, by = step)) {
  
  train <- y[start:(start + train_size - 1)]
  test <- y[(start + train_size):(start + train_size + test_size - 1)]
  last_val <- z[start + train_size - 1] 
  
  fitted_models <- lapply(models, function(m) {
    tryCatch({
      fit <- Arima(train, order = c(m[1], 0, m[3]))
      return(fit)
    }, error = function(e) NULL)
  })
  
  errors[[paste0("window_", start)]] <- rep(NA, length(models))
  merrors[[paste0("window_", start)]] <- rep(NA, length(models))
  aic_mat[[paste0("window_", start)]] <- rep(NA, length(models))
  bic_mat[[paste0("window_", start)]] <- rep(NA, length(models))
  test_original <- z[(start + train_size):(start + train_size + test_size - 1)]
  
  window_info <- c(window_info, train_size)
  
  for (i in 1:length(fitted_models)) {
    fit <- fitted_models[[i]]
    if (!is.null(fit)) {
      pred <- forecast(fit, h = length(test))$mean
      pred_mean  <- last_val + cumsum(pred)
      errors[i, ncol(errors)] <- sqrt(mean((test_original - pred_mean)^2))
      merrors[i, ncol(merrors)] <- sqrt(mean(((test_original - pred_mean)/test_original)^2))*100
    }
  }
}

merrors

mean_RMSPE <- rowMeans(merrors, na.rm = TRUE)
mean_RMSE <- rowMeans(errors, na.rm = TRUE)
mean_rmse_hors_covid <- rowMeans(errors[, !colnames(errors) %in% c("window_117", "window_21", "window_25")], na.rm = TRUE)
mean_rmspe_hors_covid <- rowMeans(merrors[, !colnames(merrors) %in% c("window_117", "window_21", "window_25")], na.rm = TRUE)
var_RMSE <- apply(errors, 1, function(x) var(x, na.rm = TRUE))
var_RMSE_HC <- apply(errors[, !colnames(errors) %in% c("window_117", "window_21", "window_25")], 1, function(x) var(x, na.rm = TRUE))


summary_table <- data.frame(
  Model = rownames(errors),
  Mean_RMSE = rowMeans(errors, na.rm = TRUE),
  Mean_RMSE_HC = mean_rmse_hors_covid,
  Mean_RMSPE = rowMeans(merrors, na.rm = TRUE),
  Mean_RMSPE_HC = mean_rmspe_hors_covid,
  Var_RMSE = var_RMSE, 
  Var_RMSE_HC = var_RMSE_HC
)

summary_table <- summary_table[order(summary_table$Mean_RMSE_HC), ]
summary_table



############# SNF AVEC COVID ###############


#### CHOC TRANSITOIRE ####

covid_start <- 80 
covid_end   <- 92

grid <- expand.grid(p = 0:3, d = 0, q = 0:3)
models <- lapply(1:nrow(grid), function(i) as.numeric(grid[i, ]))
model_names <- sapply(models, function(m) paste0("ARIMA", m[1], m[2], m[3]))

errors2  <- data.frame(matrix(nrow = length(models), ncol = 0))
merrors2  <- data.frame(matrix(nrow = length(models), ncol = 0))
aic_mat2 <- data.frame(matrix(nrow = length(models), ncol = 0))
bic_mat2 <- data.frame(matrix(nrow = length(models), ncol = 0))
rownames(errors2)  <- model_names
rownames(merrors2)  <- model_names
rownames(aic_mat2) <- model_names
rownames(bic_mat2) <- model_names

window_info <- c()

start_seq <- seq(covid_start - train_size - 3 , length(y) - train_size - test_size, by = step)

for (start in start_seq) {
  
  train <- y[start:(start + train_size - 1)]
  test <- y[(start + train_size):(start + train_size + test_size - 1)]
  last_val <- z[start + train_size - 1] 
  
  train_xreg <- rep(0, length(train))
  test_xreg  <- rep(0, length(test))
  train_indices <- start:(start + train_size - 1)
  test_indices  <- (start + train_size):(start + train_size + test_size - 1)
  train_xreg[train_indices >= covid_start & train_indices <= covid_end] <- 1
  test_xreg[test_indices >= covid_start & test_indices <= covid_end]    <- 1
  train_xreg <- matrix(train_xreg, ncol = 1)
  test_xreg  <- matrix(test_xreg, ncol = 1)
  
  
  fitted_models <- lapply(models, function(m) {
    tryCatch({
      fit <- Arima(train, order = c(m[1], m[2], m[3]), xreg = train_xreg)
      return(fit)
    }, error = function(e) NULL)
  })
  
  errors2[[paste0("window_", start)]] <- rep(NA, length(models))
  merrors2[[paste0("window_", start)]] <- rep(NA, length(models))
  aic_mat2[[paste0("window_", start)]] <- rep(NA, length(models))
  bic_mat2[[paste0("window_", start)]] <- rep(NA, length(models))
  test_original <- z[(start + train_size):(start + train_size + test_size - 1)]
  
  window_info <- c(window_info, train_size)
  
  for (i in 1:length(fitted_models)) {
    fit <- fitted_models[[i]]
    if (!is.null(fit)) {
      pred <- forecast(fit, xreg = test_xreg, h = length(test))$mean
      pred_mean  <- last_val + cumsum(pred)
      errors2[i, ncol(errors2)] <- sqrt(mean((test_original - pred_mean)^2))
      merrors2[i, ncol(merrors2)] <- sqrt(mean(((test_original - pred_mean)/test_original)^2)) * 100
    }
  }
}

errors2


########### COMPARAISON AVEC/SANS COVID###########

errors_ordered  <- errors[match(summary_table$Model, rownames(errors)), ]
errors2_ordered <- errors2[match(summary_table$Model, rownames(errors2)), ]
errors2_ordered <- errors2_ordered[, -1]
common_cols <- colnames(errors2_ordered)
errors_ordered <- errors_ordered[, common_cols]

colnames(errors_ordered )
colnames(errors2_ordered )

comparison_table <- cbind(
  errors_ordered,
  setNames(errors2_ordered, paste0("COVID_", colnames(errors2_ordered)))
)
print(comparison_table)


#####################
## MODELE 102.     ##
#####################

############# GRAPH DIFFERENTE PREDICTION #############

train_size <- 60
test_size  <- 8
step       <- 4
y <- merged_data$values_snf_diff
z <- merged_data$values_snf
time_vals <- merged_data$time

p <- 1
d <- 0
q <- 2

pred_list <- list()

for (start in seq(1, length(y) - train_size - test_size, by = step)) {
  
  prev_val <- z[start + train_size - 1]
  train <- y[start:(start + train_size - 1)]
  test  <- y[(start + train_size):(start + train_size + test_size - 1)]
  
  fit <- Arima(train, order = c(p,d,q))
  fc  <- forecast(fit, h = length(test))
  
  pred_diff <- fc$mean
  pred_mean <- prev_val + cumsum(pred_diff)
  pred_mean <- c(prev_val, pred_mean)
  
  fc_se <- (fc$upper[,2] - fc$mean) / 1.96
  pred_var <- cumsum(fc_se^2)
  upper <- pred_mean + 1.96 * sqrt(pred_var)
  lower <- pred_mean - 1.96 * sqrt(pred_var)
  
  time_pred <- time_vals[(start + train_size - 1):(start + train_size + test_size - 1)]
  
  pred_list[[paste0("window_", start)]] <- data.frame(
    time  = time_pred,
    value = pred_mean,
    upper = upper,
    lower = lower,
    window = paste0("window_", start)
  )
  
  prev_val <- tail(pred_mean, 1)
}


pred_all <- do.call(rbind, pred_list)


ggplot() +
  geom_line(aes(x = time, y = value_snf), data = merged_data, color = "black") +
  geom_line(data = pred_all, aes(x = time, y = value, color = window)) +
  geom_ribbon(data = pred_all, aes(x = time, ymin = lower, ymax = upper, fill = window), alpha = 0.2) +
  theme_minimal() +
  labs(x = "Date", y = "Valeur", color = "Fenêtre", fill = "Fenêtre") + 
  theme(legend.position = "none")


############# PREDICTION #############

pred <- 12

value_snf_diff <- merged_data$values_snf_diff

fit <- Arima(value_snf_diff, order = c(1,0,2))
fc  <- forecast(fit, h = pred)

last_train_val <- tail(value_snf, 1)
pred <- fc$mean
pred_mean <- c(last_train_val, last_train_val + cumsum(pred))
fc_se <- (fc$upper[,2] - fc$mean) / 1.96
pred_var <- c(0, cumsum(fc_se^2))
upper <- pred_mean + 1.96 * sqrt(pred_var)
lower <- pred_mean - 1.96 * sqrt(pred_var)

last_date <- tail(merged_data$time, 1) 


h <- length(pred) 
time_pred <- seq.Date(
  from = last_date + months(3),  
  by   = "3 months",
  length.out = h
)
time_pred <- c(last_date, time_pred)

prevision <- data.frame(
  time  = time_pred,
  value = pred_mean,
  upper = upper,
  lower = lower
)

ggplot() +
  geom_line(aes(x = time, y = value_snf), data = merged_data, color = "black") +
  geom_line(aes(x = time, y = value), data = prevision, color = "red") +
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), data = prevision, fill = "red", alpha = 0.2) +
  theme_minimal() +
  labs(x = "Date", y = "Prévision")


ggplot() +
  geom_line(aes(x = time, y = value_snf), data = merged_data, color = "black") +
  geom_line(aes(x = time, y = value), data = prevision, color = "red") +
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), data = prevision, fill = "red", alpha = 0.2) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2014-01-30"), NA)) +
  scale_y_continuous(limits = c(60, NA)) +
  labs(x = "Date", y = "Prévision")

#############PREDICTION COVID #############

pred <- 12

value_snf_diff <- merged_data$values_snf_diff

covid_start <- 80
covid_end   <- 92
n <- 105  

xreg <- rep(0, n)
xreg[covid_start:covid_end] <- 1
xreg <- matrix(xreg, ncol = 1)
xreg_future <- matrix(0, nrow = h, ncol = 1)

fit <- Arima(value_snf_diff, order = c(1,0,1),  xreg = xreg)
fc  <- forecast(fit, h = pred, xreg = xreg_future )

last_train_val <- tail(value_snf, 1)
pred <- fc$mean
pred_mean <- c(last_train_val, last_train_val + cumsum(pred))
fc_se <- (fc$upper[,2] - fc$mean) / 1.96
pred_var <- c(0, cumsum(fc_se^2))
upper <- pred_mean + 1.96 * sqrt(pred_var)
lower <- pred_mean - 1.96 * sqrt(pred_var)

last_date <- tail(merged_data$time, 1) 


h <- length(pred) 
time_pred <- seq.Date(
  from = last_date + months(3),  # le trimestre suivant
  by   = "3 months",
  length.out = h
)
time_pred <- c(last_date, time_pred)

prevision <- data.frame(
  time  = time_pred,
  value = pred_mean,
  upper = upper,
  lower = lower
)

ggplot() +
  geom_line(aes(x = time, y = value_snf), data = merged_data, color = "black") +
  geom_line(aes(x = time, y = value), data = prevision, color = "red") +
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), data = prevision, fill = "red", alpha = 0.2) +
  theme_minimal() +
  labs(x = "Date", y = "Prévision")


ggplot() +
  geom_line(aes(x = time, y = value_snf), data = merged_data, color = "black") +
  geom_line(aes(x = time, y = value), data = prevision, color = "red") +
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), data = prevision, fill = "red", alpha = 0.2) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2014-01-30"), NA)) +
  scale_y_continuous(limits = c(60, NA)) +
  labs(x = "Date", y = "Prévision")


##################
####### DNF     ## 
##################






