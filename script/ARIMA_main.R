source("script/functions/retrieval_webstat.R")
source("script/functions/utilitaires.R")
source("script/functions/descriptives_functions.R")
source("script/functions/ARIMA_fonctions.R")


library(portes)
library(astsa)
library(dplyr)
library(zoo) 
library(forecast)
library(tseries)
library(lubridate)
library(xtable)
library(yaml)
library(ggplot2)

secrets <- yaml::read_yaml("secret.yaml")

series <- c(
  "CNFSI.Q.S.FR.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T", 
  "CNFSI.Q.N.DE.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.IT.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.ES.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.I9.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.GB.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.US.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.S.FR.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T", 
  "CNFSI.Q.N.DE.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.IT.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.ES.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.I9.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.GB.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.US.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T"
)

data <- get_webstat_with_country(series_keys = series, api_key = secrets$api_key)
data <- get_uni_time_country(data)

any(is.na(data)) 

trimestre_par_an_df <- calcul_trimestres_par_an(data, "time")
print(trimestre_par_an_df)

plot_evolution_internationale(data, DNF, 
                              "Dette totale des agents privés non financiers en (% du PIB)",
                              subtitle = "Comparaison internationale", bold_country = "France")

plot_evolution_internationale(data, values_snf, 
                              "Dette totale des sociétés non financières en (% du PIB)",
                              subtitle = "Comparaison internationale", bold_country = "France")

plot_evolution_internationale(data, values_menages, 
                              "Dette totale des ménages en (% du PIB)",
                              subtitle = "Comparaison internationale", bold_country = "France")


plot_cumul_debt(data, country = "France")


######################
#STATIONNARITE.     ##
######################

stationarity_tests(data, c("values_menages", "values_snf","DNF"), countries = c("France", "Allemagne"))

data <- differencier(data,c("values_menages", "values_snf","DNF"))
plot_vars_moyenne_mobile(data, "time", c("values_menages_diff","values_snf_diff") , k = 12, titre = c("Ménages différencié avec moyenne mobile de 3 ans", "SNF différencié avec moyenne mobile de 3 ans"), pays = "France")
stationarity_tests(data, c("values_menages_diff", "values_snf_diff","DNF_diff"), countries = c("France", "Allemagne"))

data <- differencier(data,c("values_menages_diff"))
stationarity_tests(data, c("values_menages_diff_diff", "values_snf_diff","DNF_diff"), countries = c("France", "Allemagne"))
plot_vars_moyenne_mobile(data, "time", c("values_menages_diff_diff") , k = 12, titre = "Menages double différencié avec moyenne mobile de 3 ans", pays = "France")

#############################################################
# Vérification Autocorrelation et Partial Autocorrelation ###
#############################################################

plot_acf_pacf(country = "France", data, var_name = "values_snf_diff",nom ="SNF", ylim = c(-0.8, 0.8))


###############
# AIC et BIC  #
###############

summary_table <- test_arima_models_aic_bic(data = data , country = "France", var_name = "values_snf_diff", p_max = 3, d = 1, q_max = 3)
summary_table

latex_code <- print(xtable(summary_table), include.rownames = FALSE)

######################
#MODEL ARIMA        ##              
######################

############# SNF SANS COVID ###############


res_no_covid <- rolling_arima_errors(data,
                     p_max = 3,
                     q_max = 3,
                     var_name = "values_snf",
                     d = 1,
                     country = "France",
                     train_size = 60,
                     test_size = 12,
                     step = 4,
                     covid_windows = c("window_77", "window_81", "window_85"),
                     covid = FALSE,
                     covid_start = NULL,
                     covid_end = NULL)

print(res_no_covid)
errors_no_covid  <- res_no_covid$errors
summary_no_covid <- res_no_covid$summary_table

############# SNF AVEC COVID ###############


#### CHOC TRANSITOIRE ####
res_covid <- rolling_arima_errors(data,
                     p_max = 3,
                     q_max = 3,
                     var_name = "values_snf",
                     d = 1,
                     country = "France",
                     train_size = 60,
                     test_size = 12,
                     step = 4,
                     covid_windows = c("window_77", "window_81", "window_85"),
                     covid = TRUE,
                     covid_start = 80,
                     covid_end = 92)

print(res_covid)
errors_covid  <- res_covid$errors
summary_covid <- res_covid$summary_table

########### COMPARAISON AVEC/SANS COVID###########

comparison_table <- compare_arima_errors(
  errors_no_covid,
  errors_covid,
  covid_start_window = 81
)

print(comparison_table)

#####################
## MODELE 111.     ##
#####################

############# GRAPH DIFFERENTE PREDICTION #############

arima_rolling_plot(data, var_name = "values_snf", country = "France",
                   train_size = 60, test_size = 8, step = 4,
                   p = 1, d = 1, q = 1)

check_residuals_manual(data, var_name = "values_snf", country = "France",
                       p = 1, d = 1, q = 1, lags = 20)

############# PREDICTION #############

arima_forecast_plot(
  data = data,        
  var_name = "values_snf",  
  country = "France",        
  pred = 12,                  
  p = 1, d = 1, q = 1,       
  covid = "none",            
  start_plot = as.Date("2014-01-01") 
)

#############PREDICTION COVID #############

arima_forecast_plot(
  data = data,        
  var_name = "values_snf",  
  country = "France",        
  pred = 12,                  
  p = 1, d = 1, q = 1,       
  covid = "covid",            
  start_plot = as.Date("2014-01-01") 
)

arima_forecast_plot(
  data = data,        
  var_name = "values_snf",  
  country = "France",        
  pred = 12,                  
  p = 1, d = 1, q = 1,       
  covid = "both",            
  start_plot = as.Date("2014-01-01") 
)







