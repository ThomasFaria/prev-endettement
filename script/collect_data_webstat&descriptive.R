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

if (!dir.exists("cache")) dir.create("cache")
write.csv(data, file = "cache/data_webstat.csv", row.names = FALSE)



