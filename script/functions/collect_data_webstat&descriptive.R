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
  "CNFSI.Q.N.US.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "ICP.M.FR.N.000000.4.ANR", 
  "FM.M.FR.EUR.FR2.BB.FR10YT_RR.YLD",
  "STS.Q.FR.S.UNEH.RTT000.4.000",
  "CNFSI.Q.Y.FR.W0.S1M.S1.N.B.B8G._Z._Z._Z.XDC_R_B6G_S1M._T.S.V.N._T",
  "BLS.Q.FR.ALL.Z.H.H.B3.ST.S.FNET",
  "BLS.Q.FR.ALL.Z.H.H.B3.ZZ.D.FNET",
  "MIR1.M.FR.B.A22.K.R.A.2254U6.EUR.N",
  "MIR1.Q.FR.R.A22FRX.A.R.A.2254FR.EUR.N",
  "MIR1.Q.FR.R.A22FRX.A.D.A.2254FR.EUR.N",
  "MIR1.M.FR.B.AT0.A.R.A.2240U6.EUR.N",
  "STS.Q.FR.N.INWR.000000.2.ANR",
  "BSI.M.U2.Y.V.M30.X.I.U2.2300.Z01.V",
  "RPP.Q.FR.N.ED.00.1.00",
  "PAI.M.FR.N.PR._Z.INPR03.TX"
)

data <- get_webstat_with_country(series_keys = series, api_key = secrets$api_key)
unique(data$title_fr)

data <- get_uni_time_country(data)
a <- subset(data, Pays == "France")

trimestre_par_an_df <- calcul_trimestres_par_an(data, "time")
print(trimestre_par_an_df)

plot_evolution_internationale(data, endettement_agent_nonfinancie_privee, 
                              "Dette totale des agents privés non financiers en (% du PIB)",
                              subtitle = "Comparaison internationale", bold_country = "France")

plot_evolution_internationale(data, endettement_snf, 
                              "Dette totale des sociétés non financières en (% du PIB)",
                              subtitle = "Comparaison internationale", bold_country = "France")

plot_evolution_internationale(data, endettement_menage, 
                              "Dette totale des ménages en (% du PIB)",
                              subtitle = "Comparaison internationale", bold_country = "France")


plot_cumul_debt(data, country = "France")

if (!dir.exists("cache")) dir.create("cache")
write.csv(data, file = "cache/data_webstat.csv", row.names = FALSE)



