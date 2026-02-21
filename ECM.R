source("script/functions/retrieval_webstat.R")
source("script/functions/retrieval_insee.R")
source("script/functions/utilitaires.R")
source("script/functions/descriptives_functions.R")
source("script/functions/ARIMA_functions.R")
source("script/collect_data_webstat&descriptive.R")
source("script/ARIMA_main.R")

data1 <- read.csv("cache/data_webstat.csv", stringsAsFactors = FALSE)
data1 <- filter(data1, Pays == "France")

data2 <- 
