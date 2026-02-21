source("script/functions/retrieval_webstat.R")
source("script/functions/retrieval_insee.R")
source("script/functions/utilitaires.R")
source("script/functions/descriptives_functions.R")
source("script/functions/ARIMA_functions.R")
source("script/collect_data_webstat&descriptive.R")

data1 <- read.csv("cache/data_webstat.csv", stringsAsFactors = FALSE)
data1 <- filter(data1, Pays == "France")
data1 <- data1 %>% dplyr::select(-Pays)
data1 <- data1 %>%
  mutate(
    time = as.Date(time),
    time = make_date(year(time), month(time), 1) 
  )


ids <- c(
  "011794755", 
  "011794746",
  "011794859",
  "011794805",
  "011794792",
  "001656164",
  "011794733",
  "001565530"
)

data <- get_insee_advanced(ids)
data <- merge(data, data1, by = "time", all = TRUE)

write.csv(data, "cache/data_insee_bdf.csv", row.names = FALSE)







