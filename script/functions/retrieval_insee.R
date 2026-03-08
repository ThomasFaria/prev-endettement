# Charge les helpers partages (parse_time, etc.)
source(here::here("R/utils/helpers.R"))

get_insee <- function(ids) {
  purrr::map_dfr(ids, \(id) {
    url <- sprintf("https://bdm.insee.fr/series/sdmx/data/SERIES_BDM/%s?", id)
    
    httr2::request(url) |>
      httr2::req_perform() |>
      httr2::resp_body_xml() |>
      xml2::xml_find_all("//Obs") |>
      purrr::map_dfr(\(obs) {
        xml2::xml_attrs(obs) |>
          as.list() |>
          tibble::as_tibble()
      }) |>
      dplyr::transmute(
        time = as.Date(parse_time(TIME_PERIOD)),
        series_key = id,
        values = as.numeric(OBS_VALUE)
      ) |>
      tidyr::drop_na(time)
  })
}

get_insee_advanced <- function(ids){
  
 data <- get_insee(ids)
 data$name <- NA
 
 data$name <- ifelse(data$series_key == "011794755", "epargne2", data$name)
 data$name <- ifelse(data$series_key == "011794746", "RDB", data$name)
 data$name <- ifelse(data$series_key == "011794859", "PIB", data$name)
 data$name <- ifelse(data$series_key == "011794805", "DP", data$name)
 data$name <- ifelse(data$series_key == "011794792", "FBCF", data$name)
 data$name <- ifelse(data$series_key == "001656164", "defaillances", data$name)
 data$name <- ifelse(data$series_key == "011794733", "EBE", data$name)
 data$name <- ifelse(data$series_key == "001565530", "climat_affaires", data$name)

 
 data <- data %>%
     tidyr::pivot_wider(
       id_cols = time,
     names_from = name,
     values_from = values
   )

 data <- data %>%
   filter(time >= as.Date("1999-03-01") & time <= as.Date("2025-06-01"))
 
 return(data)
}

a <- get_insee_advanced(c("001656164","011794733"))

View(a)
