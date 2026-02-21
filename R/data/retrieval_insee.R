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

get_insee_advanced <- function(ids) {
  data <- get_insee(ids)
  data$Name <- NA
  
  data$Name <- ifelse(data$series_key == "011794755", "epargne2", data$Name)
  data$Name <- ifelse(data$series_key == "011794746", "RDB", data$Name)
  data$Name <- ifelse(data$series_key== "011794859", "PIB", data$Name)
  data$Name <- ifelse(data$series_key == "011794805", "DP", data$Name)
  data$Name <- ifelse(data$series_key == "011794792", "FBCF", data$Name)
  data$Name <- ifelse(data$series_key == "001656164", "defaillance", data$Name)
  data$Name <- ifelse(data$series_key == "011794733", "EBE", data$Name)
  data$Name <- ifelse(data$series_key == "001565530", "climat_affaires", data$Name)
  
  data <- data %>%
    filter(time > as.Date("1999-02-01"))
  
  data <- data %>%
    filter(time < as.Date("2025-09-01"))
  
  data2 <- data %>%
    filter(Name == "climat_affaires") %>%   
    mutate(
      time = as.Date(time),
      time = floor_date(time %m-% months(2), "quarter") %m+% months(2)
    ) %>%
    group_by(Name, time, series_key) %>%
    summarise(
      values = mean(values, na.rm = TRUE),
      .groups = "drop"
    )
  
  data <- data %>%
    filter(Name != "climat_affaires")
  
  data <- bind_rows(data, data2)
  
  data <- data %>%
    pivot_wider(
      names_from = Name,
      values_from = values
    )
  
  data <- data %>%
    group_by(time) %>%   
    summarise(
      across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  return(data)
}