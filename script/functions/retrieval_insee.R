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
