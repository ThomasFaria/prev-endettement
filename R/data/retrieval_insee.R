parse_time <- function(x) {
    dplyr::case_when(
        # Quarterly format like "1999-Q1"
        stringr::str_detect(x, "Q[1-4]$") ~ {
            year <- stringr::str_sub(x, 1, 4)
            qtr <- stringr::str_sub(x, 6, 7)
            month <- dplyr::recode(qtr, Q1 = "03", Q2 = "06", Q3 = "09", Q4 = "12")
            paste(year, month, "01", sep = "-")
        },
        # Monthly format like "1999-05"
        stringr::str_detect(x, "^\\d{4}-\\d{2}$") ~ paste0(x, "-01"),
        TRUE ~ x
    )
}

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
