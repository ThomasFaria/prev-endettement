get_webstat <- function(
  series_keys,
  api_key
) {
  url <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json"

  where_clause <- paste0(
    "series_key IN (",
    paste(sprintf("'%s'", series_keys), collapse = ", "),
    ")"
  )

  params <- list(
    select = "title_fr,series_key,time_period_end,obs_value",
    where = where_clause
  )

  response <- httr::RETRY(
    "GET",
    url,
    query = params,
    httr::add_headers(Authorization = paste("Apikey", api_key)),
    terminate_on = c(404),
    times = 20
  )

  if (httr::status_code(response) != 200) {
    stop(sprintf(
      "Webstat API request failed [HTTP %s]",
      httr::status_code(response)
    ))
  }

  # Parse JSON
  raw_data <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(raw_data, flatten = TRUE)

  # Ensure expected fields exist
  if (!all(c("title_fr", "series_key", "time_period_end", "obs_value") %in% names(parsed))) {
    stop("Unexpected API response format.")
  }

  parsed$time_period_end <- as.Date(parsed$time_period_end)

  # Convert values to numeric if possible
  suppressWarnings(
    parsed$obs_value <- as.numeric(parsed$obs_value)
  )

  # Ensure series_key follows the input order
  parsed$series_key <- factor(parsed$series_key, levels = series_keys)

  df <- parsed |>
    dplyr::arrange(series_key, time_period_end) |>
    dplyr::rename(
      time = time_period_end,
      values = obs_value
    ) |>
    tibble::as_tibble(key = "series_key", index = "time")

  return(df)
}
