# Requires: xml2, purrr, tibble, dplyr
# Charge les helpers partages (parse_time, etc.)
source(here::here("R/utils/helpers.R"))

get_ecb <- function(series_keys, start_period = NULL, end_period = NULL) {
  base_url <- "https://data-api.ecb.europa.eu/service/data"

  parse_sdmx_xml <- function(resp_body, series_key) {
    doc <- xml2::read_xml(resp_body)
    ns <- c(
      message = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message",
      generic = "http://www.sdmx.org/resources/sdmx/schemas/v2_1/data/generic",
      generic_alt = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/data/generic"
    )

    # Find all Series nodes (supporting either namespace alias)
    series_nodes <- xml2::xml_find_all(doc, ".//generic:Series | .//generic_alt:Series", ns = ns)
    if (length(series_nodes) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(series_nodes, function(s_node) {
      # SeriesKey dims
      sk <- xml2::xml_find_all(
        s_node,
        ".//generic:SeriesKey/generic:Value | .//generic_alt:SeriesKey/generic_alt:Value",
        ns = ns
      )
      dim_ids <- xml2::xml_attr(sk, "id")
      dim_vals <- xml2::xml_attr(sk, "value")
      dims_lst <- as.list(stats::setNames(dim_vals, dim_ids))

      # Observations
      obs <- xml2::xml_find_all(s_node, ".//generic:Obs | .//generic_alt:Obs", ns = ns)
      if (length(obs) == 0) {
        return(tibble::tibble())
      }

      time <- xml2::xml_attr(
        xml2::xml_find_all(s_node, ".//generic:Obs/generic:ObsDimension | .//generic_alt:Obs/generic_alt:ObsDimension", ns = ns),
        "value"
      )
      val <- suppressWarnings(as.numeric(
        xml2::xml_attr(
          xml2::xml_find_all(s_node, ".//generic:Obs/generic:ObsValue | .//generic_alt:Obs/generic_alt:ObsValue", ns = ns),
          "value"
        )
      ))

      tibble::tibble(
        time = time,
        values = val,
        series_key = series_key
      ) |>
        dplyr::bind_cols(if (length(dims_lst)) tibble::as_tibble_row(dims_lst) else tibble::tibble()) |>
        dplyr::arrange(time)
    })
  }

  fetch_one <- function(series_key) {
    # Convert "BLS.Q.FR..." to "BLS/Q.FR..." for the API URL
    url_key <- sub("\\.", "/", series_key)
    url <- glue::glue("{base_url}/{url_key}")
    resp <- httr2::request(url) |>
      httr2::req_url_query(
        startPeriod = start_period,
        endPeriod   = end_period
      ) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200) {
      stop("Failure to get data. Status code: ", httr2::resp_status(resp), call. = FALSE)
    }

    ct <- tolower(httr2::resp_header(resp, "content-type") %||% "")
    if (!grepl("xml", ct)) {
      stop("Unexpected Content-Type (expected XML/SDMX). Got: ", ct, call. = FALSE)
    }

    parse_sdmx_xml(httr2::resp_body_string(resp), series_key)
  }

  series_keys |>
    purrr::map(fetch_one) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      time = as.Date(parse_time(time)),
      country = REF_AREA,
    ) |>
    dplyr::select(time, series_key, country, values)
}

# Operateur null-coalesce
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
