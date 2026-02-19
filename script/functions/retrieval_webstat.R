get_webstat <- function(series_keys, api_key) {
  
  url <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json"
  
  # Construire la clause WHERE pour toutes les séries
  where_clause <- paste0(
    "series_key IN (",
    paste(sprintf("'%s'", series_keys), collapse = ", "),
    ")"
  )
  
  params <- list(
    select = "title_fr,series_key,time_period_end,obs_value",
    where = where_clause
  )
  
  # Requête API avec retry
  response <- httr::RETRY(
    "GET",
    url,
    query = params,
    httr::add_headers(Authorization = paste("Apikey", api_key)),
    terminate_on = c(404),
    times = 20
  )
  
  if (httr::status_code(response) != 200) {
    stop(sprintf("Webstat API request failed [HTTP %s]", httr::status_code(response)))
  }
  
  # Parser le JSON
  raw_data <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(raw_data, flatten = TRUE)
  
  # Forcer les colonnes essentielles même si elles manquent
  required_cols <- c("title_fr", "series_key", "time_period_end", "obs_value")
  for (col in required_cols) {
    if (!(col %in% names(parsed))) {
      parsed[[col]] <- NA
    }
  }
  
  # Convertir en date et numérique
  parsed$time_period_end <- as.Date(parsed$time_period_end)
  suppressWarnings(parsed$obs_value <- as.numeric(parsed$obs_value))
  
  # Garder uniquement les lignes valides
  parsed <- parsed %>%
    filter(!is.na(series_key))
  
  # Trier et renommer
  df <- parsed %>%
    arrange(series_key, time_period_end) %>%
    rename(
      time = time_period_end,
      values = obs_value
    ) %>%
    as_tibble()
  
  return(df)
}

get_webstat_with_country <- function(series_keys, api_key) {

  data <- get_webstat(series_keys, api_key)
  data$title_fr <- ifelse(data$title_fr == "France, Taux d'inflation", "Taux d'inflation - France, taux mensuelle", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Taux de chômage au sens du BIT - Ensemble - France métropolitaine - Données CVS", "Taux de chomage - France, taux trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Taux d'épargne des ménages, trimestre, France, en % du RDB, cvs-cjo", "Taux d'epargne des ménages - France, taux trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Octroi de crédits à l'habitat", "Octroi crédit à l'habitat - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Taux de l'emprunt phare à 10 ans - France", "Taux de l'emprunt phare à 10 ans - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Crédits nouveaux à l'habitat des particuliers, à plus d'un an, taux d'intérêt annuel", "Crédit > 1 an - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Demande de crédits à l'habitat", "Demande crédit - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Coût de financement des SNF, taux en %", "Coût de financement des SNF - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Durée moyenne des crédits immobiliers aux particuliers", "Durée moyenne des crédits immobiliers aux particuliers - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Moyenne des taux effectifs (sens étroit) des crédits immobiliers aux particuliers", "Moyenne des taux effectifs des crédits immobiliers aux particuliers - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Salaires négociés France", "Salaires négociés - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Indice des prix des logements anciens - France métropolitaine - Ensemble - Base 100 en moyenne annuelle 2015 - Série brute", "Indice des prix des logements anciens - France, Base 100 en moyenne annuelle 2015", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Indicateur ajusté et lissé mensuel des anticipations d'inflation françaises issu de la presse", "Indicateur ajusté et lissé mensuel des anticipations d'inflation - France, issu de la presse", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Agrégats monétaires Zone euro, M3 [moyenne mobile 3 mois du glissement annuel] (CVS-CJO)", "M3 zone euro - France, trimestrielle", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Demande de crédits des entreprises", "Demande de crédits des entreprises - France, trimestriels", data$title_fr)
  data$title_fr <- ifelse(data$title_fr == "Octroi de crédits aux entreprises", "Octroi de crédits aux entreprises - France, trimestriels", data$title_fr)
  
  
  data$Pays <- sub("^[^-]+-\\s*([^,]+),.*", "\\1", data$title_fr)
  data$Pays <- ifelse(data$Pays == "Italy", "Italie", data$Pays)
  data$first_part <- sub("\\s*-.*", "", data$title_fr)
  data$SNF <- ifelse(grepl("Dette des SNF", data$first_part), 1, 0)
  data$Menages <- ifelse(grepl("Dette des ménages", data$first_part), 1, 0)
  data$Inflation <- ifelse(grepl("inflation", data$first_part), 1, 0)
  data$ind_chomage <- ifelse(grepl("chomage", data$first_part), 1, 0)
  data$ind_epargne <- ifelse(grepl("epargne", data$first_part), 1, 0)
  data$ind_octroi_credit <- ifelse(grepl("Octroi crédit", data$first_part), 1, 0)
  data$ind_demande_credit <- ifelse(grepl("Demande crédit", data$first_part), 1, 0)
  data$ind_credit_aplusunan <- ifelse(grepl("Crédit > 1 an", data$first_part), 1, 0)
  data$ind_Taux_long <- ifelse(grepl("phare", data$title_fr), 1, 0)
  data$ind_Taux_immo <- ifelse(grepl("taux effectifs", data$title_fr), 1, 0)
  data$ind_Duree_immo <- ifelse(grepl("Durée", data$title_fr), 1, 0)
  data$ind_Taux_snf <- ifelse(grepl("Coût de financement des SNF", data$title_fr), 1, 0)
  data$ind_prix_logement <- ifelse(grepl("prix des logements", data$title_fr), 1, 0)
  data$ind_salaires <- ifelse(grepl("Salaires", data$title_fr), 1, 0)
  data$ind_M3 <- ifelse(grepl("M3", data$title_fr), 1, 0)
  data$ind_inflation_anticipée <- ifelse(grepl("anticipations", data$title_fr), 1, 0)
  data$ind_M3 <- ifelse(grepl("M3", data$title_fr), 1, 0)
  data$ind_octroi_credit_snf <- ifelse(grepl("Octroi de crédits aux entreprises", data$title_fr), 1, 0)
  data$ind_demande_credit_snf <- ifelse(grepl("Demande de crédits des entreprises", data$title_fr), 1, 0)
  
  data <- data %>%
    dplyr::select(-first_part)
  
  return(data)
}
