
get_uni_time_country <- function(data) {
  
  data_transformed <- data %>%
    mutate(
      endettement_menage    = ifelse(Menages == 1, values, 0),
      endettement_snf        = ifelse(SNF == 1, values, 0),
      inflation         = ifelse(Inflation == 1, values, 0),
      chomage           = ifelse(ind_chomage == 1, values, 0),
      epargne           = ifelse(ind_epargne == 1, values, 0),
      octroi_credit     = ifelse(ind_octroi_credit == 1, values, 0),
      demande_credit    = ifelse(ind_demande_credit == 1, values, 0),
      credit_aplusunan  = ifelse(ind_credit_aplusunan == 1, values, 0),
      Taux_long         = ifelse(ind_Taux_long == 1, values, 0),
      Taux_immo         = ifelse(ind_Taux_immo == 1, values, 0),
      Duree_immo        = ifelse(ind_Duree_immo == 1, values, 0),
      Taux_snf          = ifelse(ind_Taux_snf == 1, values, 0),
      prix_logement     = ifelse(ind_prix_logement == 1, values, 0),
      salaires          = ifelse(ind_salaires == 1, values, 0),
      M3                = ifelse(ind_M3 == 1, values, 0), 
      inflation_anticipée = ifelse(ind_inflation_anticipée == 1, values, 0)
    ) %>%
    group_by(Pays, time) %>%
    summarise(
      endettement_menage = sum(endettement_menage, na.rm = TRUE),
      endettement_snf     = sum(endettement_snf, na.rm = TRUE),
      inflation      = sum(inflation, na.rm = TRUE),
      chomage        = sum(chomage , na.rm = TRUE),
      epargne        = sum(epargne, na.rm = TRUE),
      octroi_credit    = sum(octroi_credit, na.rm = TRUE),
      demande_credit   = sum(demande_credit, na.rm = TRUE),
      credit_aplusunan = sum(credit_aplusunan, na.rm = TRUE),
      Taux_long      = sum(Taux_long, na.rm = TRUE),
      Taux_immo         = sum(Taux_immo, na.rm = TRUE),
      Duree_immo        = sum(Duree_immo, na.rm = TRUE),
      Taux_snf          = sum(Taux_snf, na.rm = TRUE),
      prix_logement     = sum(prix_logement, na.rm = TRUE),
      salaires          = sum( salaires , na.rm = TRUE),
      M3                = sum( M3 , na.rm = TRUE),
      inflation_anticipée = sum(inflation_anticipée, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # Début de la nouvelle partie pour trimestrialiser
    mutate(
      year    = year(time),
      quarter = quarter(time)
    ) %>%
    group_by(Pays, year, quarter) %>%
    summarise(
      endettement_menage   = sum(endettement_menage, na.rm = TRUE),
      endettement_snf       = sum(endettement_snf, na.rm = TRUE),
      chomage          = sum(chomage, na.rm = TRUE),
      epargne          = sum(epargne, na.rm = TRUE),
      octroi_credit    = sum(octroi_credit, na.rm = TRUE),
      demande_credit   = sum(demande_credit, na.rm = TRUE),
      credit_aplusunan = mean(credit_aplusunan, na.rm = TRUE),
      inflation        = mean(inflation, na.rm = TRUE), # moyenne trimestrielle
      Taux_long        = mean(Taux_long, na.rm = TRUE), # moyenne trimestrielle
      Taux_immo         = sum(Taux_immo, na.rm = TRUE),
      Duree_immo        = sum(Duree_immo, na.rm = TRUE),
      Taux_snf          = mean(Taux_snf, na.rm = TRUE),
      prix_logement     = sum(prix_logement, na.rm = TRUE),
      salaires          = sum( salaires , na.rm = TRUE),
      M3                = mean( M3 , na.rm = TRUE),
      inflation_anticipée = sum(inflation_anticipée, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      endettement_agent_nonfinancie_privee = endettement_snf + endettement_menage,
      part_menage = (endettement_menage / endettement_agent_nonfinancie_privee) * 100,
      # Date représentative : dernier jour du trimestre
      time = as.Date(paste(year, quarter * 3, "01", sep = "-")) %>% 
        ceiling_date(unit = "month") - days(1)
    ) %>%
    filter(time >= as.Date("1999-03-31")) %>%
    filter(time <= as.Date("2025-06-30")) %>%
    dplyr::select(Pays, time, endettement_menage, endettement_snf, inflation, endettement_agent_nonfinancie_privee, part_menage, epargne, octroi_credit, demande_credit, credit_aplusunan,  Taux_long, chomage
                  , Taux_immo, Taux_snf, prix_logement, M3, salaires, inflation_anticipée, Duree_immo)
  
  data_transformed <- data_transformed %>%
    mutate(
      epargne = ifelse(epargne == 0, NA, epargne),
      octroi_credit  = ifelse(time < as.Date("2003-03-31"), NA, octroi_credit),
      demande_credit  = ifelse(time < as.Date("2002-12-31"), NA, demande_credit),
      credit_aplusunan  = ifelse(time < as.Date("2003-03-31"), NA, credit_aplusunan),
      Taux_immo  = ifelse(time < as.Date("2004-03-31"), NA, Taux_immo),
      salaires  = ifelse(time < as.Date("2006-01-31"), NA, salaires),
      Duree_immo  = ifelse(time < as.Date("2004-01-31"), NA, Duree_immo),
    inflation_anticipée  = ifelse(time < as.Date("2004-01-31"), NA, inflation_anticipée)
    )
  
  return(data_transformed)
}
library(tidyr)

differencier <- function(df, colonnes = NA) {
  df <- df %>%
    mutate(across(all_of(colonnes), 
                  ~ c(NA, diff(as.numeric(.x))),   # <-- conversion en numeric ici
                  .names = "{.col}_diff"))
  
  # Supprimer uniquement les NA des colonnes différenciées
  df <- df %>% drop_na(all_of(paste0(colonnes, "_diff")))
  
  return(df)
}
  

moyenne_mobile <- function(df, colonnes, k = 12, suffixe = "_ma") {
  df <- df %>%
    mutate(across(all_of(colonnes),
                  ~ rollmean(.x, k = k, fill = NA, align = "right"),
                  .names = "{.col}{suffixe}"))
  return(df)
}

calcul_trimestres_par_an <- function(data, colonne_date) {
  data[[colonne_date]] <- as.Date(data[[colonne_date]])
 
  dates_unique <- unique(data[[colonne_date]])

  df_trimestre <- data.frame(date = dates_unique) %>%
    mutate(
      year = year(date),
      quarter = quarter(date)
    )

  trimestre_par_an <- df_trimestre %>%
    group_by(year) %>%
    summarise(n_trimestres = n(), .groups = "drop") %>%
    arrange(year)

  return(as.data.frame(trimestre_par_an))
}

  
  
  