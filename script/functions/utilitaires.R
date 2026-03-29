safe_mean <- function(x) {
  if(all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

get_uni_time_country <- function(data) {
  
  data_transformed <- data %>%
    mutate(
      endettement_menage    = ifelse(Menages == 1, values, NA),
      endettement_snf        = ifelse(SNF == 1, values, NA),
      inflation         = ifelse(Inflation == 1, values, NA),
      chomage           = ifelse(ind_chomage == 1, values, NA),
      epargne           = ifelse(ind_epargne == 1, values, NA),
      octroi_credit     = ifelse(ind_octroi_credit == 1, values, NA),
      demande_credit    = ifelse(ind_demande_credit == 1, values, NA),
      octroi_credit_snf     = ifelse(ind_octroi_credit_snf == 1, values, NA),
      demande_credit_snf   = ifelse(ind_demande_credit_snf == 1, values, NA),
      credit_aplusunan  = ifelse(ind_credit_aplusunan == 1, values, NA),
      Taux_long         = ifelse(ind_Taux_long == 1, values, NA),
      Taux_immo         = ifelse(ind_Taux_immo == 1, values, NA),
      Duree_immo        = ifelse(ind_Duree_immo == 1, values, NA),
      Taux_snf          = ifelse(ind_Taux_snf == 1, values, NA),
      prix_logement     = ifelse(ind_prix_logement == 1, values, NA),
      salaires          = ifelse(ind_salaires == 1, values, NA),
      M3                = ifelse(ind_M3 == 1, values, NA), 
      inflation_anticipée = ifelse(ind_inflation_anticipée == 1, values, NA),
      EURIBOR = ifelse(ind_EURIBOR == 1, values, NA)
    ) %>%
    mutate(
      year    = year(time),
      quarter = quarter(time)
    ) %>%
    group_by(Pays, year, quarter) %>%
    summarise(
      endettement_menage   = safe_mean(endettement_menage),
      endettement_snf       = safe_mean(endettement_snf),
      chomage          = safe_mean(chomage),
      epargne          = safe_mean(epargne),
      octroi_credit    = safe_mean(octroi_credit),
      demande_credit   = safe_mean(demande_credit),
      octroi_credit_snf    = safe_mean(octroi_credit_snf),
      demande_credit_snf   = safe_mean(demande_credit_snf),
      credit_aplusunan = safe_mean(credit_aplusunan),
      inflation        = safe_mean(inflation), # moyenne trimestrielle
      Taux_long        = safe_mean(Taux_long), # moyenne trimestrielle
      Taux_immo         = safe_mean(Taux_immo),
      Duree_immo        = safe_mean(Duree_immo),
      Taux_snf          = safe_mean(Taux_snf),
      prix_logement     = safe_mean(prix_logement),
      salaires          = safe_mean( salaires),
      M3                = safe_mean( M3),
      inflation_anticipée = safe_mean(inflation_anticipée),
      EURIBOR = safe_mean(EURIBOR),
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
                  ,Taux_immo, Taux_snf, prix_logement, M3, salaires, inflation_anticipée, Duree_immo, octroi_credit_snf, demande_credit_snf, EURIBOR)

  data_transformed <- data_transformed %>%
    mutate(
      epargne = ifelse(epargne == 0, NA, epargne),
      octroi_credit  = ifelse(time < as.Date("2002-12-31"), NA, octroi_credit),
      demande_credit  = ifelse(time < as.Date("2002-12-31"), NA, demande_credit),
      octroi_credit_snf  = ifelse(time < as.Date("2002-12-31"), NA, octroi_credit_snf),
      demande_credit_snf  = ifelse(time < as.Date("2002-12-31"), NA, demande_credit_snf),
      credit_aplusunan  = ifelse(time < as.Date("2003-03-31"), NA, credit_aplusunan),
      Taux_immo  = ifelse(time < as.Date("2004-03-31"), NA, Taux_immo),
      salaires  = ifelse(time < as.Date("2006-01-31"), NA, salaires),
      Duree_immo  = ifelse(time < as.Date("2004-01-31"), NA, Duree_immo),
    inflation_anticipée  = ifelse(time < as.Date("2004-01-31"), NA, inflation_anticipée),
    Taux_snf  = ifelse(time < as.Date("2010-12-31"), NA, Taux_snf)
    )
  
  
  return(data_transformed)
}


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

  
  
  