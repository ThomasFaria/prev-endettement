
get_uni_time_country <- function(data) {
  
  data_transformed <- data %>%
  mutate(
    values_menages = ifelse(SNF == 0, values, 0),
    values_snf     = ifelse(SNF == 1, values, 0)
  ) %>%
  group_by(Pays, time) %>%
  summarise(
    values_menages = sum(values_menages, na.rm = TRUE),
    values_snf     = sum(values_snf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DNF = values_menages + values_snf,
    part_menage = (values_menages / DNF) * 100
    
  )
}

differencier <- function(df, colonnes = NA) {
  df <- df %>%
    mutate(across(all_of(colonnes), 
                  ~ c(NA, diff(.x, differences = 1)),
                  .names = "{.col}_diff")) %>%
    na.omit()
  
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

  
  
  