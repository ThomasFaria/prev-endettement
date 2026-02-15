

plot_evolution_internationale <- function(data, var, title = NULL, subtitle = NULL, bold_country = "France") {
  library(ggplot2)
  library(dplyr)
  var <- rlang::ensym(var)
  
  palette_pays <- c(
    "France" = "red",
    "Allemagne" = "#1f78b4",
    "Italie" = "#33a02c",
    "Espagne" = "#ff7f00",
    "Zone euro" = "#6a3d9a",
    "Royaume-Uni" = "#b15928",
    "Etats-Unis" = "#a6cee3"
  )
  
  # Convertir Pays en facteur pour garder tous les niveaux
  data <- data %>%
    mutate(
      Pays = factor(Pays, levels = names(palette_pays)),
      linewidth_tmp = ifelse(Pays == bold_country, 2.2, 0.9),
      alpha_tmp = ifelse(Pays == bold_country, 1, 0.6)
    )
  
  if (is.null(title)) title <- paste("Évolution internationale de", rlang::as_name(var))
  if (is.null(subtitle)) subtitle <- "Comparaison internationale"
  
  ggplot(data, aes(x = time, y = !!var, color = Pays, group = Pays)) +
    geom_line(aes(linewidth = linewidth_tmp, alpha = alpha_tmp)) +
    scale_color_manual(values = palette_pays) +
    scale_linewidth_identity() +
    scale_alpha_identity() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Date", y = "Valeur", color = "Pays"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}


plot_cumul_debt <- function(data, country = "France") {
  

  data_country <- data %>%
    filter(Pays == country) %>%
    as.data.frame()
  
  ggplot() +
    geom_area(
      data = data_country,
      aes(x = time, y = endettement_agent_nonfinancie_privee, fill = "SNF"),
      color = NA,
      alpha = 0.6
    ) +
    geom_area(
      data = data_country,
      aes(x = time, y = endettement_menage, fill = "Ménages"),
      color = NA,
      alpha = 0.6
    ) +
    scale_fill_manual(values = c("SNF" = "#1f78b4", "Ménages" = "#fbb4ae")) +
    labs(
      title = paste("Dette des agents non financiers en", country, "(% du PIB)"),
      subtitle = "SNF et Ménages",
      x = "Date",
      y = "Dette (% du PIB)",
      fill = "Type d'agent"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white", color = NA)
    )
}
plot_vars_moyenne_mobile <- function(df, x_var, y_vars, k = 12, titres = NULL, pays = NULL, n_diff = 0) {
  
  # Filtrer par pays si demandé
  if (!is.null(pays)) {
    df <- df %>% filter(Pays %in% pays)
  }
  
  # Supprimer les premières lignes instables en fonction des différentiations et de la MM
  df <- df %>% slice((k + n_diff):n())
  
  # Sélection des colonnes utiles
  df <- df %>% dplyr::select(all_of(c(x_var, "Pays", y_vars)))
  
  plots <- list()
  
  for (i in seq_along(y_vars)) {
    y <- y_vars[i]  
    
    # Calcul de la moyenne mobile
    df_ma <- moyenne_mobile(df, colonnes = y, k = k)
    y_ma_var <- paste0(y, "_ma") 
    
    # Définir le titre
    titre <- if (!is.null(titres) && length(titres) >= i) titres[i] else y
    
    # Plot
    p <- ggplot(df_ma, aes_string(x = x_var)) +
      geom_line(aes_string(y = y), color = "blue") +
      geom_point(aes_string(y = y), color = "blue") +
      geom_line(aes_string(y = y_ma_var), color = "red", size = 1.2, na.rm = TRUE) +
      ggtitle(titre) +
      theme_minimal()
    
    plots[[y]] <- p
  }
  
  return(plots)
}
 