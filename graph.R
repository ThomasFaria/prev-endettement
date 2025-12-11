source("R/data/retrieval_webstat.R")
library(yaml)
library(ggplot2)

secrets <- yaml::read_yaml("secrets.yaml")

series <- c(
  "CNFSI.Q.S.FR.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T", 
  "CNFSI.Q.N.DE.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.IT.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.ES.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.I9.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.GB.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.US.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T"
)

data <- get_webstat(series_keys = series, api_key = secrets$api_key)
data$Pays <- gsub(".*-\\s*|,.*", "", data$title_fr)

series2 <- c(
  "CNFSI.Q.S.FR.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T", 
  "CNFSI.Q.N.DE.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.IT.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.ES.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.I9.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.GB.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.N.US.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T"
)

data2 <- get_webstat(series_keys = series2, api_key = secrets$api_key)
data2$Pays <- gsub(".*-\\s*|,.*", "", data2$title_fr)

data$Pays <- ifelse(data$Pays == "Uni", "Royaume-Uni", data$Pays)
data$Pays <- ifelse(data$Pays == "Unis", "États-Unis", data$Pays)
data2$Pays <- ifelse(data2$Pays == "Uni", "Royaume-Uni", data2$Pays)
data2$Pays <- ifelse(data2$Pays == "Unis", "États-Unis", data2$Pays)
data2$Pays <- ifelse(data2$Pays == "Italy", "Italie", data2$Pays)

merged_data <- merge(data, data2, by = c("Pays", "time"), suffixes = c("_menages", "_snf"))
merged_data$DNF <- merged_data$values_menages + merged_data$values_snf
merged_data$part_menage <-  merged_data$values_menages / merged_data$DNF * 100



palette_pays <- c(
  "France" = "red",
  "Allemagne" = "#1f78b4",
  "Italie" = "#33a02c",
  "Espagne" = "#ff7f00",
  "Zone euro" = "#6a3d9a",
  "Royaume-Uni" = "#b15928",
  "États-Unis" = "#a6cee3"
)


plot_debt_all <- function(data) {
  ggplot() +
    geom_line(
      data = subset(data, Pays != "France"),
      aes(x = time, y = values, color = Pays, group = Pays),
      size = 1, alpha = 0.6
    ) +
    geom_line(
      data = subset(data, Pays == "France"),
      aes(x = time, y = values, color = Pays, group = Pays),
      size = 2
    ) +
    scale_color_manual(values = palette_pays) +
    labs(
      title = "Dette des ménages (% du PIB)",
      subtitle = "Comparaison internationale",
      x = "Date", y = "Dette (% du PIB)", color = "Pays"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_debt_all2 <- function(data) {
  ggplot() +
    geom_line(
      data = subset(data, Pays != "France"),
      aes(x = time, y = values, color = Pays, group = Pays),
      size = 1, alpha = 0.6
    ) +
    geom_line(
      data = subset(data, Pays == "France"),
      aes(x = time, y = values, color = Pays, group = Pays),
      size = 2
    ) +
    scale_color_manual(values = palette_pays) +
    labs(
      title = "Dette des SNF (% du PIB)",
      subtitle = "Comparaison internationale",
      x = "Date", y = "Dette (% du PIB)", color = "Pays"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_debt_all3 <- function(data) {
  ggplot() +
    geom_line(
      data = subset(data, Pays != "France"),
      aes(x = time, y = DNF, color = Pays, group = Pays),
      size = 1, alpha = 0.6
    ) +
    geom_line(
      data = subset(data, Pays == "France"),
      aes(x = time, y = DNF, color = Pays, group = Pays),
      size = 2
    ) +
    scale_color_manual(values = palette_pays) +
    labs(
      title = "Dette des agents non financiers privés (% du PIB)",
      subtitle = "Comparaison internationale",
      x = "Date", y = "Dette (% du PIB)", color = "Pays"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_debt_all(data)
plot_debt_all2(data2)
plot_debt_all3(merged_data)

merged_dataFR <- merged_data %>%
  dplyr::filter(merged_data$Pays == "France")

merged_dataFR <- as.data.frame(merged_dataFR)

CUMUL <- ggplot() +
  geom_area(data = merged_dataFR, aes(x = time, y = DNF, fill = "SNF"),
            color = NA, alpha = 0.6) +  
  geom_area(data = merged_dataFR, aes(x = time, y = values_menages, fill = "Ménages"),
            color = NA, alpha = 0.6) + 
  scale_fill_manual(values = c("SNF" = "#1f78b4", "Ménages" = "#fbb4ae")) +
  labs(
    title = "Dette des agents non financiers en France (% du PIB)",
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

CUMUL

print(merged_dataFR$part_menage)


