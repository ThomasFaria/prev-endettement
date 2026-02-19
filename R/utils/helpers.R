#' Convertit les formats de temps en dates ISO
#'
#' Gere les formats trimestriels (1999-Q1), bimestriels (1999-B1),
#' semestriels (1999-S1), mensuels (1999-05) et annuels (1999) et
#' les convertit en format date ISO (YYYY-MM-DD).
#'
#' @param x Vecteur de caracteres contenant les dates a convertir
#' @return Vecteur de caracteres au format YYYY-MM-DD
#' @examples
#' parse_time("2023-Q1") # "2023-03-01"
#' parse_time("2023-B1") # "2023-02-01"
#' parse_time("2023-S1") # "2023-06-01"
#' parse_time("2023-05") # "2023-05-01"
#' parse_time("2023") # "2023-01-01"
parse_time <- function(x) {
  dplyr::case_when(
    # Format trimestriel comme "1999-Q1"
    stringr::str_detect(x, "Q[1-4]$") ~ {
      year <- stringr::str_sub(x, 1, 4)
      qtr <- stringr::str_sub(x, 6, 7)
      month <- dplyr::recode(qtr, Q1 = "03", Q2 = "06", Q3 = "09", Q4 = "12")
      paste(year, month, "01", sep = "-")
    },
    # Format bimestriel comme "1999-B1"
    stringr::str_detect(x, "B[1-6]$") ~ {
      year <- stringr::str_sub(x, 1, 4)
      bim <- stringr::str_sub(x, 6, 7)
      month <- dplyr::recode(bim, B1 = "02", B2 = "04", B3 = "06", B4 = "08", B5 = "10", B6 = "12")
      paste(year, month, "01", sep = "-")
    },
    # Format semestriel comme "1999-S1"
    stringr::str_detect(x, "S[1-2]$") ~ {
      year <- stringr::str_sub(x, 1, 4)
      sem <- stringr::str_sub(x, 6, 7)
      month <- dplyr::recode(sem, S1 = "06", S2 = "12")
      paste(year, month, "01", sep = "-")
    },
    # Format mensuel comme "1999-05"
    stringr::str_detect(x, "^\\d{4}-\\d{2}$") ~ paste0(x, "-01"),
    # Format annuel comme "1999"
    stringr::str_detect(x, "^\\d{4}$") ~ paste0(x, "-01-01"),
    TRUE ~ x
  )
}
