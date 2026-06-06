#' Wczytuje i łączy dane sprzedażowe Kaggle
#'
#' Funkcja wczytuje pliki train.csv, stores.csv oraz holidays_events.csv,
#' a następnie łączy je w jedną ramkę danych.
#'
#' @param train_path Ścieżka do pliku train.csv
#' @param stores_path Ścieżka do pliku stores.csv
#' @param holidays_path Ścieżka do pliku holidays_events.csv
#' @return Połączona ramka danych (tibble)
#' @export
#' @examples
#' # load_sales_data("train.csv", "stores.csv", "holidays_events.csv")
load_sales_data <- function(train_path, stores_path, holidays_path) {

  train_data <- readr::read_csv(train_path, show_col_types = FALSE)
  stores_data <- readr::read_csv(stores_path, show_col_types = FALSE)
  holidays_data <- readr::read_csv(holidays_path, show_col_types = FALSE)

  joined_data <- dplyr::left_join(train_data, stores_data, by = "store_nbr")
  joined_data <- dplyr::left_join(joined_data, holidays_data, by = c("date" = "date"))

  return(joined_data)
}
