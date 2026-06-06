#' Kompleksowa analiza sprzedaży dla wybranego sklepu
#'
#' Funkcja wyższego rzędu: filtruje dane dla wybranego miasta i/lub typu sklepu,
#' a następnie oblicza metryki i generuje wykres.
#'
#' @param data Ramka danych z połączonymi informacjami o sprzedaży i sklepach
#' @param filter_city Opcjonalna nazwa miasta do przefiltrowania (ciąg znaków)
#' @param filter_type Opcjonalny typ sklepu do przefiltrowania (ciąg znaków)
#' @return Lista zawierająca dwa elementy: 'metryki' (data.frame) oraz 'wykres' (ggplot)
#' @export
#' @examples
#' # wyniki <- sales_ts_logic(dane, filter_city = "Quito")
#' # wyniki$metryki
#' # print(wyniki$wykres)
sales_ts_logic <- function(data, filter_city = NULL, filter_type = NULL) {

  filtered_data <- data

  if (!is.null(filter_city)) {
    filtered_data <- dplyr::filter(filtered_data, city == filter_city)
  }

  if (!is.null(filter_type)) {
    filtered_data <- dplyr::filter(filtered_data, type == filter_type)
  }

  # Obliczanie metryk za pomocą naszej innej funkcji
  metryki <- compute_sales_metrics(filtered_data)

  # Tworzenie wykresu za pomocą naszej innej funkcji
  wykres <- plot_sales_trends(filtered_data)

  return(list(
    metryki = metryki,
    wykres = wykres
  ))
}
