#' Oblicza kluczowe metryki biznesowe dla sprzedaży
#'
#' Funkcja wylicza całkowitą sprzedaż, średnią, zmienność (odchylenie standardowe)
#' oraz średnią liczbę produktów na promocji.
#'
#' @param data Oczyszczona ramka danych (np. wynik clean_sales_ts)
#' @return Ramka danych (tibble) z jednym wierszem podsumowującym metryki
#' @export
#' @examples
#' # compute_sales_metrics(czyste_dane)
compute_sales_metrics <- function(data) {

  metrics <- data |>
    dplyr::summarise(
      calkowita_sprzedaz = sum(sales, na.rm = TRUE),
      srednia_sprzedaz = mean(sales, na.rm = TRUE),
      zmiennosc_sprzedazy = stats::sd(sales, na.rm = TRUE),
      srednia_promocja = mean(onpromotion, na.rm = TRUE)
    )

  return(metrics)
}
