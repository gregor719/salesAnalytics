#' Tworzy wizualizację trendów sprzedażowych
#'
#' Rysuje wykres liniowy pokazujący sprzedaż w czasie. Aby wykres był czytelny,
#' zaleca się wcześniejsze zagregowanie danych (np. do poziomu dziennego lub tygodniowego).
#'
#' @param data Ramka danych zawierająca kolumny 'date' i 'sales'
#' @return Obiekt wykresu ggplot
#' @export
#' @examples
#' # plot_sales_trends(czyste_dane)
plot_sales_trends <- function(data) {

  # Agregujemy dane do poziomu dnia, aby wykres był czytelny
  daily_data <- data |>
    dplyr::group_by(date) |>
    dplyr::summarise(sales = sum(sales, na.rm = TRUE))

  wykres <- ggplot2::ggplot(daily_data, ggplot2::aes(x = date, y = sales)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::labs(
      title = "Trend sprzedazy w czasie",
      x = "Data",
      y = "Calkowita sprzedaz"
    ) +
    ggplot2::theme_minimal()

  return(wykres)
}
