#' Generuje podsumowanie biznesowe dla zarządu
#'
#' Oblicza kluczowe wskaźniki takie jak najlepszy i najgorszy sklep pod względem
#' całkowitej sprzedaży oraz średnią sprzedaż dla całej sieci.
#'
#' @param data Oczyszczona ramka danych (np. wynik clean_sales_ts)
#' @return Lista zawierająca kluczowe wnioski biznesowe
#' @export
#' @examples
#' # create_management_summary(czyste_dane)
create_management_summary <- function(data) {

  # Agregacja sprzedaży per sklep
  store_performance <- data |>
    dplyr::group_by(store_nbr) |>
    dplyr::summarise(total_sales = sum(sales, na.rm = TRUE)) |>
    dplyr::arrange(dplyr::desc(total_sales))

  najlepszy_sklep <- store_performance$store_nbr[1]
  najgorszy_sklep <- store_performance$store_nbr[nrow(store_performance)]

  # Średnia sprzedaż globalna
  srednia_sprzedaz_globalna <- mean(data$sales, na.rm = TRUE)

  # Ostatnia znana data w zbiorze
  ostatnia_data <- max(data$date, na.rm = TRUE)

  podsumowanie <- list(
    Najlepszy_sklep_ID = najlepszy_sklep,
    Najgorszy_sklep_ID = najgorszy_sklep,
    Srednia_sprzedaz_transakcji = srednia_sprzedaz_globalna,
    Data_ostatniego_raportu = ostatnia_data
  )

  return(podsumowanie)
}
