#' Sprawdza jakość danych szeregów czasowych
#'
#' Weryfikuje ramkę danych pod kątem braków wartości (NA) i zduplikowanych wierszy.
#'
#' @param data Ramka danych do walidacji
#' @return Lista z wynikami weryfikacji (wartości logiczne)
#' @export
#' @examples
#' # validate_sales_ts(moje_dane)
validate_sales_ts <- function(data) {

  has_nas <- any(is.na(data))
  has_duplicates <- any(duplicated(data))

  validation_results <- list(
    zawiera_braki = has_nas,
    zawiera_duplikaty = has_duplicates
  )

  return(validation_results)
}
