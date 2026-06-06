#' Czyści dane szeregów czasowych
#'
#' Zastępuje braki danych (NA) w kolumnach numerycznych średnią wartością z danej kolumny.
#'
#' @param data Ramka danych do wyczyszczenia
#' @return Oczyszczona ramka danych bez braków w kolumnach numerycznych
#' @export
#' @examples
#' # clean_sales_ts(moje_dane)
clean_sales_ts <- function(data) {

  cleaned_data <- dplyr::mutate(
    data,
    dplyr::across(
      dplyr::where(is.numeric),
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    )
  )

  return(cleaned_data)
}
