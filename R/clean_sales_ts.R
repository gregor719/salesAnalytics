#' Czyści dane szeregów czasowych
#'
#' Zastępuje braki danych (NA) w kolumnach numerycznych średnią wartością,
#' a w kolumnach tekstowych i logicznych wstawia wartości domyślne ("Brak danych" / FALSE).
#'
#' @param data Ramka danych do wyczyszczenia
#' @return Oczyszczona ramka danych bez braków
#' @export
#' @examples
#' # clean_sales_ts(moje_dane)
clean_sales_ts <- function(data) {

  cleaned_data <- dplyr::mutate(
    data,
    # 1. Numeryczne: zastępujemy średnią
    dplyr::across(
      dplyr::where(is.numeric),
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    ),
    # 2. Tekstowe (związane ze świętami): zastępujemy tekstem
    dplyr::across(
      dplyr::where(is.character),
      ~ ifelse(is.na(.), "Brak danych", .)
    ),
    # 3. Logiczne (np. kolumna transferred): zastępujemy FALSE
    dplyr::across(
      dplyr::where(is.logical),
      ~ ifelse(is.na(.), FALSE, .)
    )
  )

  return(cleaned_data)
}
