#' Generuje prognozę sprzedaży (ARIMA i Prophet)
#'
#' Domyślnie agreguje dane dla całej firmy, aby przyspieszyć obliczenia.
#' Użytkownik może opcjonalnie podać numer konkretnego sklepu.
#'
#' @param data Ramka danych ze sprzedażą
#' @param specific_store Opcjonalny numer sklepu (store_nbr) do analizy
#' @param horizon Horyzont prognozy (domyślnie 30 dni)
#' @return Lista zawierająca modele ARIMA i Prophet (lub ich prognozy)
#' @export
#' @examples
#' # prognoza <- create_prognosis(dane, specific_store = 1)
create_prognosis <- function(data, specific_store = NULL, horizon = 30) {

  # Filtrujemy, jeśli użytkownik podał sklep
  if (!is.null(specific_store)) {
    data <- dplyr::filter(data, store_nbr == specific_store)
  }

  # Agregacja do poziomu dziennego (optymalizacja obliczeń)
  daily_data <- data |>
    dplyr::group_by(date) |>
    dplyr::summarise(sales = sum(sales, na.rm = TRUE)) |>
    dplyr::arrange(date)

  # 1. Model ARIMA (używamy pakietu forecast)
  ts_sales <- stats::ts(daily_data$sales, frequency = 7) # Zakładamy sezonowość tygodniową
  arima_model <- forecast::auto.arima(ts_sales)
  arima_forecast <- forecast::forecast(arima_model, h = horizon)

  # 2. Model Prophet (wymaga kolumn 'ds' i 'y')
  prophet_df <- data.frame(
    ds = daily_data$date,
    y = daily_data$sales
  )

  # Wyciszamy logi Propheta, żeby nie śmieciły w konsoli
  prophet_model <- prophet::prophet(prophet_df, daily.seasonality = TRUE)
  future_dates <- prophet::make_future_dataframe(prophet_model, periods = horizon)
  prophet_forecast <- stats::predict(prophet_model, future_dates)

  return(list(
    ARIMA = arima_forecast,
    Prophet = prophet_forecast
  ))
}
