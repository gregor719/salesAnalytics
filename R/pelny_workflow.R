library(salesAnalytics)

# 1. Wczytanie danych (pamiętaj, aby pliki csv z Kaggle były w folderze roboczym)
dane_surowe <- load_sales_data(
  train_path = "train.csv",
  stores_path = "stores.csv",
  holidays_path = "holidays_events.csv"
)

# 2. Walidacja jakości
raport_jakosci <- validate_sales_ts(dane_surowe)
print(raport_jakosci)

# 3. Czyszczenie danych (obsługa NA)
dane_czyste <- clean_sales_ts(dane_surowe)

# 4. Generowanie podsumowania dla zarządu
podsumowanie <- create_management_summary(dane_czyste)
print(podsumowanie)

# 5. Zaawansowana analiza dla wybranego miasta
analiza_quito <- sales_ts_logic(dane_czyste, filter_city = "Quito")
print(analiza_quito$metryki)
print(analiza_quito$wykres)

# 6. Prognozowanie ogólnego trendu (ARIMA i Prophet)
prognoza <- create_prognosis(dane_czyste, horizon = 30)
