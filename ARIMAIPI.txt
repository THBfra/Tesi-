# Carica i pacchetti necessari
library(forecast)
library(ggplot2)
library(readxl)
library(zoo)

# Carica i dati da Excel
data <- read_excel("D:/Desktop e cose/Desktop/Tesi/IPI_Trasposta.xlsx")

# Converte la colonna 'Tempo' in formato Date
data$Tempo <- as.Date(paste0(data$Tempo, "-01"), format = "%Y-%m-%d")

# Assicura che i dati siano ordinati per 'Tempo'
data <- data[order(data$Tempo), ]

# Crea l'oggetto serie storica
start_year <- as.numeric(format(min(data$Tempo), "%Y"))
start_month <- as.numeric(format(min(data$Tempo), "%m"))
ts_data <- ts(data$IPI, start = c(start_year, start_month), frequency = 12)

# Imposta i parametri per il rolling forecasting
h <- 13  # Previsioni da agosto 2023 ad agosto 2024 (13 mesi)

# Trova l'indice di inizio per il rolling forecasting (agosto 2023)
start_date <- as.Date("2023-08-01")
start_forecast <- which(time(ts_data) == as.yearmon(start_date))

# Se la data non è trovata esattamente, trova la più vicina
if (length(start_forecast) == 0) {
  start_forecast <- which.min(abs(as.numeric(time(ts_data)) - as.numeric(as.yearmon(start_date))))
}

# Prepara i dati effettivi per il grafico (dal 2022 in poi)
ts_data_plot <- window(ts_data, start = c(2022, 1))
actual_data <- data.frame(
  Date = as.Date(time(ts_data_plot)),
  Value = as.numeric(ts_data_plot),
  Lower = NA,
  Upper = NA,
  Type = "Dati Reali"
)

### Rolling Forecasting con Finestra Espandibile ###

# Inizializza i vettori per memorizzare i risultati
forecasts_expanding <- numeric(h)
lower_expanding <- numeric(h)
upper_expanding <- numeric(h)
aic_values_expanding <- numeric(h)
bic_values_expanding <- numeric(h)

# Ciclo di rolling forecasting finestra espandibile
for (i in 1:h) {
  # Finestra espandibile: dati di training fino al tempo t+i-2
  train_end_index <- start_forecast + i - 2
  if (train_end_index < 1) {
    train_end_index <- 1
  }
  train_end <- time(ts_data)[train_end_index]
  train_data <- window(ts_data, end = train_end)
  
  # Stima del modello ARIMA
  fit <- auto.arima(train_data, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
  
  # Previsione del periodo successivo con intervalli di confidenza del 95 %
  fc <- forecast(fit, h = 1, level = 95)
  
  # Memorizza la previsione e gli intervalli
  forecasts_expanding[i] <- fc$mean
  lower_expanding[i] <- fc$lower
  upper_expanding[i] <- fc$upper
  
  # Memorizza i criteri informativi
  aic_values_expanding[i] <- AIC(fit)
  bic_values_expanding[i] <- BIC(fit)
}

# Crea un data frame per le previsioni con finestra espandibile
dates_expanding <- seq(start_date, by = "month", length.out = h)
forecast_expanding_df <- data.frame(
  Date = dates_expanding,
  Value = forecasts_expanding,
  Lower = lower_expanding,
  Upper = upper_expanding,
  Type = "Previsione Rolling"
)

### Rolling Forecasting con Finestra a Lunghezza Fissa ###

# Inizializza i vettori per memorizzare i risultati
forecasts_fixed <- numeric(h)
lower_fixed <- numeric(h)
upper_fixed <- numeric(h)
aic_values_fixed <- numeric(h)
bic_values_fixed <- numeric(h)

window_size <- 120  # 10 anni

for (i in 1:h) {
  # Finestra a lunghezza fissa
  train_start_index <- start_forecast + i - window_size - 1
  train_end_index <- start_forecast + i - 2
  
  # Assicura che gli indici siano validi
  if (train_start_index < 1) {
    train_start_index <- 1
  }
  if (train_end_index < train_start_index) {
    train_end_index <- train_start_index
  }
  
  train_start <- time(ts_data)[train_start_index]
  train_end <- time(ts_data)[train_end_index]
  
  train_data <- window(ts_data, start = train_start, end = train_end)
  
  # Stima del modello ARIMA
  fit <- auto.arima(train_data, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
  
  # Previsione del periodo successivo con intervalli di confidenza
  fc <- forecast(fit, h = 1, level = 95)
  
  # Memorizza la previsione e gli intervalli
  forecasts_fixed[i] <- fc$mean
  lower_fixed[i] <- fc$lower
  upper_fixed[i] <- fc$upper
  
  # Memorizza i criteri informativi
  aic_values_fixed[i] <- AIC(fit)
  bic_values_fixed[i] <- BIC(fit)
}

# Crea un data frame per le previsioni con finestra a lunghezza fissa
dates_fixed <- seq(start_date, by = "month", length.out = h)
forecast_fixed_df <- data.frame(
  Date = dates_fixed,
  Value = forecasts_fixed,
  Lower = lower_fixed,
  Upper = upper_fixed,
  Type = "Previsione Rolling"
)

# Combina i dati reali e le previsioni per il grafico (finestra espandibile)
plot_data_expanding <- rbind(
  actual_data,
  forecast_expanding_df
)

# Grafico per la finestra espandibile (dal 2022 in poi)
ggplot(plot_data_expanding, aes(x = Date, y = Value, color = Type)) +
  geom_line() +
  geom_ribbon(data = forecast_expanding_df, aes(ymin = Lower, ymax = Upper, fill = Type), alpha = 0.2, color = NA) +
  scale_x_date(
    limits = as.Date(c("2022-01-01", max(plot_data_expanding$Date))),
    date_breaks = "2 months",
    date_labels = "%Y-%m"
  ) +
  ggtitle("Modello ARIMA con Finestra Espandibile") +
  xlab("Anno") + ylab("Valore Indice") +
  scale_color_manual(values = c("Dati Reali" = "black", "Previsione Rolling" = "blue")) +
  scale_fill_manual(values = c("Previsione Rolling" = "blue")) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# Combina i dati reali e le previsioni per il grafico (finestra a lunghezza fissa)
plot_data_fixed <- rbind(
  actual_data,
  forecast_fixed_df
)

# Grafico per la finestra a lunghezza fissa (dal 2022 in poi)
ggplot(plot_data_fixed, aes(x = Date, y = Value, color = Type)) +
  geom_line() +
  geom_ribbon(data = forecast_fixed_df, aes(ymin = Lower, ymax = Upper, fill = Type), alpha = 0.2, color = NA) +
  scale_x_date(
    limits = as.Date(c("2022-01-01", max(plot_data_fixed$Date))),
    date_breaks = "2 months",
    date_labels = "%Y-%m"
  ) +
  ggtitle("Modello ARIMA con Finestra a Lunghezza Fissa") +
  xlab("Anno") + ylab("Valore Indice") +
  scale_color_manual(values = c("Dati Reali" = "black", "Previsione Rolling" = "red")) +
  scale_fill_manual(values = c("Previsione Rolling" = "red")) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# Calcola gli indici di accuratezza utilizzando i dati di test effettivi
test_data_indices <- (start_forecast):(start_forecast + h -1)
test_data_indices <- test_data_indices[test_data_indices <= length(ts_data)]
test_data <- ts_data[test_data_indices]

# Allinea le previsioni con i dati di test
forecasts_expanding_aligned <- forecasts_expanding[1:length(test_data)]
forecasts_fixed_aligned <- forecasts_fixed[1:length(test_data)]

# Calcola l'accuratezza
accuracy_expanding <- accuracy(forecasts_expanding_aligned, test_data)
accuracy_fixed <- accuracy(forecasts_fixed_aligned, test_data)

# Stampa gli indici di accuratezza
print("Indici di Accuratezza per ARIMA (Finestra Espandibile):")
print(accuracy_expanding)

print("Indici di Accuratezza per ARIMA (Finestra a Lunghezza Fissa):")
print(accuracy_fixed)

# Stampa i criteri informativi
info_criteria_expanding <- data.frame(
  Date = dates_expanding[1:length(aic_values_expanding)],
  AIC = aic_values_expanding,
  BIC = bic_values_expanding
)
print("Criteri Informativi per ARIMA (Finestra Espandibile):")
print(info_criteria_expanding)

info_criteria_fixed <- data.frame(
  Date = dates_fixed[1:length(aic_values_fixed)],
  AIC = aic_values_fixed,
  BIC = bic_values_fixed
)
print("Criteri Informativi per ARIMA (Finestra a Lunghezza Fissa):")
print(info_criteria_fixed)

