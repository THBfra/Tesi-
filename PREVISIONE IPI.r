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

# Prepara i dati effettivi per il grafico (dal 2022 in poi)
ts_data_plot <- window(ts_data, start = c(2022, 1))
actual_data <- data.frame(
  Date = as.Date(time(ts_data_plot)),
  Value = as.numeric(ts_data_plot),
  Lower = NA,
  Upper = NA,
  Type = "Dati Reali"
)

# Imposta la dimensione della finestra a lunghezza fissa
window_size <- 120  # 10 anni

# Data di fine training
train_end_date <- as.Date("2024-08-01")
train_end_index <- which(time(ts_data) == as.yearmon(train_end_date))
if (length(train_end_index) == 0) {
  stop("La data di fine training non è presente nei dati.")
}

# Calcola l'indice di inizio del training
train_start_index <- train_end_index - window_size + 1
if (train_start_index < 1) {
  train_start_index <- 1
}

# Estrai i dati di training
train_start_time <- time(ts_data)[train_start_index]
train_end_time <- time(ts_data)[train_end_index]
train_data <- window(ts_data, start = train_start_time, end = train_end_time)

# Stima del modello ETS
fit_personal <- ets(train_data)

# Previsione dei prossimi 13 mesi con intervalli di confidenza al 95%
h <- 12  # Da settembre 2024 ad agosto 2025
fc_personal <- forecast(fit_personal, h = h, level = 95)

# Crea un data frame per la previsione personale
dates_personal <- seq(as.Date("2024-09-01"), by = "month", length.out = h)
personal_forecast_df <- data.frame(
  Date = dates_personal,
  Value = as.numeric(fc_personal$mean),
  Lower = as.numeric(fc_personal$lower),
  Upper = as.numeric(fc_personal$upper),
  Type = "Intervalli di confidenza previsione"
)

# Combina i dati reali e la previsione per il grafico
plot_data <- rbind(
  actual_data,
  personal_forecast_df
)

# Grafico della previsione personale (dal 2022 in poi)
ggplot(plot_data, aes(x = Date, y = Value)) +
  geom_line(data = subset(plot_data, Type == "Dati Reali"), aes(color = "Dati Reali")) +
  geom_line(data = subset(plot_data, Type == "Intervalli di confidenza previsione"), aes(color = "Previsione")) +
  geom_ribbon(data = subset(plot_data, Type == "Intervalli di confidenza previsione"), aes(ymin = Lower, ymax = Upper, fill = "Area di confidenza"), alpha = 0.2, color = NA) +
  geom_point(data = personal_forecast_df, aes(x = Date, y = Value), color = "green", size = 1.5) +  # Punto verde per ogni valore atteso
  scale_x_date(
    limits = as.Date(c("2022-01-01", "2025-09-01")),
    date_breaks = "2 months",
    date_labels = "%Y-%m"
  ) +
  ggtitle("Previsione IPI con Modello ETS (Finestra a Lunghezza Fissa)") +
  xlab("Anno") + ylab("Valore Indice") +
  scale_color_manual(name = "Legenda", values = c("Dati Reali" = "black", "Previsione" = "green")) +
  scale_fill_manual(name = "Legenda", values = c("Area di confidenza" = "lightgreen")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# Stampa i valori attesi, lower e upper per ogni mese previsto
print("Valori attesi, Lower e Upper per ogni mese previsto:")
print(personal_forecast_df[, c("Date", "Value", "Lower", "Upper")])
