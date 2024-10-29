# Carica i pacchetti necessari
library(forecast)
library(ggplot2)
library(readxl)
library(zoo)
library(lubridate)

# Carica i dati da Excel
dati <- read_excel("D:/Desktop e cose/Desktop/Tesi/PIL_Trasposta.xlsx")

# Rimuovi eventuali spazi nella colonna 'Tempo'
dati$Tempo <- trimws(dati$Tempo)

# Converte la colonna 'Tempo' in date trimestrali
dati$Tempo <- as.yearqtr(dati$Tempo, format = "%Y-Q%q")

# Assicura che i dati siano ordinati per 'Tempo'
dati <- dati[order(dati$Tempo), ]

# Crea l'oggetto serie storica
anno_inizio <- as.numeric(format(min(dati$Tempo), "%Y"))
trimestre_inizio <- as.numeric(format(min(dati$Tempo), "%q"))
ts_dati <- ts(dati$PIL, start = c(anno_inizio, trimestre_inizio), frequency = 4)

# Verifica la presenza di valori NA nel PIL
if (any(is.na(ts_dati))) {
    stop("Ci sono valori mancanti nella colonna 'PIL'.")
}

# Definisci le date disponibili
date_disponibili <- as.Date(as.yearqtr(time(ts_dati)))

# Imposta l'orizzonte di previsione
h <- 4  # Dal 2024-Q3 al 2025-Q2

# Stima del modello ARIMA utilizzando tutti i dati disponibili fino al 2024-Q2
data_fine_stima <- as.Date(as.yearqtr("2024 Q2"))
fine_stima_index <- which(date_disponibili == data_fine_stima)
if (length(fine_stima_index) == 0) {
    stop("La data di fine stima non è presente nei dati.")
}

dati_train <- window(ts_dati, end = time(ts_dati)[fine_stima_index])

# Stima del modello ARIMA con finestra espandibile
modello_arima <- auto.arima(dati_train, seasonal = TRUE)

# Previsione per i prossimi 4 trimestri con intervallo di confidenza all'80%
previsione <- forecast(modello_arima, h = h, level = 80)

# Crea un data frame per le previsioni
date_previsione <- as.Date(as.yearqtr(time(previsione$mean)))
previsioni_df <- data.frame(
    Data = date_previsione,
    Valore = as.numeric(previsione$mean),
    Inferiore = as.numeric(previsione$lower[,1]),
    Superiore = as.numeric(previsione$upper[,1]),
    Tipo = "Previsione"
)

# Prepara i dati effettivi per il grafico
dati_reali <- data.frame(
    Data = date_disponibili,
    Valore = as.numeric(ts_dati),
    Inferiore = NA,
    Superiore = NA,
    Tipo = "Dati Reali"
)

# Combina i dati per il grafico
dati_grafico <- rbind(
    dati_reali,
    previsioni_df
)

# Grafico della previsione
ggplot(dati_grafico, aes(x = Data, y = Valore, color = Tipo)) +
    geom_line() +
    geom_point(data = previsioni_df, size = 2) +
    geom_ribbon(data = previsioni_df, aes(ymin = Inferiore, ymax = Superiore, fill = Tipo), alpha = 0.2, color = NA) +
    scale_x_date(
        limits = c(as.Date("2018-01-01"), max(dati_grafico$Data) + months(6)),
        date_breaks = "6 months",
        date_labels = "%Y-Q%q"
    ) +
    ggtitle("Previsione del PIL con Modello ARIMA (Finestra Espandibile)") +
    xlab("Anno") + ylab("Valore PIL") +
    scale_color_manual(values = c("Dati Reali" = "black", "Previsione" = "green")) +
    scale_fill_manual(values = c("Previsione" = "green")) +
    theme_minimal() +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
    )
