# Carica i pacchetti necessari
library(forecast)
library(ggplot2)
library(readxl)
library(zoo)

# Carica i dati da Excel
dati <- read_excel("D:/Desktop e cose/Desktop/Tesi/PIL_Trasposta.xlsx")

# Rimuovi eventuali spazi nella colonna 'Tempo'
dati$Tempo <- trimws(dati$Tempo)

# Converte la colonna 'Tempo' in date trimestrali
dati$Tempo <- as.yearqtr(dati$Tempo, format = "%Y-Q%q")

# Verifica la presenza di valori NA nelle date
if (any(is.na(dati$Tempo))) {
    stop("Errore nella conversione delle date. Controlla il formato della colonna 'Tempo'.")
}

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
date_disponibili <- as.yearqtr(time(ts_dati))

# Imposta i parametri per il rolling forecasting
h <- 8  # Previsioni da 2022-Q3 a 2024-Q2 (8 trimestri)

# Trova l'indice per l'inizio del rolling forecasting (2022-Q4)
data_inizio <- as.yearqtr("2022 Q3")
inizio_forecast <- which(date_disponibili == data_inizio)

# Se la data non viene trovata esattamente, trova la più vicina
if (length(inizio_forecast) == 0) {
    inizio_forecast <- which.min(abs(as.numeric(date_disponibili) - as.numeric(data_inizio)))
}

# Prepara i dati effettivi per il grafico (dal 2018 in poi)
ts_dati_grafico <- window(ts_dati, start = c(2018, 1))
dati_reali <- data.frame(
    Data = as.Date(date_disponibili[date_disponibili >= as.yearqtr("2018 Q1")]),
    Valore = as.numeric(ts_dati_grafico),
    Inferiore = NA,
    Superiore = NA,
    Tipo = "Dati Reali"
)

### Rolling Forecasting con Finestra Espandibile ###

# Inizializza i vettori per memorizzare i risultati
previsioni_espandibile <- numeric(h)
inferiore_espandibile <- numeric(h)
superiore_espandibile <- numeric(h)
aic_espandibile <- numeric(h)
bic_espandibile <- numeric(h)

# Ciclo di rolling forecasting
for (i in 1:h) {
    # Finestra espandibile: dati di training fino al tempo t + i - 2
    fine_train_index <- inizio_forecast + i - 2
    if (fine_train_index < 1) {
        fine_train_index <- 1
    }
    fine_train <- time(ts_dati)[fine_train_index]
    dati_train <- window(ts_dati, end = fine_train)
    
    # Stima del modello ETS
    modello <- ets(dati_train)
    
    # Previsione del periodo successivo con intervalli di confidenza
    previsione <- forecast(modello, h = 1, level = 80)
    
    # Memorizza la previsione e gli intervalli
    previsioni_espandibile[i] <- previsione$mean
    inferiore_espandibile[i] <- previsione$lower
    superiore_espandibile[i] <- previsione$upper
    
    # Memorizza i criteri informativi
    aic_espandibile[i] <- AIC(modello)
    bic_espandibile[i] <- BIC(modello)
}

# Crea un data frame per le previsioni con finestra espandibile
date_espandibile <- seq(data_inizio, by = 0.25, length.out = h)
previsioni_espandibile_df <- data.frame(
    Data = as.Date(date_espandibile),
    Valore = previsioni_espandibile,
    Inferiore = inferiore_espandibile,
    Superiore = superiore_espandibile,
    Tipo = "Previsione Rolling"
)

### Rolling Forecasting con Finestra a Lunghezza Fissa ###

# Inizializza i vettori per memorizzare i risultati
previsioni_fissa <- numeric(h)
inferiore_fissa <- numeric(h)
superiore_fissa <- numeric(h)
aic_fissa <- numeric(h)
bic_fissa <- numeric(h)

dimensione_finestra <- 40  # 40 trimestri (10 anni)

for (i in 1:h) {
    # Finestra a lunghezza fissa
    inizio_train_index <- inizio_forecast + i - dimensione_finestra - 1
    fine_train_index <- inizio_forecast + i - 2
    
    # Assicura che gli indici siano validi
    if (inizio_train_index < 1) {
        inizio_train_index <- 1
    }
    if (fine_train_index < inizio_train_index) {
        fine_train_index <- inizio_train_index
    }
    
    inizio_train <- time(ts_dati)[inizio_train_index]
    fine_train <- time(ts_dati)[fine_train_index]
    
    dati_train <- window(ts_dati, start = inizio_train, end = fine_train)
    
    # Stima del modello ETS
    modello <- ets(dati_train)
    
    # Previsione del periodo successivo con intervalli di confidenza
    previsione <- forecast(modello, h = 1, level = 80)
    
    # Memorizza la previsione e gli intervalli
    previsioni_fissa[i] <- previsione$mean
    inferiore_fissa[i] <- previsione$lower
    superiore_fissa[i] <- previsione$upper
    
    # Memorizza i criteri informativi
    aic_fissa[i] <- AIC(modello)
    bic_fissa[i] <- BIC(modello)
}

# Crea un data frame per le previsioni con finestra a lunghezza fissa
date_fissa <- seq(data_inizio, by = 0.25, length.out = h)
previsioni_fissa_df <- data.frame(
    Data = as.Date(date_fissa),
    Valore = previsioni_fissa,
    Inferiore = inferiore_fissa,
    Superiore = superiore_fissa,
    Tipo = "Previsione Rolling"
)

### Grafici dei Risultati ###

# Combina i dati per il grafico (Finestra Espandibile)
dati_grafico_espandibile <- rbind(
    dati_reali,
    previsioni_espandibile_df
)

# Grafico per la finestra espandibile
ggplot(dati_grafico_espandibile, aes(x = Data, y = Valore, color = Tipo)) +
    geom_line() +
    geom_ribbon(data = previsioni_espandibile_df, aes(ymin = Inferiore, ymax = Superiore, fill = Tipo), alpha = 0.2, color = NA) +
    scale_x_date(
        limits = c(as.Date("2018-01-01"), max(dati_grafico_espandibile$Data)),
        date_breaks = "6 months",
        date_labels = "%Y-Q%q"
    ) +
    ggtitle("Modello ETS con Finestra Espandibile") +
    xlab("Anno") + ylab("Valore PIL") +
    scale_color_manual(values = c("Dati Reali" = "black", "Previsione Rolling" = "blue")) +
    scale_fill_manual(values = c("Previsione Rolling" = "blue")) +
    theme_minimal() +
    theme(
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
    )

# Combina i dati per il grafico (Finestra a Lunghezza Fissa)
dati_grafico_fissa <- rbind(
    dati_reali,
    previsioni_fissa_df
)

# Grafico per la finestra a lunghezza fissa
ggplot(dati_grafico_fissa, aes(x = Data, y = Valore, color = Tipo)) +
    geom_line() +
    geom_ribbon(data = previsioni_fissa_df, aes(ymin = Inferiore, ymax = Superiore, fill = Tipo), alpha = 0.2, color = NA) +
    scale_x_date(
        limits = c(as.Date("2018-01-01"), max(dati_grafico_fissa$Data)),
        date_breaks = "6 months",
        date_labels = "%Y-Q%q"
    ) +
    ggtitle("Modello ETS con Finestra a Lunghezza Fissa") +
    xlab("Anno") + ylab("Valore PIL") +
    scale_color_manual(values = c("Dati Reali" = "black", "Previsione Rolling" = "red")) +
    scale_fill_manual(values = c("Previsione Rolling" = "red")) +
    theme_minimal() +
    theme(
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
    )

### Calcolo degli Indici di Accuratezza ###

# Calcola l'accuratezza utilizzando i dati effettivi disponibili
test_data_indices <- (inizio_forecast):(inizio_forecast + h -1)
test_data_indices <- test_data_indices[test_data_indices <= length(ts_dati)]
dati_test <- ts_dati[test_data_indices]

# Allinea le previsioni con i dati di test
previsioni_espandibile_allineate <- previsioni_espandibile[1:length(dati_test)]
previsioni_fissa_allineate <- previsioni_fissa[1:length(dati_test)]

# Calcola l'accuratezza
accuratezza_espandibile <- accuracy(previsioni_espandibile_allineate, dati_test)
accuratezza_fissa <- accuracy(previsioni_fissa_allineate, dati_test)

# Stampa gli indici di accuratezza
print("Indici di Accuratezza per ETS (Finestra Espandibile):")
print(accuratezza_espandibile)

print("Indici di Accuratezza per ETS (Finestra a Lunghezza Fissa):")
print(accuratezza_fissa)

# Stampa i criteri informativi
criteri_info_espandibile <- data.frame(
    Data = date_espandibile[1:length(aic_espandibile)],
    AIC = aic_espandibile,
    BIC = bic_espandibile
)
print("Criteri Informativi per ETS (Finestra Espandibile):")
print(criteri_info_espandibile)

criteri_info_fissa <- data.frame(
    Data = date_fissa[1:length(aic_fissa)],
    AIC = aic_fissa,
    BIC = bic_fissa
)
print("Criteri Informativi per ETS (Finestra a Lunghezza Fissa):")
print(criteri_info_fissa)
