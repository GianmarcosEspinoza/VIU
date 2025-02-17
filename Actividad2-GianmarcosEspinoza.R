library(dplyr)
library(ggplot2)
library(tidyr)
library(tseries)
library(stats)
library(forecast)
library(corrplot)
library(tseries)


#1. Datasets
dataset2 <- read.csv("https://raw.githubusercontent.com/GianmarcosEspinoza/VIU/refs/heads/main/ventas.csv", header = TRUE)
cat("Número de filas:", nrow(dataset2), "\n");

#Se soluciona el tipo de dato de fecha de venta
str(dataset2)
dataset2$datesold <- as.Date(dataset2$datesold)
str(dataset2)

#2. Obtener dataset de train y test
set.seed(140);
dato_entrenamiento <- dataset2[sample(nrow(dataset2), 0.8 * nrow(dataset2)), ]
dato_prueba <- dataset2[-seq_len(nrow(dato_entrenamiento)), ]

dato_entrenamiento <- dato_entrenamiento[order(dato_entrenamiento$datesold), ]
dato_prueba <- dato_prueba[order(dato_prueba$datesold), ]

#3 Verifica si hay datos nulos y duplicados
cantidad_nulos <- sum(is.na(dataset2))
cat("La cantidad de datos nulos es:", cantidad_nulos, "\n")

duplicados <- dataset2[duplicated(dataset2), ]
cat("La cantidad de duplicados es:", nrow(duplicados), "\n")

#4. Estadísticas descriptivas
descriptive_stats <- summary(dato_entrenamiento)
print(descriptive_stats)


#5. boxplot para identificar outliers

generar_boxplot_general <- function(data, variables) {
  data %>%
    select(any_of(variables)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
    geom_boxplot() +
    facet_wrap(~ Variable, scales = "free_y") +  # Escalas independientes
    labs(title = "Boxplot de Price y Bedrooms",
         x = "Variable", y = "Valor") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank())
}

generar_boxplot_general(dato_entrenamiento, c("price", "bedrooms"))

# Se trata los outliers de price
outliers_count <- sum(dato_entrenamiento$price > 900000)
dato_entrenamiento <- dato_entrenamiento[dato_entrenamiento$price <= 900000, ]
dato_prueba <- dato_prueba[dato_prueba$price <= 900000, ]


#6. Se encodea la variable  cualitativa

dato_entrenamiento$propertyType <- as.factor(dato_entrenamiento$propertyType)
dato_entrenamiento$propertyType <- ifelse(dato_entrenamiento$propertyType == 'house', 1, 0)

dato_prueba$propertyType <- as.factor(dato_prueba$propertyType)
dato_prueba$propertyType <- ifelse(dato_prueba$propertyType == 'house', 1, 0)



#7. Verificar la normalidad mediante histograma


crear_histograma_general <- function(data, variables) {
  data %>%
    select(any_of(variables)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    ggplot(aes(x = Valor)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    facet_wrap(~ Variable, scales = "free", ncol = 2) +  # Escalas independientes para X e Y
    labs(title = "Histogramas de Variables Seleccionadas", 
         x = "Valor", y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

crear_histograma_general(dato_entrenamiento, c("postcode", "price", "propertyType", "bedrooms"))


#8. Se analiza la matriz de correlación

numeric_data <- dato_entrenamiento[sapply(dato_entrenamiento, is.numeric)]
matriz_correlacion <- cor(numeric_data)
# Configurar los márgenes de la gráfica
par(mar = c(1, 1, 1, 1))

corrplot(matriz_correlacion, 
         method = "color", 
         type = "full", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black")


#9. Descomposicion de la serie temporal

df_mensual <- dato_entrenamiento %>%
  mutate(year_month = format(datesold, "%Y-%m")) %>%  # Crea columna de año-mes
  group_by(year_month) %>%
  summarise(monthly_price = mean(price)) %>%          # Calcula el precio promedio del mes
  ungroup() %>%
  arrange(year_month)


start_year  <- as.numeric(substr(df_mensual$year_month[1], 1, 4))
start_month <- as.numeric(substr(df_mensual$year_month[1], 6, 7))
ts_mensual <- ts(df_mensual$monthly_price, frequency = 12, start = c(start_year, start_month))
decomposition_mensual <- stl(ts_mensual, s.window = "periodic")


plot(decomposition_mensual)
trend_component    <- decomposition_mensual$time.series[, "trend"] 
seasonal_component <- decomposition_mensual$time.series[, "seasonal"]
residual_component <- decomposition_mensual$time.series[, "remainder"]

#10. Análisis de autocorrelación 


par(mfrow = c(1, 2))
acf(ts_mensual, lag.max = 36, main = "Autocorrelación (ACF)")
pacf(ts_mensual, lag.max = 36, main = "Autocorrelación Parcial (PACF)")
par(mfrow = c(1, 1))  # Restablece la disposición gráfica

ljung_box_test <- Box.test(residual_component, lag = 20, type = "Ljung-Box")
print(ljung_box_test)

kpss_test <- kpss.test(ts_mensual)
print(kpss_test)

##11. diferenciacion

ts_diff <- diff(ts_mensual)  # Primera diferenciación
adf.test(ts_diff)
kpss.test(ts_diff)

adf_test_diff <- adf.test(ts_diff)
print(adf_test_diff)

# Realizamos la prueba KPSS sobre la serie diferenciada
kpss_test_diff <- kpss.test(ts_diff)
print(kpss_test_diff)


#12. Creación del modelo adecuado


arima_model <- auto.arima(ts_mensual)
summary(arima_model)

checkresiduals(arima_model)


h_forecast <- min(12, length(dato_prueba$price))
forecast_arima <- forecast(arima_model, h = h_forecast)
plot(forecast_arima)
accuracy(forecast_arima$mean, dato_prueba$price[1:h_forecast])

