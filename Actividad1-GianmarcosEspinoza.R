#Si no se cuenta con alguna libreria toca instalarla con el siguiente comando como guía
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)
library(corrplot)
library(nnet)
library(caret)

#1. Cargar el dataset y ver cantidad de datos
dataset <- read.csv("https://github.com/GianmarcosEspinoza/VIU/raw/refs/heads/main/car_price.csv", header = TRUE, sep=",")
cat("La cantidad de columnas es", ncol(dataset), "y sus nombres son:", paste(names(dataset), collapse = ", "), "\n");
cat("Número de filas:", nrow(dataset), "\n");


#2. Se divide el dataset en datos para entrenamiento y prueba, y evitar tocar datos no vistos (datos prueba)
set.seed(123);
indices_entrenamiento <- sample(nrow(dataset), 0.9 * nrow(dataset))
dataset_entrenamiento <- dataset[indices_entrenamiento, ]
dataset_prueba <- dataset[-indices_entrenamiento, ]



#3 Tratamiento de datos nulos
verificar_nulos <- function(df) {
  nombre_df <- deparse(substitute(df))
  nulos_por_fila <- rowSums(is.na(df))
  total_filas_nulas <- sum(nulos_por_fila > 0)
  cat("La cantidad total de filas con datos nulos en el dataframe", nombre_df, "es:", total_filas_nulas, "\n")
}


verificar_nulos(dataset_entrenamiento)
verificar_nulos(dataset_prueba)


#4. Tratamiento de duplicados
cat("La cantidad de duplicados es:", nrow(dataset_entrenamiento[duplicated(dataset_entrenamiento), ]), "\n")
cat("La cantidad de duplicados es:", nrow(dataset_prueba[duplicated(dataset_prueba), ]), "\n")



###################################Se comienza el análisis de train##################


#5. Calcular estadísticas descriptivas
descriptive_stats <- summary(dataset_entrenamiento)
print(descriptive_stats)


#6. Realizar diagrama de caja para identificar outliers
generar_boxplot <- function(data, variable) {
  data %>%
    select(any_of(variable)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", variable),
         x = "Variable", y = "Valor") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.8),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

generar_boxplot(dataset_entrenamiento, "enginesize")
generar_boxplot(dataset_entrenamiento, "price")
generar_boxplot(dataset_entrenamiento, "curbweight")
generar_boxplot(dataset_entrenamiento, "horsepower")
generar_boxplot(dataset_entrenamiento, "compressionratio")
generar_boxplot(dataset_entrenamiento, "wheelbase")
generar_boxplot(dataset_entrenamiento, "carlength")
generar_boxplot(dataset_entrenamiento, "carwidth")
generar_boxplot(dataset_entrenamiento, "carheight")
generar_boxplot(dataset_entrenamiento, "citympg")
generar_boxplot(dataset_entrenamiento, "highwaympg")

# Se complementa con el método del IQR
identify_outliers <- function(data) {
  outlier_indices <- list()  # Lista para almacenar los índices de outliers
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {  # Solo procesar columnas numéricas
      # Calcular Q1, Q3 e IQR
      Q1 <- quantile(data[[col]], 0.25)
      Q3 <- quantile(data[[col]], 0.75)
      IQR <- Q3 - Q1
      # Definir límites inferior y superior
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      # Identificar índices de outliers
      outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
      outlier_indices[[col]] <- outliers  # Almacenar índices por columna
    }
  }
  return(outlier_indices)
}

outliers <- identify_outliers(dataset_entrenamiento)
print(outliers)




#7. Verificar la normalidad mediante prueba de shapiro-wilk complementado con Regla de Scott para definir bindwidth

binwidth_scott <- function(x) {
  3.5 * sd(x) / length(x)^(1/3)
}

#Definir la función para crear histogramas
crear_histograma <- function(data, variable) {
  ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(binwidth = binwidth_scott(data[[variable]]),
                   fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histograma de", variable),
         x = "Valor", y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

crear_histograma(dataset_entrenamiento, "symboling")
crear_histograma(dataset_entrenamiento, "wheelbase")
crear_histograma(dataset_entrenamiento, "carlength")
crear_histograma(dataset_entrenamiento, "carwidth")
crear_histograma(dataset_entrenamiento, "carheight")
crear_histograma(dataset_entrenamiento, "curbweight")
crear_histograma(dataset_entrenamiento, "enginesize")
crear_histograma(dataset_entrenamiento, "boreratio")
crear_histograma(dataset_entrenamiento, "stroke")
crear_histograma(dataset_entrenamiento, "compressionratio")
crear_histograma(dataset_entrenamiento, "horsepower")
crear_histograma(dataset_entrenamiento, "peakrpm")
crear_histograma(dataset_entrenamiento, "citympg")
crear_histograma(dataset_entrenamiento, "highwaympg")
crear_histograma(dataset_entrenamiento, "price")



#Definir la función para la prueba de normalidad

shapiro_wilk_test <- function(data, variable) {
  shapiro.test(data[[variable]])
}

shapiro_wilk_test(dataset_entrenamiento, "symboling")
shapiro_wilk_test(dataset_entrenamiento, "wheelbase")
shapiro_wilk_test(dataset_entrenamiento, "carlength")
shapiro_wilk_test(dataset_entrenamiento, "carwidth")
shapiro_wilk_test(dataset_entrenamiento, "carheight")
shapiro_wilk_test(dataset_entrenamiento, "curbweight")
shapiro_wilk_test(dataset_entrenamiento, "enginesize")
shapiro_wilk_test(dataset_entrenamiento, "boreratio")
shapiro_wilk_test(dataset_entrenamiento, "stroke")
shapiro_wilk_test(dataset_entrenamiento, "compressionratio")
shapiro_wilk_test(dataset_entrenamiento, "horsepower")
shapiro_wilk_test(dataset_entrenamiento, "peakrpm")
shapiro_wilk_test(dataset_entrenamiento, "citympg")
shapiro_wilk_test(dataset_entrenamiento, "highwaympg")
shapiro_wilk_test(dataset_entrenamiento, "price")


#8. Diagrama de dispersión

# Crear función para diagrama de dispersión con línea de ajuste
crear_diagrama_dispersión <- function(dataset, x_variable, y_variable) {
  ggplot(dataset, aes(x = .data[[x_variable]], y = .data[[y_variable]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Línea de regresión
    labs(title = paste("Diagrama de Dispersión:", x_variable, "vs", y_variable),
         x = x_variable, y = y_variable) +
    theme_minimal()
}

# Crear diagramas de dispersión con línea de ajuste
crear_diagrama_dispersión(dataset_entrenamiento, "symboling", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "wheelbase", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "carlength", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "carwidth", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "carheight", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "curbweight", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "enginesize", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "boreratio", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "stroke", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "compressionratio", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "horsepower", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "peakrpm", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "citympg", "price")
crear_diagrama_dispersión(dataset_entrenamiento, "highwaympg", "price")


#9. Se encodea las variables categoricas y se eliminan las variables innecesarias para el modelo

variables_insignificativas <- c("ID", "name", "symboling", "aspiration", "enginelocation")
dataset_entrenamiento <- dataset_entrenamiento[, !(names(dataset_entrenamiento) %in% variables_insignificativas)]
dataset_prueba <- dataset_prueba[, !(names(dataset_prueba) %in% variables_insignificativas)]


variables_categoricas <- c("fueltypes", "doornumbers", "carbody", "drivewheels", "enginetype", "cylindernumber", "fuelsystem")
# Función para aplicar One-Hot Encoding
encode_categoricas <- function(dataset) {
  dataset_encoded <- model.matrix(~ fueltypes + doornumbers + carbody + drivewheels + enginetype + cylindernumber + fuelsystem - 1, data = dataset)
  dataset_encoded <- as.data.frame(dataset_encoded)
  dataset <- dataset[, !(colnames(dataset) %in% variables_categoricas)]
  dataset <- cbind(dataset, dataset_encoded)
  return(dataset)
}

# Aplicar la codificación a ambos datasets
dataset_entrenamiento <- encode_categoricas(dataset_entrenamiento)
dataset_prueba <- encode_categoricas(dataset_prueba)


#10. Matriz de correlación


variables_numéricas <- c("wheelbase", "carlength", "carwidth", "carheight", "curbweight", "enginesize",
                         "boreratio", "stroke", "compressionratio", "horsepower", "peakrpm", "citympg", "highwaympg", "price")

dataset_entrenamiento_num <- dataset_entrenamiento[variables_numéricas]
matriz_correlacion <- cor(dataset_entrenamiento_num)


# Configurar los márgenes de la gráfica
par(mar = c(1, 1, 1, 1))

# Visualizar la matriz de correlación
corrplot(matriz_correlacion, 
         method = "color", 
         type = "full", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black")


variables_redundantes <- c("highwaympg", "carwidth", "horsepower", "stroke", "compressionratio", "peakrpm")
dataset_entrenamiento <- dataset_entrenamiento[, !(names(dataset_entrenamiento) %in% variables_redundantes)]
dataset_prueba <- dataset_prueba[, !(names(dataset_prueba) %in% variables_redundantes)]

###################################MODELO DE REGRESION LINEAL##################

#10.5. Algoritmo de Regresión lineal simple

modelo_lineal <- lm(price ~ curbweight, data = dataset_entrenamiento)
predicciones <- predict(modelo_lineal, newdata = dataset_prueba)
summary(modelo_lineal)
residuos <- residuals(modelo_lineal)

# Graficar los residuos vs valores ajustados
plot(fitted(modelo_lineal), residuos, 
     xlab = "Valores Ajustados", 
     ylab = "Residuos",
     main = "Gráfico de Residuos vs Valores Ajustados")
abline(h = 0, col = "red")  

# Calcular el Error Absoluto Medio (MAE)
MAE <- mean(abs(dataset_prueba$price - predicciones))
print(MAE)



###################################MODELO DE REGRESION MULTILINEAL##################

#11. Algoritmo de Regresión multilineal


dataset_prueba <- dataset_prueba[, !names(dataset_prueba) %in% c("carbodyhardtop", "enginetypedohcv", "enginetypeohcv", "cylindernumberfive", "cylindernumbertwelve", "fuelsystem2bbl", "fuelsystemmfi", "drivewheelsrwd", "fueltypesdiesel", "doornumberstwo", "fueltypesgas", "enginetyperotor", "carlength", "fuelsystem4bbl")]
dataset_entrenamiento <- dataset_entrenamiento[, !names(dataset_entrenamiento) %in% c("fueltypesdiesel", "doornumberstwo", "fueltypesgas", "enginetyperotor", "carlength", "fuelsystem4bbl")]

modelo_regresion_multilineal <- lm(price ~ wheelbase +  carheight + curbweight + 
                                     enginesize + boreratio + citympg +  
                                     carbodyhatchback + carbodywagon + 
                                     drivewheelsfwd   + 
                                     enginetypel + enginetypeohcf   + 
                                     cylindernumberfour   + 
                                     cylindernumbertwo +   
                                     fuelsystemidi   + 
                                     fuelsystemspdi, 
                                   data = dataset_entrenamiento)

# Mostrar un resumen del modelo 
summary(modelo_regresion_multilineal)
predicciones <- predict(modelo_regresion_multilineal, newdata = dataset_prueba)

# Ver las primeras predicciones
head(predicciones)
mae <- mean(abs(predicciones - dataset_prueba$price))
print(mae)

###################################MODELO DE REGRESION LOGISTICA##################

#12. Preparación y creación del algoritmo Regresión Logística

dataset_entrenamiento_logistica <- dataset_entrenamiento
dataset_prueba_logistica <- dataset_prueba

dataset_entrenamiento_logistica$price <- ifelse(dataset_entrenamiento_logistica$price < 8500, "normal", "elevado")
dataset_prueba_logistica$price <- ifelse(dataset_prueba_logistica$price < 8500, "normal", "elevado")
dataset_prueba_logistica$price <- factor(dataset_prueba_logistica$price, levels = c("normal", "elevado"))
dataset_entrenamiento_logistica$price <- factor(dataset_entrenamiento_logistica$price, levels = c("normal", "elevado"))


table(dataset_entrenamiento_logistica$price)
table(dataset_prueba_logistica$price)

#Se crea el modelo de regresión logística
modelo_logistica_multinomial <- multinom(price ~ wheelbase +  carheight + curbweight + 
                                           enginesize + boreratio + citympg +  
                                           carbodyhatchback + carbodywagon + 
                                           drivewheelsfwd   + 
                                           enginetypel + enginetypeohcf   + 
                                           cylindernumberfour   + 
                                           cylindernumbertwo +   
                                           fuelsystemidi   + 
                                           fuelsystemspdi, 
                                         data = dataset_entrenamiento_logistica)
summary(modelo_logistica_multinomial)



# Realizar predicciones en el conjunto de prueba
predicciones_prob <- predict(modelo_logistica_multinomial, newdata = dataset_prueba_logistica)
predicciones_prob <- factor(predicciones_prob, levels = c("normal", "elevado"))
matriz_confusion <- confusionMatrix(predicciones_prob, dataset_prueba_logistica$price)

# Imprimir la matriz de confusión y la precisión
print(matriz_confusion)
accuracy <- matriz_confusion$overall['Accuracy']
print(paste("Precisión del modelo:", round(accuracy * 100, 2), "%"))

#gráficarlo
matriz_df <- as.data.frame(matriz_confusion$table)

# Crear la gráfica de la matriz de confusión
ggplot(matriz_df, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Matriz de Confusión", x = "Predicción", y = "Real") +
  theme_minimal()




