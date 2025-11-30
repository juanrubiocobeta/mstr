# Cargamos los paquetes necesarios para manipular datos (dplyr), crear el modelo (randomForest) y calcular la media móvil (TTR).
library(dplyr)
library(randomForest)
library(TTR)

# 1. Carga y Preparación de Datos

# Cargamos los datos del CSV.
datos <- read.csv("actividad2/SP.csv")
# Ordenamos los datos por fecha, de más antiguo a más reciente.
datos <- datos[order(as.Date(datos$Date)), ]

# 2. Ingeniería de Variables (Feature Engineering)

# Transformamos los datos para predecir el 'cambio' en lugar del 'precio'.
datos <- datos %>%
  mutate(
    # Variable objetivo: cambio porcentual del precio.
    Price_Change = (Real.Price / lag(Real.Price)) - 1,
    # Predictor: cambio porcentual de las ganancias.
    Earnings_Change = (Real.Earnings / lag(Real.Earnings)) - 1,
    # Predictor: cambio porcentual de los dividendos.
    Dividend_Change = (Real.Dividend / lag(Real.Dividend)) - 1,
    # Predictor: cambio absoluto de la tasa de interés.
    Interest_Rate_Change = Long.Interest.Rate - lag(Long.Interest.Rate),
    
    # Variables de Contexto (La Novedad del Modelo 3)
    # Predictor de 'Memoria': el cambio de precio del mes anterior.
    Price_Change_Lag1 = lag(Price_Change, 1),
    # Predictor de 'Tendencia': la media móvil de los últimos 10 cambios de precio.
    SMA_10_Price_Change = SMA(Price_Change, n = 10)
  )

# Eliminamos las filas con NAs creadas por las funciones 'lag' y 'SMA'.
datos <- na.omit(datos)

# 3. División de Datos (Train/Test)

# Definimos el punto de corte (80% para entrenar, 20% para probar).
punto_corte <- floor(0.8 * nrow(datos))
# Creamos el set de entrenamiento (el pasado).
train_set <- datos[1:punto_corte, ]
# Creamos el set de prueba (el futuro).
test_set <- datos[(punto_corte + 1):nrow(datos), ]

# Fijamos una semilla para que el modelo sea reproducible.
set.seed(123)

# 4. Entrenamiento del Modelo

# Entrenamos el Modelo 3, prediciendo 'Price_Change' con todas nuestras variables.
rf_modelo_final <- randomForest(
  Price_Change ~ Earnings_Change + Dividend_Change + Interest_Rate_Change + Price_Change_Lag1 + SMA_10_Price_Change,
  data = train_set,
  ntree = 500,
  # 'nodesize' y 'mtry' son ajustes para que el modelo sea más general y estable.
  nodesize = 10,  
  mtry = 3        
)

# 5. Predicción y Reconstrucción

# 1. Predecimos los cambios de precio en el set de prueba.
predicted_changes <- predict(rf_modelo_final, newdata = test_set)

# 2. Creamos un vector vacío para guardar nuestros precios reconstruidos.
reconstructed_predictions <- numeric(length(predicted_changes))

# Este bucle calcula la predicción de cada mes basándose en el precio REAL del mes anterior.
for (i in 1:length(predicted_changes)) {
  
# 3. Identificamos el precio "base" REAL sobre el que calcular el cambio.
  if (i == 1) {
    # Para la primera predicción, el precio base es el último real del set de entrenamiento.
    base_price <- train_set$Real.Price[nrow(train_set)]
  } else {
    # Para todas las demás, el precio base es el valor REAL del mes anterior (del set de prueba).
    base_price <- test_set$Real.Price[i - 1] 
  }
  
# 4. Calculamos el precio predicho (Precio_Base_Real * (1 + Cambio_Predicho)).
  predicted_price <- base_price * (1 + predicted_changes[i])
  
# 5. Guardamos la predicción en nuestro vector.
  reconstructed_predictions[i] <- predicted_price
  
}


# 6. Resultados y Gráfico

# Creamos un dataframe final para comparar lo Real vs. la Predicción.
resultados <- data.frame(
  Fecha = as.Date(test_set$Date),
  Real = test_set$Real.Price,
  Prediccion = reconstructed_predictions
)

# Dibujamos el gráfico (que ahora debería ajustarse mucho mejor).
plot(resultados$Fecha, resultados$Real, type = 'l', col = 'blue', ylim=range(c(resultados$Real, resultados$Prediccion)),
     xlab = "Fecha", ylab = "Precio Real", main = "Modelo 3: Predicción Corregida")
# Añadimos la línea de predicción.
lines(resultados$Fecha, resultados$Prediccion, col = 'red', lty = 2)
# Añadimos la leyenda.
legend("topleft", legend = c("Valor Real", "Predicción"), col = c("blue", "red"), lty = 1:2)



# 1. Calculamos la matriz de importancia de las variables
importancia_modelo <- importance(rf_modelo_final)
print("Matriz de Importancia de Variables (%IncMSE):")
print(importancia_modelo)
varImpPlot(rf_modelo_final, main = "Importancia de Variables")




residuos <- resultados$Real - resultados$Prediccion
plot(resultados$Fecha, residuos, 
     type = 'h', # 'h' dibuja líneas verticales desde y=0, ideal para residuos
     col = "blue",
     main = "Diagnóstico: Residuos del Modelo 3 (vs. Tiempo)",
     xlab = "Fecha",
     ylab = "Error de Predicción (Residuos)")
abline(h = 0, col = "red", lty = 2)



#Intento de indicador nuevo, no explicado en el informe:

comparacion_df <- data.frame(
  Cambio_Real = test_set$Price_Change,
  Cambio_Predicho = predicted_changes
)
comparacion_df$Direccion_Real <- sign(comparacion_df$Cambio_Real)
comparacion_df$Direccion_Predicha <- sign(comparacion_df$Cambio_Predicho)

# 3. Comparamos si las direcciones coinciden
comparacion_df <- comparacion_df[comparacion_df$Direccion_Real != 0 & comparacion_df$Direccion_Predicha != 0, ]
comparacion_df$Acierto <- (comparacion_df$Direccion_Real == comparacion_df$Direccion_Predicha)

# 4. Calculamos el "Porcentaje de Éxito" (la media de aciertos)
precision_direccional <- mean(comparacion_df$Acierto, na.rm = TRUE)

# 5. Mostramos el resultado
print(paste0("Porcentaje de Éxito: ", 
             round(precision_direccional * 100, 3), 
             "%"))
