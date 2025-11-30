# Cargamos 'dplyr', un paquete esencial para manipular y transformar datos fácilmente.
library(dplyr)
library(randomForest)

datos <- read.csv("actividad2/SP.csv")
datos <- datos[order(as.Date(datos$Date)),]

# Iniciamos una cadena de operaciones ('pipe') sobre el dataframe 'datos' usando dplyr.
datos <- datos %>%
  mutate(
    # Creamos 'Price_Change', nuestro nuevo objetivo a predecir (el cambio porcentual del precio).
    Price_Change = (Real.Price / lag(Real.Price)) - 1,
    # Creamos 'Earnings_Change', el cambio porcentual de las ganancias (ahora es un predictor).
    Earnings_Change = (Real.Earnings / lag(Real.Earnings)) - 1,
    # Creamos 'Dividend_Change', el cambio porcentual de los dividendos.
    Dividend_Change = (Real.Dividend / lag(Real.Dividend)) - 1,
    # Creamos 'Interest_Rate_Change', la diferencia (cambio absoluto) en la tasa de interés.
    Interest_Rate_Change = Long.Interest.Rate - lag(Long.Interest.Rate)
  )

# Eliminamos las primeras filas que ahora tienen 'NA' (datos faltantes) por el cálculo de 'lag'.
datos <- na.omit(datos)

# (La división de datos train/test es igual que antes)
punto_corte <- floor(0.8 * nrow(datos))
train_set <- datos[1:punto_corte, ]
test_set <- datos[(punto_corte + 1):nrow(datos), ]

set.seed(123)

# Entrenamos el Modelo 2 para predecir el *cambio* (`Price_Change`) usando los cambios de los predictores.
rf_modelo_mejorado <- randomForest(Price_Change ~ Earnings_Change + Dividend_Change + Interest_Rate_Change,
                                   data = train_set,
                                   ntree = 500)

# 1. Usamos el modelo 2 para predecir los cambios de precio, no el precio final.
predicted_changes <- predict(rf_modelo_mejorado, newdata = test_set)

# 2. Obtenemos el último precio real conocido (el del final del set de entrenamiento) como punto de partida.
last_known_price <- train_set$Real.Price[nrow(train_set)]

# Creamos un vector vacío para guardar los precios que vamos a reconstruir.
reconstructed_predictions <- numeric(length(predicted_changes))

# 3. Iniciamos un bucle que recorre cada predicción de cambio, una por una, para "reconstruir" el precio.
for (i in 1:length(predicted_changes)) {
  # Calculamos el precio predicho de este mes (Precio_Ayer * (1 + Cambio_Predicho_Hoy)).
  predicted_price <- last_known_price * (1 + predicted_changes[i])
  # Guardamos el precio reconstruido en nuestro vector.
  reconstructed_predictions[i] <- predicted_price
  
  # Actualizamos el 'último precio' para que sea el punto de partida de la siguiente predicción.
  last_known_price <- predicted_price
}

# 4. Creamos la tabla de resultados usando nuestras predicciones reconstruidas.
resultados <- data.frame(
  Fecha = as.Date(test_set$Date),
  Real = test_set$Real.Price,
  Prediccion = reconstructed_predictions
)

# Dibujamos el gráfico, ajustando el eje Y (`ylim`) para que quepan tanto la línea real como la predicha.
plot(resultados$Fecha, resultados$Real, type = 'l', col = 'blue', ylim=range(c(resultados$Real, resultados$Prediccion)),
     xlab = "Fecha", ylab = "Precio Real", main = "Modelo 2: Predicción de Cambios")
lines(resultados$Fecha, resultados$Prediccion, col = 'red', lty = 2)
legend("topleft", legend = c("Valor Real", "Predicción"), col = c("blue", "red"), lty = 1:2)