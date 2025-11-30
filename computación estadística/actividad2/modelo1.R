# Cargamos el paquete 'randomForest' para poder usar este algoritmo.
library(randomForest)

# Leemos los datos desde nuestro archivo CSV.
datos <- read.csv("actividad2/SP.csv")

# Ordenamos todos los datos por fecha, del más antiguo al más reciente.
datos <- datos[order(as.Date(datos$Date)),]

# Calculamos dónde cortar los datos (el 80%) para separar entrenamiento y prueba.
punto_corte <- floor(0.8 * nrow(datos))

# Creamos el set de entrenamiento con el primer 80% de los datos (el pasado).
train_set <- datos[1:punto_corte, ]

# Creamos el set de prueba con el 20% final de los datos (el futuro).
test_set <- datos[(punto_corte + 1):nrow(datos), ]

# Fijamos una semilla (123) para que el modelo siempre dé el mismo resultado al ejecutarse.
set.seed(123) 

# Entrenamos el Modelo 1 (Random Forest) para predecir el precio (`Real.Price`) usando los datos de entrenamiento.
rf_modelo <- randomForest(Real.Price ~ Real.Earnings + Real.Dividend + Long.Interest.Rate,
                          data = train_set,
                          ntree = 500,      
                          na.action = na.omit) 

# Mostramos un resumen de cómo se ha construido el modelo.
print(rf_modelo)

# Usamos el modelo entrenado para predecir los precios en el set de prueba (los datos que nunca ha visto).
predicciones <- predict(rf_modelo, newdata = test_set)

# Creamos una nueva tabla 'resultados' para comparar fácilmente los valores reales con los predichos.
resultados <- data.frame(
  Fecha = as.Date(test_set$Date),
  Real = test_set$Real.Price,
  Prediccion = predicciones
)

# Dibujamos la línea azul, que representa el precio real del S&P 500.
plot(resultados$Fecha, resultados$Real, type = 'l', col = 'blue',
     xlab = "Fecha", ylab = "Precio Real", main = "Modelo 1: Predicción vs. Valor Real")

# Añadimos la línea roja discontinua, que representa la predicción de nuestro modelo.
lines(resultados$Fecha, resultados$Prediccion, col = 'red', lty = 2)

# Añadimos una leyenda al gráfico para saber qué es cada línea.
legend("topleft", legend = c("Valor Real", "Predicción"), col = c("blue", "red"), lty = 1:2)

# Calculamos qué variables (Earnings, Dividend, etc.) han sido más importantes para el modelo.
importancia <- importance(rf_modelo)

# Mostramos la puntuación de importancia de cada variable.
print(importancia)