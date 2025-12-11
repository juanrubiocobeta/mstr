library(readxl)
# 1. Cargar datos
datos <- read_excel("datos_culturales.xlsx")
datos_num <- datos[, -1] # Eliminar columna de nombres

# 2. Ejecutar K-means para k=3 y k=4 (usamos nstart alto para estabilidad)
set.seed(123) # Semilla para reproducibilidad
km3 <- kmeans(datos_num, centers = 3, nstart = 25)
km4 <- kmeans(datos_num, centers = 4, nstart = 25)

# 3. Calcular el porcentaje de varianza explicada (Betweenss / Totss)
prop3 <- (km3$betweenss / km3$totss) * 100
prop4 <- (km4$betweenss / km4$totss) * 100

# 4. Imprimir resultados
cat("Para 3 centros (k=3):", round(prop3, 1), "%\n")
cat("Para 4 centros (k=4):", round(prop4, 1), "%\n")
