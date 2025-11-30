set.seed(42)
datos_normales <- rnorm(n = 1000, mean = 0, sd = 1)

# Crear el histograma
hist(datos_normales,
     main = "Histograma de Datos Normales Simulados",
     xlab = "Valor",
     ylab = "Frecuencia",
     col = "#0072B2",
     border = "white"
)
