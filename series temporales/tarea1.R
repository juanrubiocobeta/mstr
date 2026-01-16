if (!require("readxl")) install.packages("readxl")
if (!require("tidyr")) install.packages("tidyr")
library(readxl)
library(tidyr)


# A) Datos de Lluvia (Manuales - Agregación)
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
lluvia_mm <- c(52.8, 3.9, 83.1, 49.2, 25.2, 29.9, 
               70.6, 45.2, 48.5, 94.2, 70.1, 74.4)

# B) Datos SPY (Muestreo)
if (file.exists("SPY.xlsx")) {
  datos_spy <- read_excel("SPY.xlsx")
} else {
  message("Selecciona tu archivo SPY.xlsx:")
  datos_spy <- read_excel(file.choose())
}

if (ncol(datos_spy) == 1) {
  message("Separando columnas por comas...")
  datos_spy <- separate(datos_spy, col = 1, sep = ",",
                        into = c("date", "open", "high", "low", "close", "volume"))
  datos_spy$close <- as.numeric(datos_spy$close)
  datos_spy$open  <- as.numeric(datos_spy$open)
}

# Normalizar nombres y fechas
colnames(datos_spy) <- tolower(colnames(datos_spy))
datos_spy$date <- as.Date(datos_spy$date, format = "%m/%d/%Y")

if (all(is.na(datos_spy$date))) {
  datos_spy_raw <- read_excel("SPY.xlsx")
  try(datos_spy$date <- as.Date(datos_spy$date), silent=TRUE)
}

datos_spy <- na.omit(datos_spy)
datos_spy <- datos_spy[order(datos_spy$date), ]
spy_reducido <- tail(datos_spy, 365)

dev.new() # Opcional: abre nueva ventana si prefieres, si no, usa el panel Plots
plot(spy_reducido$date, spy_reducido$close, 
     type = "l", col = "blue", lwd = 2,
     main = "1. Serie por Muestreo: Precio SPY (Cierre Diario)",
     xlab = "Fecha", ylab = "Precio ($)")
grid()
# (Si copias y pegas todo, R generará este, y al instante el siguiente. 
#  Usa las flechas del panel Plots para volver a ver este).


barplot(lluvia_mm, names.arg = meses, col = "forestgreen",
        main = "2. Serie por Agregación: Lluvias Mensuales (Londres)",
        ylab = "Lluvia Acumulada (mm)")
box()
grid(nx=NA, ny=NULL)


rango_y <- range(c(spy_reducido$close, spy_reducido$open))

plot(spy_reducido$date, spy_reducido$close, 
     type = "l", col = "blue", lwd = 2, ylim = rango_y,
     main = "3. Serie Múltiple: Apertura vs Cierre",
     xlab = "Fecha", ylab = "Valor ($)")

# Añadimos la segunda línea
lines(spy_reducido$date, spy_reducido$open, col = "orange", lty = 2, lwd = 2)

legend("topleft", legend=c("Cierre", "Apertura"),
       col=c("blue", "orange"), lty=c(1,2), lwd=2, bg="white")
grid()
