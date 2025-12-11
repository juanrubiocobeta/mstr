
datos <- read.csv("datos_culturales.xlsx", header=TRUE, sep=",")

rownames(datos) <- datos[, 1] 
datos_numericos <- datos[, -1] 

dist_manhattan <- dist(datos_numericos, method = "manhattan")

matriz_m <- as.matrix(dist_manhattan)

valor_ceuta_rioja <- matriz_m["Ceuta y Melilla", "Rioja (La)"]

max_total <- max(matriz_m)

cat("Distancia Manhattan entre Ceuta y Melilla y La Rioja:", valor_ceuta_rioja, "\n")
cat("Distancia Máxima en toda la matriz:", max_total, "\n")

if (valor_ceuta_rioja == max_total) {
  cat("CONCLUSIÓN: Es la distancia más elevada.\n")
  cat("En un cluster jerárquico, los elementos MÁS LEJANOS (distancia máxima) se unen LOS ÚLTIMOS.\n")
} else {
  cat("No es la distancia máxima.\n")
}