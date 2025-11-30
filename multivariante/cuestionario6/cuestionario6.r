library(MASS)
datos_test <- read.table("test_datos_discriminante.txt", 
                         header = TRUE, 
                         sep = "\t",
                         dec = ",")
datos_test$ansiedad <- as.factor(datos_test$ansiedad)


modelo_lda_completo <- lda(ansiedad ~ x1 + x2 + x3 + x4 + x5 + x6, data = datos_test)

print(modelo_lda_completo)
print(round(modelo_lda_completo$means, 2))
print(modelo_lda_completo$svd^2 / sum(modelo_lda_completo$svd^2))

coeficientes_ld1 <- modelo_lda_completo$scaling[, 1]
centroide_g3 <- modelo_lda_completo$means[3, ]
puntuacion_discriminante_g3_ld1 <- sum(centroide_g3 * coeficientes_ld1)
print(round(puntuacion_discriminante_g3_ld1, 2))


nuevos_datos_x1x2 <- data.frame(
  x1 = c(8, 5.8), 
  x2 = c(5, 11.3)
)
modelo_lda_x1x2 <- lda(ansiedad ~ x1 + x2, data = datos_test)
predicciones_nuevas_x1x2 <- predict(modelo_lda_x1x2, newdata = nuevos_datos_x1x2)

print(predicciones_nuevas_x1x2$posterior)

predicciones_completo <- predict(modelo_lda_completo)
matriz_confusion_completo <- table(Clasificación_Predicha = predicciones_completo$class, 
                                 Grupo_Real = datos_test$ansiedad)
error_total <- 1 - sum(diag(matriz_confusion_completo)) / sum(matriz_confusion_completo)
error_por_grupo <- numeric(ncol(matriz_confusion_completo))
nombres_grupos <- colnames(matriz_confusion_completo)

for (i in 1:ncol(matriz_confusion_completo)) {
  total_grupo_i <- sum(matriz_confusion_completo[, i]) 
  mal_clasificadas_i <- total_grupo_i - matriz_confusion_completo[i, i]
  error_por_grupo[i] <- mal_clasificadas_i / total_grupo_i
}
names(error_por_grupo) <- paste("Error Grupo", nombres_grupos)

print(matriz_confusion_completo)
print(round(error_total * 100, 2))
print(round(error_por_grupo * 100, 2))

nuevo_individuo_x1x2x3 <- data.frame(
  x1 = 7,
  x2 = 8,
  x3 = 9.8
)
modelo_lda_x1x2x3 <- lda(ansiedad ~ x1 + x2 + x3, data = datos_test)
predicciones_x1x2x3 <- predict(modelo_lda_x1x2x3, newdata = nuevo_individuo_x1x2x3)

print(predicciones_x1x2x3$posterior)

nuevo_individuo_x5x6 <- data.frame(
  x5 = 2,
  x6 = 50
)

modelo_lda_x5x6 <- lda(ansiedad ~ x5 + x6, data = datos_test)
predicciones_x5x6 <- predict(modelo_lda_x5x6) # Para Matriz de Confusión (Pregunta 10)
prediccion_ind_x5x6 <- predict(modelo_lda_x5x6, newdata = nuevo_individuo_x5x6) # Para Predicción (Pregunta 5)

matriz_confusion_x5x6 <- table(Clasificación_Predicha = predicciones_x5x6$class, 
                                 Grupo_Real = datos_test$ansiedad)
print(matriz_confusion_x5x6)

probabilidades_x5x6 <- prediccion_ind_x5x6$posterior
maxima_probabilidad <- max(probabilidades_x5x6)
error_clasificacion <- 1 - maxima_probabilidad

print(probabilidades_x5x6)
print(maxima_probabilidad)
print(error_clasificacion)