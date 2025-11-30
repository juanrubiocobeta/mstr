datos <- read.table("Genero.txt", header = TRUE)
datos_mca <- subset(datos, select = -Continent)
s <- ncol(datos_mca)
p <- sum(sapply(datos_mca, function(x) length(unique(x))))
total_dimensiones <- p - s
inercia_total <- (p / s) - 1


library(ade4)
datos_mca_factors <- data.frame(lapply(datos_mca, as.factor))
acm_resultado <- dudi.acm(datos_mca_factors, scannf = FALSE, nf = 9)
inercias_ejes <- acm_resultado$eig
inercia_total <- sum(inercias_ejes)
total_dimensiones <- length(inercias_ejes)
inercia_media <- inercia_total / total_dimensiones
ejes_sobre_media <- sum(inercias_ejes > inercia_media)
inercia_dos_primeros_ejes <- sum(inercias_ejes[1:2])
porcentaje_dos_primeros_ejes <- (inercia_dos_primeros_ejes / inercia_total) * 100


library(factoextra)
contrib_plot_1_3 <- fviz_contrib(acm_resultado, choice = "var", axes = c(1, 3))
contrib_data <- contrib_plot_1_3$data
print(contrib_data)
contrib_orden_asc <- contrib_data[order(contrib_data$contrib), ]
contrib_orden_desc <- contrib_data[order(contrib_data$contrib, decreasing = TRUE), ]
max_2_contrib <- head(contrib_orden_desc, 2)
print(max_2_contrib)
min_2_contrib <- head(contrib_orden_asc, 2)
print(min_2_contrib)


library(factoextra)
acm <- acm_resultado
print(
  fviz_mca_var(acm, axes=c(1,2), choice="var.cat", repel=T, 
               gradient.cols=c("yellow","orange","red","blue","black"),
               col.var="contrib")
)




datos_matriz <- matrix(c(40, 15,  9,  5,
                         30, 30, 15,  7,
                         10, 15, 10,  5,
                         5, 10, 15, 10), 
                       nrow = 4, byrow = TRUE)
datos_acs <- as.data.frame(datos_matriz)
colnames(datos_acs) <- c("JIF1", "JIF2", "JIF3", "JIF4")
rownames(datos_acs) <- c("HDI1", "HDI2", "HDI3", "HDI4")
acs_simple <- dudi.coa(datos_acs, scannf = FALSE)
inercias_ejes <- acs_simple$eig
inercia_total <- sum(inercias_ejes)
inercia_eje_1 <- inercias_ejes[1]
porcentaje_eje_1 <- (inercia_eje_1 / inercia_total) * 100
print(
  fviz_ca_biplot(acs_simple, map = "colprincipal", arrow = c(TRUE, TRUE))
)
