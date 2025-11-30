library(smacof)

datos_df <- data.frame(
  c1 = c(0, 569, 667, 530, 141, 140, 357, 396, 570, 190),
  c2 = c(569, 0, 1212, 1043, 617, 446, 325, 423, 787, 648),
  c3 = c(667, 1212, 0, 201, 596, 768, 923, 882, 714, 714),
  c4 = c(530, 1043, 201, 0, 431, 608, 740, 690, 516, 622),
  c5 = c(141, 617, 596, 431, 0, 177, 340, 337, 436, 320),
  c6 = c(140, 446, 768, 608, 177, 0, 218, 272, 519, 302),
  c7 = c(357, 325, 923, 740, 340, 218, 0, 114, 472, 514),
  c8 = c(396, 423, 882, 690, 337, 272, 114, 0, 364, 573),
  c9 = c(569, 787, 714, 516, 436, 519, 472, 364, 0, 755),
  c10 = c(190, 648, 714, 622, 320, 302, 514, 573, 755, 0)
)

city_names <- c("Londres", "Estocolmo", "Lisboa", "Madrid", "Paris", 
                "Amsterdam", "Berlin", "Praga", "Roma", "Dublin")

rownames(datos_df) <- city_names
colnames(datos_df) <- city_names

datos_dist <- as.dist(datos_df)

res.metrico <- smacofSym(datos_dist, ndim = 2, type = "ratio")
print(res.metrico)

res.nometrico <- smacofSym(datos_dist, ndim = 2, type = "ordinal")
print(res.nometrico)


plot(res.metrico$conf[, 1], -res.metrico$conf[, 2], 
     type = "n", main = "MDS Métrico", asp = 1)
text(res.metrico$conf[, 1], -res.metrico$conf[, 2], 
     labels = city_names, cex = 0.8)

plot(res.nometrico$conf[, 1], -res.nometrico$conf[, 2], 
     type = "n", main = "MDS No Métrico", asp = 1)
text(res.nometrico$conf[, 1], -res.nometrico$conf[, 2], 
     labels = city_names, cex = 0.8)


