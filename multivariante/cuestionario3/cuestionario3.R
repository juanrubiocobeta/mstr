datos <- read.table("factorial2.txt", header = TRUE)
install.packages("mvnormtest")
library(mvnormtest)

mshapiro.test(t(datos))

fac <- prcomp(datos, center = TRUE, scale. = TRUE)

summary(fac)

loadings_F1 <- fac$rotation[, 1] * fac$sdev[1]

loadings_F2 <- fac$rotation[, 2] * fac$sdev[2]

communalities_m2 <- loadings_F1^2 + loadings_F2^2

names(communalities_m2) <- colnames(datos)

print(communalities_m2)

biplot(fac)

install.packages("psych")

library(psych)

datos_f3 <- read.table("factorial3.txt", header = TRUE)

KMO(datos_f3)

fac_f3 <- prcomp(datos_f3, center = TRUE, scale. = TRUE)

summary(fac_f3)
  
biplot(fac_f3)
