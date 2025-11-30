install.packages("readxl")
install.packages("CCA")
install.packages("CCP")

library(readxl)
library(CCA)
library(CCP)

datos <- read_excel("BaseCuestionario.xlsx")

datos.feto <- datos[c("DBP34", "PC34", "PA34")]
datos.rn <- datos[c("PESO_RN", "TALLA_RN", "PC_RN", "PT_RN")]

matriz.correlacion <- matcor(datos.feto, datos.rn)

matriz.correlacion$Ycor

cca.datos <- cc(datos.feto, datos.rn)
round(cca.datos$cor[1], 4)

cca.datos$scores$corr.X.xscores[, 2]




rho <- cca.datos$cor
n <- dim(datos)[1]
p <- length(datos.feto)
q <- length(datos.rn)

p.asym(rho, n, p, q, tstat = "Wilks")

cca.datos$ycoef[, 1]


cca.datos$scores$corr.X.xscores[, 1]
cca.datos$scores$corr.Y.yscores[, 1]
