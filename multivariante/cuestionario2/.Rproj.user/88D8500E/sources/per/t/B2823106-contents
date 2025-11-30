# 1. Cargar las librerías necesarias. 'factoextra' es ideal para crear el biplot.
# Si no están instaladas, ejecutar: install.packages("FactoMineR") y install.packages("factoextra")
library(ade4)
library(FactoMineR)
library(factoextra)

# 2. Cargar el conjunto de datos 'doubs'
data(doubs)

# 3. Asignar el data frame 'env' a una nueva variable
datos <- doubs$env

# 4. Realizar el Análisis de Componentes Principales (ACP)
pca_resultado <- PCA(datos, scale.unit = TRUE, ncp = 5, graph = FALSE)

# 5. Crear el gráfico combinado de variables e individuos (biplot)
fviz_pca_biplot(pca_resultado, repel = TRUE)
