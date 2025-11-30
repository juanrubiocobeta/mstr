#----------------------------------------------------------------
# Ejercicio (f): Filtro de Kalman y Suavizador Punto Fijo en R
#----------------------------------------------------------------
#-- 0. Definicion de Parametros del Modelo-
# Modelo escalar:
# x(k+1) = Phi * x(k) + w(k)
# z(k) = H * x(k) + v(k)
# Asumimos Gamma = 1 (implicito en x(k+1) = ... + w(k))
Phi <- 0.95
H <- 1
Q <- 0.1 # Varianza de w(k)
R <- 0.5 # Varianza de v(k)
P0 <- 1 # Varianza de x(0)
x0_media <- 0 # Media de x(0)
#-- Parametros de Simulacion y Suavizado-
k_fixed <- 20 # Instante k que queremos suavizar (k=20)
N_horizon <- 30 # Horizonte de suavizado N (j = k+1...k+N)
N_total <- k_fixed + N_horizon # Instante final
# Usaremos N_steps = N_total + 1 para incluir el k=0
# El indice R [i] correspondera al tiempo k = i-1
# k=0-> i=1
# k=N_total-> i=N_total+1
N_steps <- N_total + 1
#-- 1. Simulacion del Sistema (Generar datos "reales")-
x_true <- numeric(N_steps) # Vector para el estado real x(k)
z_obs <- numeric(N_steps) # Vector para la observacion z(k)
# Estado inicial k=0 (indice 1)
x_true[1] <- x0_media + sqrt(P0) * rnorm(1)
z_obs[1] <- H * x_true[1] + sqrt(R) * rnorm(1)
# Simular k=1 hasta k=N_total (indice 2 hasta N_steps)
for (i in 2:N_steps) {
  # Ecuacion del estado
  x_true[i] <- Phi * x_true[i-1] + sqrt(Q) * rnorm(1)
  # Ecuacion de observacion
  z_obs[i] <- H * x_true[i] + sqrt(R) * rnorm(1)
}
#-- 2. FASE 1: Ejecucion del Filtro de Kalman (Teorema 2)-
# Vectores para almacenar resultados del filtro
x_pred <- numeric(N_steps) # Almacena x_hat(k/k-1)
P_pred <- numeric(N_steps) # Almacena P(k/k-1)
x_fil <- numeric(N_steps) # Almacena x_hat(k/k)
P_fil <- numeric(N_steps) # Almacena P(k/k)
K_gain <- numeric(N_steps) # Almacena K(k)
innov <- numeric(N_steps) # Almacena z_tilde(k/k-1)
Pi_cov <- numeric(N_steps) # Almacena Cov(innov) = Pi(k)
# Inicializacion k=0 (indice 1)
x_pred[1] <- x0_media # x_hat(0/-1) = E[x0] = 0
P_pred[1] <- P0
# P(0/-1) = P0
# Actualizacion de medida k=0
Pi_cov[1] <- H * P_pred[1] * H + R
K_gain[1] <- P_pred[1] * H / Pi_cov[1] #
innov[1] <- z_obs[1]- H * x_pred[1]
x_fil[1] <- x_pred[1] + K_gain[1] * innov[1] #
P_fil[1] <- (1- K_gain[1] * H) * P_pred[1] #
# Bucle del Filtro k=1 hasta k=N_total (indice 2 hasta N_steps)
for (i in 2:N_steps) {
  #--- Prediccion (Ciclo de Tiempo)--
  x_pred[i] <- Phi * x_fil[i-1] #
  P_pred[i] <- Phi * P_fil[i-1] * Phi + Q # (con Gamma=1)
  #--- Actualizacion (Ciclo de Medida)--
  # Covarianza de la innovacion
  Pi_cov[i] <- H * P_pred[i] * H + R
  # Ganancia de Kalman
  K_gain[i] <- P_pred[i] * H / Pi_cov[i]
  # Innovacion
  innov[i] <- z_obs[i]- H * x_pred[i]
  # Filtro
  x_fil[i] <- x_pred[i] + K_gain[i] * innov[i]
  # Covarianza del filtro
  P_fil[i] <- (1- K_gain[i] * H) * P_pred[i]
}
#-- 3. FASE 2: Suavizador Punto Fijo (Sec. 4.1)-
# Queremos suavizar en k = k_fixed (indice R i_fixed)
i_fixed <- k_fixed + 1
# Queremos iterar j desde k+1 hasta k+N (indice R j_idx)
j_start_idx <- i_fixed + 1
j_end_idx <- N_steps
# Vectores para almacenar los resultados del suavizador
# El tamano es N_horizon + 1 (para incluir j=k)
n_smooth_steps <- N_horizon + 1
x_smooth <- numeric(n_smooth_steps) # x_hat(k_fixed / j)
P_smooth <- numeric(n_smooth_steps) # P(k_fixed / j)
# Inicializacion en j = k_fixed (indice 1 del vector smooth)
x_smooth[1] <- x_fil[i_fixed] #
P_smooth[1] <- P_fil[i_fixed] #
L <- P_fil[i_fixed]
# L(k,k) = P(k/k)
# Bucle del Suavizador: j desde k+1 hasta N_total
# Indice R: j_idx desde j_start_idx hasta j_end_idx
# Indice Smooth: s_idx desde 2 hasta n_smooth_steps
s_idx <- 1 # Indice para los vectores x_smooth/P_smooth
for (j_idx in j_start_idx:j_end_idx) {
  s_idx <- s_idx + 1 # Avanza el indice del suavizador (2, 3, ...)
  # Ganancia del Suavizador K(k,j)
  # (Pi_cov[j_idx] = H*P_pred[j_idx]*H + R, ya calculado)
  K_smooth_j <- L * Phi * H / Pi_cov[j_idx]
  # Actualizacion del Estimador Suavizado
  x_smooth[s_idx] <- x_smooth[s_idx-1] + K_smooth_j * innov[j_idx]
  # Actualizacion de la Covarianza Suavizada
  P_smooth[s_idx] <- P_smooth[s_idx-1]- K_smooth_j * Pi_cov[j_idx] * K_smooth_j
  # Actualizacion de L para la siguiente iteracion (j-> j+1)
  L <- L * Phi * (1- K_gain[j_idx] * H)
}
#--- Resultados--
# x_smooth contiene x_hat(k_fixed/k_fixed), ..., x_hat(k_fixed/k_fixed+N)
# P_smooth contiene P(k_fixed/k_fixed), ..., P(k_fixed/k_fixed+N)
print(paste("Estado real en k =", k_fixed, ":", x_true[i_fixed]))
print(paste("Estimacion filtrada x_hat(k/k):", x_smooth[1]))
print(paste("Estimacion suavizada x_hat(k/k+N):", x_smooth[n_smooth_steps]))
print("Varianza del error de filtrado P(k/k):")
print(P_smooth[1])
print("Varianza del error de suavizado P(k/k+N):")
print(P_smooth[n_smooth_steps])



















library(ggplot2)
library(tidyr)
# -----------------------------------------------------------------
# Apartado (g): Graficacion de Filtro y Suavizado P. Fijo
# -----------------------------------------------------------------

# -- 0. Definicion de Parametros del Modelo --
Phi <- 0.95
H   <- 1
Q   <- 0.1  # Varianza de w(k)
R   <- 0.5  # Varianza de v(k)
P0  <- 1    # Varianza de x(0)
x0_media <- 0 # Media de x(0)

# -- Parametros de Simulacion --
N_iter    <- 50 # 50 iteraciones (k=0 hasta k=50 -> 51 pasos)
N_steps   <- N_iter + 1
N_horizons <- c(1, 2, 4) # Horizontes N para suavizado
N_max     <- max(N_horizons)

# Generamos un vector de tiempo
time <- 0:N_iter

# -- 1. Simulacion del Sistema (Generar datos "reales") --

x_true <- numeric(N_steps) # Vector para el estado real x(k)
z_obs  <- numeric(N_steps) # Vector para la observacion z(k)

# Fijamos la semilla para reproducibilidad
set.seed(123)

# Estado inicial k=0 (indice 1)
x_true[1] <- x0_media + sqrt(P0) * rnorm(1)
z_obs[1]  <- H * x_true[1] + sqrt(R) * rnorm(1)

# Simular k=1 hasta k=N_iter (indice 2 hasta N_steps)
for (i in 2:N_steps) {
  # Ecuacion del estado
  x_true[i] <- Phi * x_true[i-1] + sqrt(Q) * rnorm(1)
  # Ecuacion de observacion
  z_obs[i]  <- H * x_true[i] + sqrt(R) * rnorm(1)
}


# -- 2. FASE 1: Ejecucion del Filtro de Kalman (Teorema 2) --

# Vectores para almacenar resultados del filtro
x_pred <- numeric(N_steps) # Almacena x_hat(k/k-1)
P_pred <- numeric(N_steps) # Almacena P(k/k-1)
x_fil  <- numeric(N_steps) # Almacena x_hat(k/k)
P_fil  <- numeric(N_steps) # Almacena P(k/k)
K_gain <- numeric(N_steps) # Almacena K(k)
innov  <- numeric(N_steps) # Almacena z_tilde(k/k-1)
Pi_cov <- numeric(N_steps) # Almacena Cov(innov) = Pi(k)

# Inicializacion k=0 (indice 1)
x_pred[1] <- x0_media
P_pred[1] <- P0
Pi_cov[1] <- H * P_pred[1] * H + R
K_gain[1] <- P_pred[1] * H / Pi_cov[1]
innov[1]  <- z_obs[1] - H * x_pred[1]
x_fil[1]  <- x_pred[1] + K_gain[1] * innov[1]
P_fil[1]  <- (1 - K_gain[1] * H) * P_pred[1]

# Bucle del Filtro k=1 hasta k=N_iter (indice 2 hasta N_steps)
for (i in 2:N_steps) {
  x_pred[i] <- Phi * x_fil[i-1]
  P_pred[i] <- Phi * P_fil[i-1] * Phi + Q
  Pi_cov[i] <- H * P_pred[i] * H + R
  K_gain[i] <- P_pred[i] * H / Pi_cov[i]
  innov[i]  <- z_obs[i] - H * x_pred[i]
  x_fil[i]  <- x_pred[i] + K_gain[i] * innov[i]
  P_fil[i]  <- (1 - K_gain[i] * H) * P_pred[i]
}


# -- 3. FASE 2: Suavizador Punto Fijo (Sec. 4.1) Iterativo --

# Almacenamos los resultados en matrices
# (NAs por defecto para los puntos que no se pueden calcular)
x_smooth <- matrix(NA, nrow = N_steps, ncol = length(N_horizons))
P_smooth <- matrix(NA, nrow = N_steps, ncol = length(N_horizons))
colnames(x_smooth) <- paste0("N=", N_horizons)
colnames(P_smooth) <- paste0("N=", N_horizons)

# Bucle EXTERNO: Itera sobre cada punto k que queremos suavizar
for (i in 1:N_steps) {
  k <- i - 1 # Tiempo k
  
  # Inicializamos el suavizador en j=k
  x_smooth_current <- x_fil[i] # x_hat(k/k)
  P_smooth_current <- P_fil[i] # P(k/k)
  L_current <- P_fil[i]        # L(k,k)
  
  # Bucle INTERNO: Itera hacia el futuro (j = k+1, ..., k+N_max)
  for (j_offset in 1:N_max) {
    j_idx <- i + j_offset # Indice de j (j=k+j_offset)
    
    # Si j esta fuera de nuestros datos, paramos
    if (j_idx > N_steps) {
      break
    }
    
    # Ecuaciones del Suavizador P. Fijo (Sec. 4.1)
    # Ganancia K(k,j)
    K_smooth_j <- L_current * Phi * H / Pi_cov[j_idx]
    
    # Estimador x_hat(k/j)
    x_smooth_current <- x_smooth_current + K_smooth_j * innov[j_idx]
    
    # Varianza P(k/j)
    P_smooth_current <- P_smooth_current - K_smooth_j * Pi_cov[j_idx] * K_smooth_j
    
    # Matriz L(k,j) para la siguiente iteracion
    L_current <- L_current * Phi * (1 - K_gain[j_idx] * H)
    
    # Almacenamos el resultado si N=j_offset es uno de los que buscamos
    if (j_offset %in% N_horizons) {
      col_idx <- which(N_horizons == j_offset)
      x_smooth[i, col_idx] <- x_smooth_current
      P_smooth[i, col_idx] <- P_smooth_current
    }
  }
}


# -- 4. Preparacion de Datos y Graficacion --

# --- GRAFICA 1: Trayectorias (CORREGIDA) ---
data_g1 <- data.frame(
  Tiempo = time,
  Estado_Real = x_true,
  Observacion = z_obs,
  Filtrado = x_fil,
  Suavizado_N2 = x_smooth[, "N=2"]
)

# Convertimos a formato "largo" para ggplot
data_g1_long <- tidyr::pivot_longer(
  data_g1,
  cols = -Tiempo,
  names_to = "Serie",
  values_to = "Valor"
)

# Creamos la Grafica 1 (Corregida)
g1 <- ggplot(data_g1_long, aes(x = Tiempo, y = Valor, color = Serie)) +
  geom_line(aes(linetype = Serie)) +
  scale_linetype_manual(values = c(
    "Estado_Real" = "solid", 
    "Observacion" = "dotted", 
    "Filtrado" = "dashed", 
    "Suavizado_N2" = "solid"
  )) +
  scale_color_manual(values = c(
    "Estado_Real" = "black", 
    "Observacion" = "grey50", 
    "Filtrado" = "blue", 
    "Suavizado_N2" = "red"
  )) +
  labs(
    title = "Comparativa de Estimaciones (N=2)",
    x = "Tiempo (k)",
    y = "Valor",
    color = "Series",    # <--- Titulo para la leyenda de color
    linetype = "Series"  # <--- EXACTO MISMO Titulo para la leyenda de linea
  ) +
  theme_minimal()

# --- GRAFICA 2: Varianzas ---
data_g2 <- data.frame(
  Tiempo = time,
  Filtrado = P_fil,
  Suavizado_N1 = P_smooth[, "N=1"],
  Suavizado_N2 = P_smooth[, "N=2"],
  Suavizado_N4 = P_smooth[, "N=4"]
)

# Convertimos a formato "largo"
data_g2_long <- tidyr::pivot_longer(
  data_g2,
  cols = -Tiempo,
  names_to = "Serie",
  values_to = "Varianza"
)

# Creamos la Grafica 2
g2 <- ggplot(data_g2_long, aes(x = Tiempo, y = Varianza, color = Serie)) +
  geom_line() +
  labs(
    title = "Comparativa de Varianzas del Error",
    x = "Tiempo (k)",
    y = "Varianza del Error (P)",
    color = "Estimador"
  ) +
  coord_cartesian(ylim = c(0, NA)) + # Asegura que el eje Y empiece en 0
  theme_minimal()

# --- Mostrar Graficas ---
print(g1)
print(g2)





# --- 0. Cargar Librerías ---
library(ggplot2)
library(tidyr)

# --- 1. Definición de Parámetros ---
N_steps <- 20 # 20 primeros valores (k=0 a 19)
k_values <- 0:(N_steps - 1)

# Parámetros para la Trayectoria 1 (Bajo ruido de observación)
P0_1 <- 1.0
R_1  <- 0.1

# Parámetros para la Trayectoria 2 (Alto ruido de observación)
P0_2 <- 1.0 # Cambiamos P0 para que la amplitud sea distinta
R_2  <- 2.0

# Fijamos semilla para que los resultados sean reproducibles
set.seed(456)


# --- 2. Simulación de las Dos Trayectorias (Corregido) ---

# NOTA: La ecuación de estado es x(k+1) = (-1)^(2k+1) * x(k)
# 2k+1 es siempre impar, por lo que (-1)^(2k+1) = -1.
# La dinámica real es: x(k+1) = -x(k)

# Vectores para almacenar los datos
x_true_1 <- numeric(N_steps)
z_obs_1  <- numeric(N_steps)
x_true_2 <- numeric(N_steps)
z_obs_2  <- numeric(N_steps)

# Trayectoria 1 (Bajo Ruido)
x0_1 <- rnorm(1, mean = 0, sd = sqrt(P0_1))
x_true_1[1] <- x0_1 # k=0
v_1 <- rnorm(N_steps, mean = 0, sd = sqrt(R_1))
z_obs_1[1] <- x_true_1[1] + v_1[1]

# Trayectoria 2 (Alto Ruido)
x0_2 <- rnorm(1, mean = 0, sd = sqrt(P0_2))
x_true_2[1] <- x0_2 # k=0
v_2 <- rnorm(N_steps, mean = 0, sd = sqrt(R_2))
z_obs_2[1] <- x_true_2[1] + v_2[1]


# Simular k=1 hasta k=19 (índice 2 hasta 20)
for (i in 2:N_steps) {
  # Trayectoria 1
  x_true_1[i] <- -x_true_1[i-1] # Dinámica x(k) = -x(k-1)
  z_obs_1[i]  <- x_true_1[i] + v_1[i]
  
  # Trayectoria 2
  x_true_2[i] <- -x_true_2[i-1] # Dinámica x(k) = -x(k-1)
  z_obs_2[i]  <- x_true_2[i] + v_2[i]
}

# Creamos los DataFrames
df_1 <- data.frame(k = k_values, 
                   x_true = x_true_1, 
                   z_obs = z_obs_1, 
                   Escenario = "Trayectoria 1: R bajo (0.1)")

df_2 <- data.frame(k = k_values, 
                   x_true = x_true_2, 
                   z_obs = z_obs_2, 
                   Escenario = "Trayectoria 2: R alto (2.0)")

# Combinamos los datos
df_total <- rbind(df_1, df_2)


# --- 3. Preparación de Datos y Gráfica ---

# Convertimos a formato largo para ggplot
df_long <- tidyr::pivot_longer(df_total, 
                               cols = c(x_true, z_obs), 
                               names_to = "Serie", 
                               values_to = "Valor")

# Renombramos las series para la leyenda
df_long$Serie <- factor(df_long$Serie, 
                        levels = c("x_true", "z_obs"), 
                        labels = c("Estado Real (x_k)", "Observación (z_k)"))

# Generamos la gráfica
g <- ggplot(df_long, aes(x = k, y = Valor, color = Serie)) +
  # Usamos línea sólida para el estado real y punteada para la observación
  geom_line(aes(linetype = Serie), size = 0.8) +
  scale_linetype_manual(values = c("Estado Real (x_k)" = "solid", 
                                   "Observación (z_k)" = "dotted")) +
  scale_color_manual(values = c("Estado Real (x_k)" = "red", 
                                "Observación (z_k)" = "black")) +
  # Conectamos los puntos del estado real con líneas, 
  # pero las observaciones serán solo puntos
  geom_point(data = subset(df_long, Serie == "Observación (z_k)"), size = 1.5) +
  geom_line(data = subset(df_long, Serie == "Estado Real (x_k)"), size = 0.8) +
  
  # Separamos las dos trayectorias en gráficos distintos (facetas)
  facet_wrap(~ Escenario, ncol = 1, scales = "free_y") +
  labs(title = "Trayectorias del Estado vs. Observaciones",
       x = "Tiempo (k)",
       y = "Valor",
       color = "Tipo de Serie",
       linetype = "Tipo de Serie") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Imprimimos la gráfica
print(g)































library(ggplot2)
library(tidyr)

# --- 1. Definicion de parametros y simulacion ---
Phi <- -1; H <- 1; Q <- 0; R <- 0.5; P0 <- 1; x0_media <- 0
N_iter <- 50; N_steps <- N_iter + 1
N_horizon <- 2; time <- 0:N_iter
set.seed(123)

x_true <- numeric(N_steps); z_obs <- numeric(N_steps)
v <- rnorm(N_steps, 0, sqrt(R))
x_true[1] <- rnorm(1, x0_media, sqrt(P0))
z_obs[1] <- H * x_true[1] + v[1]
for (i in 2:N_steps) {
  x_true[i] <- Phi * x_true[i-1]
  z_obs[i] <- H * x_true[i] + v[i]
}

# --- 2. Algoritmo de Filtrado de Kalman ---
x_pred <- numeric(N_steps); P_pred <- numeric(N_steps)
x_fil <- numeric(N_steps); P_fil <- numeric(N_steps)
K_gain <- numeric(N_steps); innov <- numeric(N_steps)
Pi_cov <- numeric(N_steps)

x_pred[1] <- x0_media; P_pred[1] <- P0
Pi_cov[1] <- H * P_pred[1] * H + R
K_gain[1] <- P_pred[1] * H / Pi_cov[1]
innov[1] <- z_obs[1] - H * x_pred[1]
x_fil[1] <- x_pred[1] + K_gain[1] * innov[1]
P_fil[1] <- (1 - K_gain[1] * H) * P_pred[1]

for (i in 2:N_steps) {
  # Bucle del Filtro de Kalman
  x_pred[i] <- Phi * x_fil[i-1]
  P_pred[i] <- P_fil[i-1] + Q # P(k/k-1) = Phi*P*Phi' + Q = (-1)^2*P + 0 = P
  
  Pi_cov[i] <- H * P_pred[i] * H + R
  K_gain[i] <- P_pred[i] * H / Pi_cov[i]
  innov[i] <- z_obs[i] - H * x_pred[i]
  x_fil[i] <- x_pred[i] + K_gain[i] * innov[i]
  P_fil[i] <- (1 - K_gain[i] * H) * P_pred[i]
}

# --- 3. Algoritmo de Suavizamiento Punto Fijo (N=2) ---
x_smooth <- numeric(N_steps); P_smooth <- numeric(N_steps)
x_smooth[] <- NA; P_smooth[] <- NA

for (i in 1:N_steps) {
  # Bucle del Suavizador (para cada k, mira N pasos adelante)
  x_smooth_current <- x_fil[i]; P_smooth_current <- P_fil[i]; L_current <- P_fil[i]
  j_offset <- 0
  for (j in (i + 1):(i + N_horizon)) {
    j_offset <- j_offset + 1
    if (j > N_steps) { break }
    
    K_smooth_j <- L_current * Phi * H / Pi_cov[j]
    x_smooth_current <- x_smooth_current + K_smooth_j * innov[j]
    P_smooth_current <- P_smooth_current - K_smooth_j * Pi_cov[j] * K_smooth_j
    L_current <- L_current * Phi * (1 - K_gain[j] * H)
  }
  
  if (j_offset == N_horizon || (i + N_horizon > N_steps && i <= N_steps)) {
    x_smooth[i] <- x_smooth_current
    P_smooth[i] <- P_smooth_current
  }
}
x_smooth[is.na(x_smooth)] <- x_fil[is.na(x_smooth)]
P_smooth[is.na(P_smooth)] <- P_fil[is.na(P_smooth)]

# --- 4. Impresion de resultados ---
k_sample <- 20
i_sample <- k_sample + 1

print(paste("Resultados en k =", k_sample))
print(paste("  Estado Real x(k):", round(x_true[i_sample], 4)))
print(paste("  Observacion z(k):", round(z_obs[i_sample], 4)))
print(paste("  Filtro x_hat(k/k):", round(x_fil[i_sample], 4)))
print(paste("  Suavizado x_hat(k/k+N):", round(x_smooth[i_sample], 4)))
print("---")
print("Varianzas del Error (Teoricas):")
print(paste("  Filtro P(k/k):", round(P_fil[i_sample], 4)))
print(paste("  Suavizado P(k/k+N):", round(P_smooth[i_sample], 4)))










































# 0. Cargar Librerias
library(ggplot2)
library(tidyr)

# 1. Definicion de Parametros del Modelo (Ejercicio 2)
# x(k+1) = -1*x(k) + 0*w(k)
# z(k)   = 1*x(k) + v(k)
Phi <- -1
H   <- 1
Q   <- 0    # No hay ruido de proceso
R   <- 0.5  # Varianza de v(k)
P0  <- 1    # Varianza de x(0)
x0_media <- 0

# 2. Parametros de Simulacion y Suavizado
N_iter    <- 20 # 20 iteraciones (k=0 a 20)
N_steps   <- N_iter + 1
N_horizons <- c(1, 2, 4) # Horizontes N para suavizado
N_max     <- max(N_horizons)
time <- 0:N_iter
set.seed(123) # Para reproducibilidad

# 3. Simulacion del Sistema (x(k+1) = -x(k))
x_true <- numeric(N_steps)
z_obs  <- numeric(N_steps)
v <- rnorm(N_steps, 0, sqrt(R)) # Ruido de observacion

x_true[1] <- rnorm(1, x0_media, sqrt(P0))
z_obs[1]  <- H * x_true[1] + v[1]

for (i in 2:N_steps) {
  x_true[i] <- Phi * x_true[i-1]
  z_obs[i]  <- H * x_true[i] + v[i]
}

# 4. FASE 1: Algoritmo de Filtrado de Kalman
# Almacenamos todos los resultados necesarios
x_pred <- numeric(N_steps) # x_hat(k/k-1)
P_pred <- numeric(N_steps) # P(k/k-1)
x_fil  <- numeric(N_steps) # x_hat(k/k)
P_fil  <- numeric(N_steps) # P(k/k)
K_gain <- numeric(N_steps) # K(k)
innov  <- numeric(N_steps) # z_tilde(k/k-1)
Pi_cov <- numeric(N_steps) # Cov(innov)

# Inicializacion k=0
x_pred[1] <- x0_media
P_pred[1] <- P0
Pi_cov[1] <- H * P_pred[1] * H + R
K_gain[1] <- P_pred[1] * H / Pi_cov[1]
innov[1]  <- z_obs[1] - H * x_pred[1]
x_fil[1]  <- x_pred[1] + K_gain[1] * innov[1]
P_fil[1]  <- (1 - K_gain[1] * H) * P_pred[1]

# Bucle del Filtro k=1 hasta k=N_iter
for (i in 2:N_steps) {
  x_pred[i] <- Phi * x_fil[i-1]
  P_pred[i] <- P_fil[i-1] + Q
  
  Pi_cov[i] <- H * P_pred[i] * H + R
  K_gain[i] <- P_pred[i] * H / Pi_cov[i]
  innov[i]  <- z_obs[i] - H * x_pred[i]
  x_fil[i]  <- x_pred[i] + K_gain[i] * innov[i]
  P_fil[i]  <- (1 - K_gain[i] * H) * P_pred[i]
}

# 5. FASE 2: Algoritmo de Suavizamiento Punto Fijo
x_smooth <- matrix(NA, nrow = N_steps, ncol = length(N_horizons))
P_smooth <- matrix(NA, nrow = N_steps, ncol = length(N_horizons))
colnames(x_smooth) <- paste0("N=", N_horizons)
colnames(P_smooth) <- paste0("N=", N_horizons)

# Bucle EXTERNO: Itera sobre cada punto k que queremos suavizar
for (i in 1:N_steps) {
  
  # Inicializamos el suavizador en j=k
  x_smooth_current <- x_fil[i]
  P_smooth_current <- P_fil[i]
  L_current <- P_fil[i]
  
  # Bucle INTERNO: Itera hacia el futuro (j = k+1, ..., k+N_max)
  for (j_offset in 1:N_max) {
    j_idx <- i + j_offset # Indice de j (j=k+j_offset)
    
    if (j_idx > N_steps) { break }
    
    K_smooth_j <- L_current * Phi * H / Pi_cov[j_idx]
    x_smooth_current <- x_smooth_current + K_smooth_j * innov[j_idx]
    P_smooth_current <- P_smooth_current - K_smooth_j * Pi_cov[j_idx] * K_smooth_j
    L_current <- L_current * Phi * (1 - K_gain[j_idx] * H)
    
    if (j_offset %in% N_horizons) {
      col_idx <- which(N_horizons == j_offset)
      x_smooth[i, col_idx] <- x_smooth_current
      P_smooth[i, col_idx] <- P_smooth_current
    }
  }
}

# 6. Preparacion de Datos y Graficacion

# GRAFICA 1: Trayectorias 
data_g1 <- data.frame(
  Tiempo = time,
  Estado_Real = x_true,
  Observacion = z_obs,
  Filtrado = x_fil,
  Suavizado_N2 = x_smooth[, "N=2"]
)
data_g1$Suavizado_N2[is.na(data_g1$Suavizado_N2)] <- data_g1$Filtrado[is.na(data_g1$Suavizado_N2)]

data_g1_long <- tidyr::pivot_longer(
  data_g1,
  cols = -Tiempo,
  names_to = "Serie",
  values_to = "Valor"
)

g1 <- ggplot(data_g1_long, aes(x = Tiempo, y = Valor, color = Serie)) +
  geom_line(aes(linetype = Serie), size = 0.8) +
  geom_point(data = subset(data_g1_long, Serie == "Observacion"), size = 1.5, shape = 1) +
  scale_linetype_manual(values = c(
    "Estado_Real" = "solid", 
    "Observacion" = "blank",
    "Filtrado" = "dashed", 
    "Suavizado_N2" = "solid"
  )) +
  scale_color_manual(values = c(
    "Estado_Real" = "black", 
    "Observacion" = "grey50", 
    "Filtrado" = "blue", 
    "Suavizado_N2" = "red"
  )) +
  labs(
    title = "Comparativa de Estimaciones (N=2)",
    x = "Tiempo (k)",
    y = "Valor",
    color = "Series",
    linetype = "Series"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# GRAFICA 2: Varianzas
data_g2 <- data.frame(
  Tiempo = time,
  Filtrado = P_fil,
  Suavizado_N1 = P_smooth[, "N=1"],
  Suavizado_N2 = P_smooth[, "N=2"],
  Suavizado_N4 = P_smooth[, "N=4"]
)

# Rellenamos los NA al final con el valor del filtro
data_g2[is.na(data_g2)] <- data_g2$Filtrado[is.na(data_g2)]

data_g2_long <- tidyr::pivot_longer(
  data_g2,
  cols = -Tiempo,
  names_to = "Serie",
  values_to = "Varianza"
)

# Reordenamos factores para que la leyenda salga ordenada
data_g2_long$Serie <- factor(data_g2_long$Serie, levels = c("Filtrado", "Suavizado_N1", "Suavizado_N2", "Suavizado_N4"))

g2 <- ggplot(data_g2_long, aes(x = Tiempo, y = Varianza, color = Serie)) +
  geom_line(size = 0.8) +
  labs(
    title = "Comparativa de Varianzas del Error",
    x = "Tiempo (k)",
    y = "Varianza del Error (P)",
    color = "Estimador"
  ) +
  coord_cartesian(ylim = c(0, NA)) + 
  theme_minimal() +
  theme(legend.position = "bottom")

print(g1)
print(g2)




























# 0. Cargar Librerias
library(ggplot2)
library(tidyr)

# 1. Definicion de Parametros del Modelo
Phi <- -1
H <- 1
Q <- 0
N_iter <- 20
N_steps <- N_iter + 1
time <- 0:N_iter

# 2. Funcion para calcular la Varianza del Filtro 
calcular_varianza_filtro <- function(P0_val, R_val, Phi, H, Q, N_steps) {
  
  P_pred <- numeric(N_steps)
  P_fil  <- numeric(N_steps)
  
  # Inicializacion k=0
  P_pred[1] <- P0_val
  Pi_cov_k0 <- H * P_pred[1] * H + R_val
  K_gain_k0 <- P_pred[1] * H / Pi_cov_k0
  P_fil[1]  <- (1 - K_gain_k0 * H) * P_pred[1]
  
  # Bucle del Filtro k=1 hasta k=N_iter
  for (i in 2:N_steps) {
    P_pred[i] <- P_fil[i-1] + Q 
    Pi_cov_i <- H * P_pred[i] * H + R_val
    K_gain_i <- P_pred[i] * H / Pi_cov_i
    P_fil[i]  <- (1 - K_gain_i * H) * P_pred[i]
  }
  return(P_fil)
}

# --- 3. Definicion y calculo de Escenarios ---
P0_base <- 1.0; R_base <- 0.5
P0_alt <- 10.0; R_alt_P0 <- 0.5
P0_alt_R <- 1.0; R_alt <- 2.0
P0_baj_R <- 1.0; R_baj <- 0.1

P_fil_base <- calcular_varianza_filtro(P0_base, R_base, Phi, H, Q, N_steps)
P_fil_P0_alt <- calcular_varianza_filtro(P0_alt, R_alt_P0, Phi, H, Q, N_steps)
P_fil_R_alt <- calcular_varianza_filtro(P0_alt_R, R_alt, Phi, H, Q, N_steps)
P_fil_R_baj <- calcular_varianza_filtro(P0_baj_R, R_baj, Phi, H, Q, N_steps)

# 4. Preparacion de Datos para Grafica
data_g3 <- data.frame(
  Tiempo = time,
  Varianza_Base = P_fil_base,
  Varianza_P0_Alto = P_fil_P0_alt,
  Varianza_R_Alto = P_fil_R_alt,
  Varianza_R_Bajo = P_fil_R_baj
)

data_g3_long <- tidyr::pivot_longer(
  data_g3,
  cols = -Tiempo,
  names_to = "Escenario",
  values_to = "Varianza"
)

# Definir etiquetas y orden de la leyenda
data_g3_long$Escenario <- factor(data_g3_long$Escenario, 
                                 levels = c("Varianza_P0_Alto", "Varianza_R_Alto", "Varianza_Base", "Varianza_R_Bajo"),
                                 labels = c("(10.0, 0.5)", 
                                            "(1.0, 2.0)", 
                                            "(1.0, 0.5)", 
                                            "(1.0, 0.1)"))

# 5. Representación
g3 <- ggplot(data_g3_long, aes(x = Tiempo, y = Varianza, color = Escenario)) +
  geom_line(size = 0.8) +
  labs(
    x = "Tiempo (k)",
    y = "Varianza del Error (P)",
    color = "(P0, R)"
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g3)
