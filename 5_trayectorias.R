# ==============================================================================
# PARAMETROS INICIALES
# ==============================================================================
theta_min <- 2
alpha     <- 0.55
bar_theta <- 1000
k         <- 0.0000000000000000001
n         <- 10000
n_curvas  <- 5  # Número de simulaciones independientes

# Definimos colores bonitos y distintos para las 5 curvas
colores <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

# Matrices para almacenar los resultados de las 5 simulaciones
# Cada columna representará una curva distinta
matriz_theta <- matrix(NA, nrow = n, ncol = n_curvas)
matriz_EV    <- matrix(NA, nrow = n, ncol = n_curvas)

# ==============================================================================
# SIMULACIÓN (Bucle para 5 trayectorias distintas)
# ==============================================================================
for (j in 1:n_curvas) {
  
  # Condición inicial (se reinicia para cada curva)
  theta_l <- 100 
  
  # RECURRENCIA INTERNA
  for (i in 1:n) {
    matriz_theta[i, j] <- theta_l
    
    theta_I <- theta_l / runif(1, min=0, max=1)^(1/alpha)
    theta_C <- theta_min / runif(1, min=0, max=1)^(1/alpha)
    
    # REGLA
    if (theta_l < bar_theta) { # r=(1,0)
      EV <- (alpha/(2*alpha-1)) * (min((1-k)*theta_l, theta_min))^(alpha) * max((1-k)*theta_l, theta_min)^(1-alpha)
      if (theta_C > theta_I*(1-k)) {
        theta_l <- max(theta_min, theta_I*(1-k))
      } else {
        theta_l <- max(theta_l, theta_C/(1-k))
      }
    } else { # r = (1,1)
      EV <- (alpha/(2*alpha-1)) * (1-k) * (theta_min^alpha) * (theta_l)^(1-alpha)
      theta_l <- min(theta_I, max(theta_C, theta_l))
    }
    
    matriz_EV[i, j] <- EV
  }
  # El último elemento
  matriz_theta[n, j] <- theta_l
}

# ==============================================================================
# GRÁFICOS
# ==============================================================================
x <- 1:n

# Configuramos la pantalla para 2 filas, 1 columna, y ajustamos márgenes izquierdos
par(mfrow = c(2, 1), mar = c(4.5, 5.5, 2, 2), mgp = c(3, 1, 0))

# ------------------------------------------------------------------------------
# GRÁFICO 1: Trayectorias log(theta)
# ------------------------------------------------------------------------------
# Calculamos los límites globales en Y para log(theta) incluyendo la barra de threshold
rango_y_theta <- range(log(matriz_theta), log(bar_theta), na.rm = TRUE)

for (j in 1:n_curvas) {
  if (j == 1) {
    # Inicializa el lienzo con la primera curva
    plot(x, log(matriz_theta[, j]), 
         type = "l", 
         col = colores[j], 
         ylim = rango_y_theta,
         xlab = "t", 
         ylab = expression(log(theta[t]^(l+1))), 
         main = "Trayectorias de Theta")
  } else {
    # Añade las curvas restantes
    lines(x, log(matriz_theta[, j]), col = colores[j])
  }
}
# Línea de Threshold (se mantiene visible arriba de todas las curvas)
abline(h = log(bar_theta), col = "black", lty = 2, lwd = 2) 

# ------------------------------------------------------------------------------
# GRÁFICO 2: Valores del EV
# ------------------------------------------------------------------------------
# Calculamos los límites globales en Y para EV
rango_y_ev <- range(matriz_EV, na.rm = TRUE)

for (j in 1:n_curvas) {
  if (j == 1) {
    # Inicializa el lienzo con la primera curva
    plot(x, matriz_EV[, j], 
         type = "l", 
         col = colores[j], 
         ylim = rango_y_ev,
         xlab = "t", 
         ylab = "EV", 
         main = "Trayectorias de EV")
  } else {
    # Añade las curvas restantes
    lines(x, matriz_EV[, j], col = colores[j])
  }
}

# Opcional: Agregar una leyenda pequeña al final
legend("topright", legend = paste("Sim", 1:5), col = colores, lty = 1, bty = "n", cex = 0.8)