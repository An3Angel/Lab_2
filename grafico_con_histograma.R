#Parámetros
theta_min <- 2
alpha     <- 0.55
bar_theta <- 1000
k         <- 0.5
# TIEMPO DE ITERACIONES
n         <- 100
# TRAYECTORIAS
m         <- 100000
par(mfrow = c(1, 1))

# Vector para almacenar los tiempos en que se supera el threshold
his <- vector(mode = "numeric", length = m)

for (j in 1:m){
  
  # theta_l inicial siempre es 100
  theta_l <- 100
  
  #graf_temporal <- rep(NA, n)
  
  for (i in 1:n) {
    #graf_temporal[i] <- theta_l
    theta_I <- theta_l / runif(1, min = 0, max = 1)^(1 / alpha)
    theta_C <- theta_min / runif(1, min = 0, max = 1)^(1 / alpha)
    
    # REGLA
    if (theta_l < bar_theta) { # r=(1,0)
      if (theta_C > theta_I * (1 - k)) {
        theta_l <- max(theta_min, theta_I * (1 - k))
      } else {
        theta_l <- max(theta_l, theta_C / (1 - k))
      }
    } else { # r = (1,1) -> Supera el threshold
      his[j] <- i  # Guardamos el tiempo de quiebre
      theta_l <- min(theta_I, max(theta_C, theta_l))
      
      #graf_temporal[i] <- theta_l 
      break
    }
  }
  
  # Si el ciclo terminó naturalmente y nunca superó el threshold, le asignamos el tiempo máximo n
  if (his[j] == 0) {
    his[j] <- -1
  }
  
  
}


esp <- hist(his, 
      plot=TRUE,
     breaks = 100, 
     col = "darkgray", 
     border = "white",
     xlab = "Tiempo de cruce (t)", 
     ylab = "Frecuencia",
     main = bquote(kappa== .(k)))

desviacion_estandar <- sd(his)
promedio <- mean(his)
posicion_maxima <- which.max(esp$counts)
limite_inferior <- esp$breaks[posicion_maxima]
limite_superior <- esp$breaks[posicion_maxima + 1]
frecuencia <- esp$counts[posicion_maxima]