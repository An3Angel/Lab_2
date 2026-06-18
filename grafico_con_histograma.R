#Parámetros
theta_min <- 2
alpha     <- 0.55
bar_theta <- 1000
K        <- function(i){return(0.5)}
# TIEMPO DE ITERACIONES
n         <- 1000
# TRAYECTORIAS
m         <- 1000
par(mfrow = c(1, 1))
k_lista <- c(function(x){return(0.6)}, function(x){return(1-x/n)}, function(x){return(x/n)},
             function(x){return(1-1/x^2)}, function(x){return(n/x^2)}, 
             function(x){return(0.999)})
k_titulos <- c("k = 0.6", "k = 1-x/n", "k=x/n",
               "k=1-1/x^2", "k=n/x^2", 
               "k=0.999")
# Vector para almacenar los tiempos en que se supera el threshold
his <- vector(mode = "numeric", length = m)
for(l in 1:6){
for (j in 1:m) {
  
  # theta_l inicial siempre es 100
  theta_l <- 100
  
  #graf_temporal <- rep(NA, n)
  
  for (i in 1:n) {
    #graf_temporal[i] <- theta_l
    k=(i)
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
    his[j] <- 0
  }
  
  
}
}
par(mfrow = c(3, 2),mar=c(4, 5, 2, 3) )
esp <- hist(his, 
      plot=TRUE,
     breaks = 100, 
     col = "darkgray", 
     border = "white",
     xlab = "Tiempo de cruce (t)", 
     ylab = "Frecuencia",
     main = expression(paste(kappa)))

desviacion_estandar <- sd(his)
promedio <- mean(his)
posicion_maxima <- which.max(esp$counts)
limite_inferior <- esp$breaks[posicion_maxima]
limite_superior <- esp$breaks[posicion_maxima + 1]
frecuencia <- esp$counts[posicion_maxima]