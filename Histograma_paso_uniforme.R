# Histogramas con k paso uniforme

#Parámetros
theta_min <- 2
alpha     <- 0.55
bar_theta <- 1000
k_max         <- 1
# TIEMPO DE ITERACIONES
n         <- 100
# TRAYECTORIAS
m         <- 100
# Tamaño del paso
t         <- 10
# Lista con m vectores numéricos de tamaño n
his_lista <- replicate(t-1, vector(mode = "numeric", length = m), simplify = FALSE)

# Definimos un kappa para cada paso 
for(s in 1:(t-1)){
  
  
  # kappa del paso
  k <- s/t
  
  #TRAYECTORIA m
  for (j in 1:m) {
    # Condición inicial
    theta_l = 100
    # REGLAS DE LA TRAYECCTORIA j
    for (i in 1:n) {
      
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
        his_lista[[s]][[j]] <- i  # Guardamos el tiempo de quiebre del paso s en la trayectoria j
        theta_l <- min(theta_I, max(theta_C, theta_l))
        
        #graf_temporal[i] <- theta_l 
        break
      }
    }
    
    # Si el ciclo terminó naturalmente y nunca superó el threshold, le asignamos el tiempo máximo n
    if (his_lista[[s]][[j]]== 0) {
      his_lista[[s]][[j]] <- -1
    }
    
    
  }
}


# Plot de las trayectorias

par(mfrow = c(1, 1),mar=c(4, 5, 2, 3) )
for(i in 1:(t-1)){
  hist(his_lista[[i]], 
       plot=TRUE,
       breaks = 100, 
       col = "darkgray", 
       border = "white",
       xlab = "Tiempo de cruce (t)", 
       ylab = "Frecuencia",
       main = bquote(kappa==.(i/t)))
}
