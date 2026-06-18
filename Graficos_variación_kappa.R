# GRAFICOS PARA DISTINTOS VALORES DE KAPPA

# Cantidad de gráficos y trayectorias
m = 6
# Iteraciones
n=10000

# Lista con m vectores numéricos de tamaño n
graf_lista <- replicate(m, vector(mode = "numeric", length = n), simplify = FALSE)
# Lista de kappas variables 
k_lista <- c(function(x){return(0.6)}, function(x){return(1-x/n)}, function(x){return(x/n)},
             function(x){return(1-1/x^2)}, function(x){return(n/x^2)}, 
             function(x){return(0.999)})
k_titulos <- c("k = 0.6", "k = 1-x/n", "k=x/n",
             "k=1-1/x^2", "k=n/x^2", 
             "k=0.999")
# En cada recorrido se crea un vector para graficar la trayectoria j
for(j in 1:m){
   # Parámetros fijos 
  alpha = 0.75
  theta_min = 2
  bar_theta = 1000
  # Condición inicial
  theta_l = 100
  
  #REGLA
  for(i in 1:n){
    k <- k_lista[[j]](i)
    #print(k)
    graf_lista[[j]][i]<- theta_l
    #print(theta_l)
    theta_I <- theta_l/runif(1, min=0, max=1)^(1/alpha)
    theta_C <- theta_min/runif(1, min=0, max=1)^(1/alpha)
    
    # REGLA
    if(theta_l<bar_theta){ #r=(1,0)
      if(theta_C>theta_I*(1-k)){
        theta_l <- max(theta_min,theta_I*(1-k))
      }else{
        theta_l <-max(theta_l,theta_C/(1-k))
      }
    }else{ # r = (1,1)
      
      theta_l <- min(theta_I, max(theta_C, theta_l))
    }
  }
}


# Plot de las trayectorias

par(mfrow = c(3, 2),mar=c(4, 5, 2, 3) )
for(i in 1:m){
plot(log(graf_lista[[i]]), 
     type = "l",
     xlab = "t", 
     ylab = expression(theta[l]^(t+1)),
     main = k_titulos[i])
abline(h=log(bar_theta), col="red", lty=2)
abline(v= 50, col="green", lty=2)
}

