# CÓDIGO ALPHA>1/2
#Asignar valor inicial de theta
theta_min = 2
alpha = 0.9
bar_theta = 1000
# Condición inicial
theta_l = 100
k = 0.99
n=10000
his <- vector(mode = "numeric", length = m)

graf <- vector(mode = "numeric", length = n)
graf2 <- vector(mode = "numeric", length = n)
#RECURRENCIA
for(i in 1:n){
  graf[i]<- theta_l
  #print(theta_l)
  theta_I <- theta_l/runif(1, min=0, max=1)^(1/alpha)
  theta_C <- theta_min/runif(1, min=0, max=1)^(1/alpha)
  
  # REGLA
  if(theta_l<bar_theta){ #r=(1,0)
    EV = (alpha/(2*alpha-1))*(min((1-k)*theta_l,theta_min))^(alpha)*max((1-k)*theta_l,theta_min)^(1-alpha)
    if(theta_C>theta_I*(1-k)){
      theta_l <- max(theta_min,theta_I*(1-k))
    }else{
      theta_l <-max(theta_l,theta_C/(1-k))
    }
  }else{ # r = (1,1)
    EV = (alpha/(2*alpha-1))*(1-k)*(theta_min^alpha)*(theta_l)^(1-alpha)
    theta_l <- min(theta_I, max(theta_C, theta_l))
  }
  #print(theta_l)
  graf2[i] <- EV
}
# El último elemento no se guarda en el vector
graf[n] <- theta_l
# Gráfico de dispersión 
x <- 1:n

#par(mar=c(5, 5, 1, 1) )

plot(x, y=log(graf), 
     type = "l",
     #main = "Gráfico de Dispersión", 
     xlab = "t", 
     ylab = expression(theta[l]^(t+1)), 
     col = "blue") 
abline(h=log(bar_theta), col="red", lty=2)
