
knitr::opts_chunk$set(echo = TRUE)
library("tinytex", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library(ContourFunctions)
library("rmarkdown", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
set.seed(3)	# se establece la semilla


simula_unif = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
  nrow = N, ncol=dims, byrow=T)
  m
}

simula_gaus = function(N=2,dim=2,sigma){
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")
  # genera 1 muestra, con las desviaciones especificadas
  simula_gauss1 = function() rnorm(dim, sd = sigma) 
  
  # repite N veces, simula_gauss1 y se hace la traspuesta
  m = t(replicate(N,simula_gauss1())) 
  
  m
}

simula_recta = function (intervalo = c(-1,1), visible=F){
  ptos = simula_unif(2,2,intervalo) 
  a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1]) 
  b = ptos[1,2]-a*ptos[1,1] 
  
  if (visible) {
    if (dev.cur()==1)
      plot(1, type="n", xlim=intervalo, ylim=intervalo)
    points(ptos,col=3)  #pinta en verde los puntos
    abline(b,a,col=3)   # y la recta
  }
  
  c(a,b) # devuelve el par pendiente y punto de corte
}


datos1 <- simula_unif(50,2,c(-50,50))
plot(datos1, xlab = " ", ylab =" ", pch=20, main = "Nube Uniforme")


datos2 <- simula_gaus(50,2,c(5,7))
plot(datos2, xlab = " ", ylab =" ", pch=20, xlim = c(-20,20), 
     ylim = c(-20,20), main = "Nube Gaus")


muestra2D = simula_unif(50,2,c(-50,50))
plot(datos1, xlab = " ", ylab =" ", pch=20, main = "Nube Uniforme muestra2D")

ab <- simula_recta(c(-50,50))

signo <- function(xy){
  sign(xy[2]-ab[1]*xy[1]-ab[2])
}


generarEtiquetas <- function(datos = muestra2D, recta = ab ){
  etiquetas <- apply(X = datos, FUN = signo, MARGIN = 1)
  plot(datos, col=etiquetas+3, pch=20, xlab = " ", ylab =" ", main="Etiquetas" )
  abline(ab[2],ab[1])
  etiquetas
}


etiquetas1 <- generarEtiquetas(muestra2D,ab)

ruido <- function(etiquetas=etiquetas1, porcentaje=10){
  et = length(etiquetas)
  et1 = sum(etiquetas == 1)
  et2 = sum(etiquetas != 1)
  
  ind1 = which(etiquetas == 1)
  ind2 = which(etiquetas == -1)
  
  etiquetas[sample(ind1,et1*porcentaje/100)] = -1
  etiquetas[sample(ind2,et2*porcentaje/100)] = 1
  etiquetas
}

# Aplicamos ruido a las etiquetas
etiquetasRuido1 <- ruido(etiquetas1,10)
# Pintamos la muestra con el ruido
plot(muestra2D, col=etiquetasRuido1+3, pch=20, xlab = " ", ylab =" ", 
     main="Etiquetas Ruido muestra2D" )
abline(ab[2],ab[1])


f1 <- function(x,y){
  resultado <- ((x-10)^2 + (y-20)^2 - 400)
  resultado
}

f2 <- function(x,y){
  resultado <- ((0.5*(x+10)^2) + (y-20)^2 - 400)
  resultado
}

f3 <- function(x,y){
  resultado <- ((0.5*(x-10)^2) - (y+20)^2 - 400)
  resultado
}

f4 <- function(x,y){
  resultado <- (y - 20*x^2 - (5*x) + 3)
  resultado
}

pintar_frontera = function(f,rango=c(-50,50)) {
  x=y=seq(rango[1],rango[2],length.out = (rango[2]-rango[1]))
  z = outer(x,y,FUN=f)
  if(dev.cur() == 1)
    plot(1,pch = 20, type="n",xlim=rango,ylim=rango)
  contour(x,y,z,drawlabels = FALSE , xlim =rango, ylim=rango, xlab = "", ylab = "",
lwd = 0.5)
}


pintar_frontera(f1)
puntos_fuera <- subset(muestra2D,f1(muestra2D[,1],muestra2D[,2])>0)
puntos_dentro <- subset(muestra2D,f1(muestra2D[,1],muestra2D[,2])<0) 

points(puntos_fuera, col = "blue",pch = 20)
points(puntos_dentro, col = "red",pch = 20)

pintar_frontera(f2)
puntos_fuera <- subset(muestra2D,f2(muestra2D[,1],muestra2D[,2])>0)
puntos_dentro <- subset(muestra2D,f2(muestra2D[,1],muestra2D[,2])<0) 

points(puntos_fuera, col = "blue",pch = 20)
points(puntos_dentro, col = "red",pch = 20)


pintar_frontera(f3)
puntos_fuera <- subset(muestra2D,f3(muestra2D[,1],muestra2D[,2])>0)
puntos_dentro <- subset(muestra2D,f3(muestra2D[,1],muestra2D[,2])<0) 

points(puntos_fuera, col = "blue",pch = 20)
points(puntos_dentro, col = "red",pch = 20)


pintar_frontera(f4)
puntos_fuera <- subset(muestra2D,f4(muestra2D[,1],muestra2D[,2])>0)
puntos_dentro <- subset(muestra2D,f4(muestra2D[,1],muestra2D[,2])<0) 

points(puntos_fuera, col = "blue",pch = 20)
points(puntos_dentro, col = "red",pch = 20)


# Algoritmo perceptron
ajusta_PLA <- function(datos, label, max_iter=2000, vini) {
  # Inicializamos el vector de pesos y el contador de iteracciones
  w = vini
  iters = 1
  seguir = TRUE
  # Añadimos una columna para el calculo
  datos = cbind(rep(1,nrow(datos)),datos)
  # Si no llegamos a las iteracciones y debemos seguir...
  while(iters < max_iter & seguir) {
    seguir = FALSE
    # Iteramos sobre cada dato y calculamos su signo ...
    for(i in sample(nrow(datos))) {
      signo = sign(datos[i,] %*% w)
      # Si el signo es distinto a como lo clasificamos, 
      # cambiamos el vector de pesos para ajustarnos más
      if(signo != label[i]) {
        w = w + datos[i,]*label[i]
        
      }else{
        seguir = TRUE
      }
      iters = iters + 1
    }
  }
  # Devolvemos el hiperplano, en este caso, 
  # una recta y el num de iters para siguientes apartados
  c(-w[1]/w[3], -w[2]/w[3], iters)
}


# Con 10 iters
# --------------------------------------------------------------------
perceptron_a <- ajusta_PLA(muestra2D, etiquetas1, 10, c(0,0,0))
plot(muestra2D, main = "PLA para w = 0 y 10 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_a[1],perceptron_a[2])
cat("\nCon ", 10, " -> Num iters necesarias: ", perceptron_a[3])
#---------------------------------------------------------------------

# Con 100 iters
# --------------------------------------------------------------------
perceptron_a <- ajusta_PLA(muestra2D, etiquetas1, 100, c(0,0,0))
plot(muestra2D, main = "PLA para w = 0 y 100 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_a[1],perceptron_a[2])
cat("\nCon ", 100, " -> Num iters necesarias: ", perceptron_a[3])

#---------------------------------------------------------------------

# Con 1000 iters
# --------------------------------------------------------------------
perceptron_a <- ajusta_PLA(muestra2D, etiquetas1, 1000, c(0,0,0))
plot(muestra2D, main = "PLA para w = 0 y 1000 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_a[1],perceptron_a[2])
cat("\nCon ", 1000, "-> Num iters necesarias: ", perceptron_a[3])
#---------------------------------------------------------------------

# Con 10000 iters
# --------------------------------------------------------------------
perceptron_a <- ajusta_PLA(muestra2D, etiquetas1, 10000, c(0,0,0))
plot(muestra2D, main = "PLA para w = 0 y 10000 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_a[1],perceptron_a[2])
cat("\nCon ", 10000, " -> Num iters necesarias: ", perceptron_a[3])
#---------------------------------------------------------------------

# Con 20000 iters
# --------------------------------------------------------------------
perceptron_a <- ajusta_PLA(muestra2D, etiquetas1, 20000, c(0,0,0))
plot(muestra2D, main = "PLA para w = 0  y 20000 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_a[1],perceptron_a[2])
cat("\nCon ", 20000, " -> Num iters necesarias: ", perceptron_a[3])
#---------------------------------------------------------------------

#  b) El vector w inicializado a valores aleatorios

# Con 10 iters
# --------------------------------------------------------------------
perceptron_b <- ajusta_PLA(muestra2D, etiquetas1, 10, runif(3,0,1))
plot(muestra2D, main = "PLA para w Random Para 10 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_b[1],perceptron_b[2])
cat("\nCon ", 10, " -> Num iters necesarias: ", perceptron_b[3])
# --------------------------------------------------------------------


# Con 100 iters
# --------------------------------------------------------------------
perceptron_b <- ajusta_PLA(muestra2D, etiquetas1, 100, runif(3,0,1))
plot(muestra2D, main = "PLA para w Random Para 100 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_b[1],perceptron_b[2])
cat("\nCon ", 100, " -> Num iters necesarias: ", perceptron_b[3])
# --------------------------------------------------------------------


# Con 1000 iters
# --------------------------------------------------------------------
perceptron_b <- ajusta_PLA(muestra2D, etiquetas1, 1000, runif(3,0,1))
plot(muestra2D, main = "PLA para w Random Para 1000 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_b[1],perceptron_b[2])
cat("\nCon ", 1000, " -> Num iters necesarias: ", perceptron_b[3])
# --------------------------------------------------------------------

# Con 10000 iters
# --------------------------------------------------------------------
perceptron_b <- ajusta_PLA(muestra2D, etiquetas1, 10000, runif(3,0,1))
plot(muestra2D, main = "PLA para w Random Para 10000 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_b[1],perceptron_b[2])
cat("\nCon ", 10000, " -> Num iters necesarias: ", perceptron_b[3])
# --------------------------------------------------------------------

# Con 20000 iters
# --------------------------------------------------------------------
perceptron_b <- ajusta_PLA(muestra2D, etiquetas1, 20000, runif(3,0,1))
plot(muestra2D, main = "PLA para w Random Para 20000 iters", col = etiquetas1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron_b[1],perceptron_b[2])
cat("\nCon ", 20000, " -> Num iters necesarias: ", perceptron_b[3])
# --------------------------------------------------------------------


#  a) El vector w inicializado a 0
  

perceptron2_a <- ajusta_PLA(muestra2D, etiquetasRuido1, 10, c(0,0,0))
plot(muestra2D, main = "PLA ruido para w = 0 y 10 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_a[1],perceptron2_a[2])
cat("\nCon: ", 10, " ->Num iters necesarias: ", perceptron2_a[3])

perceptron2_a <- ajusta_PLA(muestra2D, etiquetasRuido1, 100, c(0,0,0))
plot(muestra2D, main = "PLA ruido para w = 0 y 100 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_a[1],perceptron2_a[2])
cat("\nCon: ", 100, " ->Num iters necesarias: ", perceptron2_a[3])

perceptron2_a <- ajusta_PLA(muestra2D, etiquetasRuido1, 1000, c(0,0,0))
plot(muestra2D, main = "PLA ruido para w = 0 y 1000 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_a[1],perceptron2_a[2])
cat("\nCon: ", 1000, " ->Num iters necesarias: ", perceptron2_a[3])

perceptron2_a <- ajusta_PLA(muestra2D, etiquetasRuido1, 10000, c(0,0,0))
plot(muestra2D, main = "PLA ruido para w = 0 y 10000 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_a[1],perceptron2_a[2])
cat("\nCon: ", 10000, " ->Num iters necesarias: ", perceptron2_a[3])

perceptron2_a <- ajusta_PLA(muestra2D, etiquetasRuido1, 20000, c(0,0,0))
plot(muestra2D, main = "PLA ruido para w = 0 y 20000 iters", col = etiquetasRuido1+3, xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_a[1],perceptron2_a[2])
cat("\nCon: ", 20000, " ->Num iters necesarias: ", perceptron2_a[3])

perceptron2_a <- ajusta_PLA(muestra2D, etiquetasRuido1, 200000, c(0,0,0))
plot(muestra2D, main = "PLA ruido para w = 0 y 200000 iters", col = etiquetasRuido1+3, xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_a[1],perceptron2_a[2])
cat("\nCon: ", 200000, " ->Num iters necesarias: ", perceptron2_a[3])



  
#  b) El vector w inicializado a valores aleatorios


perceptron2_b <- ajusta_PLA(muestra2D, etiquetasRuido1, 10, runif(3,0,1))
plot(muestra2D, main = "PLA ruido para w Random y 10 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_b[1],perceptron2_b[2])
cat("\nCon: ", 10, " ->Num iters necesarias: ", perceptron2_b[3])

perceptron2_b <- ajusta_PLA(muestra2D, etiquetasRuido1, 100, runif(3,0,1))
plot(muestra2D, main = "PLA ruido para w Random y 100 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_b[1],perceptron2_b[2])
cat("\nCon: ", 100, " ->Num iters necesarias: ", perceptron2_b[3])

perceptron2_b <- ajusta_PLA(muestra2D, etiquetasRuido1, 1000, runif(3,0,1))
plot(muestra2D, main = "PLA ruido para w Random y 1000 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_b[1],perceptron2_b[2])
cat("\nCon: ", 1000, " ->Num iters necesarias: ", perceptron2_b[3])

perceptron2_b <- ajusta_PLA(muestra2D, etiquetasRuido1, 10000, runif(3,0,1))
plot(muestra2D, main = "PLA ruido para w Random y 10000 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_b[1],perceptron2_b[2])
cat("\nCon: ", 10000, " ->Num iters necesarias: ", perceptron2_b[3])

perceptron2_b <- ajusta_PLA(muestra2D, etiquetasRuido1, 20000, runif(3,0,1))
plot(muestra2D, main = "PLA ruido para w Random y 20000 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_b[1],perceptron2_b[2])
cat("\nCon: ", 20000, " ->Num iters necesarias: ", perceptron2_b[3])

perceptron2_b <- ajusta_PLA(muestra2D, etiquetasRuido1, 200000, runif(3,0,1))
plot(muestra2D, main = "PLA ruido para w Random y 200000 iters", col = etiquetasRuido1+3, 
     xlim = c(-50,50), ylim = c(-50,50), xlab = "", ylab = "",pch=20)
abline(perceptron2_b[1],perceptron2_b[2])
cat("\nCon: ", 200000, " ->Num iters necesarias: ", perceptron2_b[3])


# Función norma que usaremos para la RL
norma <- function(w_old, w_new){
  sqrt(sum((w_old - w_new)^2))
}

# Para clasificar dos puntos 
dos_puntos = simula_unif(2,2,rango = c(0,2))
recta_2puntos = simula_recta(intervalo = c(0,2))
etiquetas_2puntos = sign(dos_puntos[,2]-recta_2puntos[1]*
                           dos_puntos[,1] -recta_2puntos[2])

# Para dos puntos
n_puntos = simula_unif(100,2,c(0,2))
recta_Npuntos = simula_recta(intervalo = c(0,2))
etiquetas_Npuntos = sign(n_puntos[,2]-recta_Npuntos[1]*
                           n_puntos[,1] -recta_Npuntos[2])


# Implementación de RL 
RL <- function(datos,etiquetas,vini = c(0,0,0), mu = 0.01, max_iter = 500){
  # Inicializamos los pesos.
  w_nuevo <- vini
  w_viejo <- c(0,0,0)
  # Añadimos una columna para el calculo  y el cnt de iters
  datos <- cbind(1,datos)
  iters <- 1
  seguir = TRUE
  # Mientras no se llegue al limite de iters 
  while (iters <= max_iter | seguir) {
    # Actualizamos el peso y permutamos las etiquetas
    w_viejo <- w_nuevo
    permutacion <- sample(1:length(etiquetas))
    # Por cada itetiqueta calculamos el valor que tendrá el SGD 
    # y actualizamos el peso con el nuevo valor
    for (i in permutacion) {
      SGD <- (-etiquetas[i]*datos[i,]) / 
        (1 + exp(etiquetas[i]*w_nuevo %*% datos[i,]))
      w_nuevo <- w_nuevo - mu * SGD
    }
    iters = iters +1
    
    # Si se mejora maramos
    if(norma(w_viejo,w_nuevo) < mu){
       seguir <- FALSE     
    }
  }
  # Devolvemos el hiperplano
  c(-w_nuevo[1]/w_nuevo[3], -w_nuevo[2] / w_nuevo[3]) 
}


recta_RL <- RL(dos_puntos,etiquetas_2puntos,c(0,0,0), 0.01, 500)
plot(dos_puntos, main = "Regresión 2 Puntos", pch=20, col = etiquetas_2puntos+3, 
     xlab = "", ylab = "",xlim = c(0,2), ylim = c(0,2))
abline(recta_RL[1],recta_RL[2])


recta_RL2 <- RL(n_puntos,etiquetas_Npuntos,c(0,0,0), 0.01, 500)
plot(n_puntos, main = "Regresión 100 Puntos", pch=20, col = etiquetas_Npuntos+3, 
     xlab = "", ylab = "",xlim = c(0,2), ylim = c(0,2))
abline(recta_RL2[1],recta_RL2[2])


datos3 <- simula_unif(1000,2,c(0,2))
recta3 <- simula_recta(c(0,2))
etiquetas3 <- sign(datos3[,2]-recta3[1]*datos3[,1]-recta3[1] -recta3[2])

regresion <- RL(datos3,etiquetas3)
etiquetas_regresion <- sign(datos3[,2]-regresion[1]*datos3[,1]
                            -regresion[2])

E_in <- 0
E_in <- sum(etiquetas3 != etiquetas_regresion) / length(etiquetas3)
E_in


datos3.test <- simula_unif(1000,2,c(0,2))
recta3.test <- simula_recta(c(0,2))
etiquetas3.test <- sign(datos3.test[,2]-recta3.test[1]*
                     datos3.test[,1]-recta3.test[1] -recta3[2])

etiquetas_regresion.test <- sign(datos3.test[,2]-regresion[1]
                            *datos3.test[,1]-regresion[2])

E_out <- 0
E_out <- sum(etiquetas3.test != etiquetas_regresion.test) / length(etiquetas3)
E_out




