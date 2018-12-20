#---
# title: Trabajo 1 | Programación
# author: Antonio Miguel Morillo Chica
# date: 22/03/2018
# output: pdf_document
---

knitr::opts_chunk$set(echo = TRUE)
library("tinytex", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("rmarkdown", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

E <- function(u,v){
  (u^(3) * exp(v-2) - 4*v^(3)*exp(-u))^2
}

dE <- function(u,v){
  duE <- 2 *  ((u^3)*exp(v-2) - 4* exp(-u)*v^3) * (3*u^(2)*exp(v-2) + 4*exp(-u)*v^3 ) 
  dvE <- 2 *  ((u^3)*exp(v-2) - 12*exp(-u)*v^2) * (  u^(3)*exp(v-2) - 4*exp(-u)*v^3 ) 
  c(duE, dvE)
}

GD <- function(f, f_prima, t_aprendizaje = 0.1, umbral = 10^(-14), nIter=5000,
  pInicial=c(1,1)){
  
  w_anterior <- c(0,0)
  w_nuevo <- pInicial
  
  iters <- 0
  datos <- rep(nIter)
  datos[iters] <- f(w_nuevo[1], w_nuevo[2])
  
  cat("\nInicio: ", w_nuevo[1], w_nuevo[2])
  while( (iters < nIter) &  ( f(w_nuevo[1],w_nuevo[2]) > umbral) & 
         (abs(f(w_nuevo[1], w_nuevo[2]) - f(w_anterior[1], w_anterior[2])) > umbral) ) {
    
    w_anterior = w_nuevo
    
    gradiante <- f_prima(w_nuevo[1], w_nuevo[2])
    n_gradiante <- gradiante * (-1)
    w_nuevo <- w_anterior + t_aprendizaje * n_gradiante
    iters <- iters + 1
    datos[iters] <- f(w_nuevo[1], w_nuevo[2]) 
  }
  cat("\nSolución: ", w_nuevo[1], w_nuevo[2])
  cat("\nIteracciones:", iters)
  
  list(valor_funcion = datos[1:iters])
}

GD(E, dE, t_aprendizaje=0.05, umbral=10^(-14), nIter=500, pInicial=c(1,1))

GD(E, dE, t_aprendizaje=0.05, umbral=10^(-15), nIter=500, pInicial=c(1,1))

F <- function(x,y){
  (x-2)^(2)+2*(y+2)^(2)+2*sin(2*pi*x)*sin(2*pi*y)
}

dF <- function(x,y){
  dxF <- 2*(2*pi*cos(2*pi*x)*sin(2*pi*y)+x-2)
  dyF <- 4*(pi*sin(2*pi*x)*cos(2*pi*y)+y+2)
  c(dxF,dyF)
}

# mu = 0.01
datos1 = GD(F,dF, t_aprendizaje=0.01, umbral=10^(-14), nIter=50, pInicial=c(1,1))
plot(datos1$valor_funcion, type = "o", pch = 20, col = 50, xlab = "Iteracion", 
     ylab = "E(u,v)", main = "Gradiente Descendiente E(u,v)")

Sys.sleep(3)

# mu = 0.1
datos2 = GD(F,dF, t_aprendizaje=0.1,  umbral=10^(-14), nIter=50, pInicial=c(1,1))
plot(datos2$valor_funcion, type = "o", pch = 10, col = 5, xlab = "Iteracion", 
     ylab ="E(u,v)", main = "Gradiente Descendiente E(u,v)")

Sys.sleep(3)

datos3 = GD(F,dF, t_aprendizaje=0.1, umbral=10^(-14), nIter=50, pInicial=c(2.1,-2.1))
plot(datos3$valor_funcion, type = "o", pch = 10, col = 5, xlab = "Iteracion", 
     ylab ="E(u,v)", main = "Gradiente Descendiente E(u,v)")
Sys.sleep(3)

datos4 = GD(F,dF, t_aprendizaje=0.1, umbral=10^(-14), nIter=50, pInicial=c(3,-3))
plot(datos4$valor_funcion, type = "o", pch = 10, col = 5, xlab = "Iteracion", 
     ylab ="E(u,v)", main = "Gradiente Descendiente E(u,v)")
Sys.sleep(3)

datos5 = GD(F,dF, t_aprendizaje=0.1, umbral=10^(-14), nIter=50, pInicial=c(1.5,1.5))
plot(datos5$valor_funcion, type = "o", pch = 10, col = 5, xlab = "Iteracion", 
     ylab ="E(u,v)", main = "Gradiente Descendiente E(u,v)")
Sys.sleep(3)

datos6 = GD(F,dF, t_aprendizaje=0.1, umbral=10^(-14), nIter=50, pInicial=c(1,-1))
plot(datos6$valor_funcion, type = "o", pch = 10, col = 5, xlab = "Iteracion", 
     ylab ="E(u,v)", main = "Gradiente Descendiente E(u,v)")
Sys.sleep(3)

todos <- c(datos3$valor_funcion, datos4$valor_funcion, datos5$valor_funcion, datos5$valor_funcion)
plot(todos, type = "o", pch = 100, col = 100, xlab = "Iteracion", 
     ylab ="E(u,v)", main = "Gradiente Descendiente E(u,v)")
Sys.sleep(3)

digit.train <- read.table("datos/zip.train", quote="\"", comment.char="", 
                          stringsAsFactors=FALSE)

# Guardamos los 4 y los 8
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]

# Etiquetas
digitos.train = digitos15.train[,1]  
ndigitos.train = nrow(digitos15.train)
  
# Se retira la clase y se monta una matriz de tamanio 16x16
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos.train,16,16))
grises.train = lapply(seq(dim(grises)[1]), function(m) {matrix(grises[m,,],16)})

# Visualizamos las imágenes 
par(mfrow=c(2,2)) 

for(i in 1:4){
  imagen = grises[i,,16:1] # Se rotan para verlas bien
  image(z=imagen)
}


intensidad.train = unlist(lapply(grises.train, FUN=mean))

simetria <- function(matriz){
  
  matriz_original = matriz[1:256]
  matriz_invertida = matriz[,ncol(matriz):1]
  diferencia_matriz = matriz_original - matriz_invertida
  simetria = mean(abs(diferencia_matriz))
  
  simetria
}

etiquetas15.train <- digitos15.train[,1]
etiquetas15.train <- (etiquetas15.train-3)/(-2)

```{r warning=FALSE}

simetria.train = unlist(lapply(grises.train, simetria))

plot(x=intensidad.train, y=simetria.train, col=etiquetas15.train+3, xlab = "Intensidad", 
     ylab = "Simetria", main="Intensidad y Simetría (train)")
Sys.sleep(3)

digit.test <- read.table("datos/zip.test", quote="\"", comment.char="", 
                          stringsAsFactors=FALSE)

# Guardamos los 4 y los 8
digitos15.test = digit.test[digit.test[,1]==1 | digit.test[,1]==5,]

# Etiquetas
digitos.test = digitos15.test[,1]  
ndigitos.test = nrow(digitos15.test)
  
# Se retira la clase y se monta una matriz de tamanio 16x16
grises = array(unlist(subset(digitos15.test,select=V1)),c(ndigitos.test,16,16))
grises.test = lapply(seq(dim(grises)[1]), function(m) {matrix(grises[m,,],16)})

intensidad.test = unlist(lapply(grises.test, FUN=mean))

etiquetas15.test <- digitos15.test[,1]
# Convertimos las --etiquetas 
etiquetas15.test <- (etiquetas15.test-3)/(-2)

simetria.test = unlist(lapply(grises.test, simetria))

plot(x=intensidad.test, y=simetria.test, col=etiquetas15.test+3, xlab = "Intensidad", 
     ylab = "Simetria", main="Intensidad y Simetría (test)")



  11. Implementación de la regresión lineal para obtención de los pesos, además pinto la grafica de la recta:

Regression_Lin <- function(datos, etiq){
  x <- cbind(1, data.matrix(datos))
   
  x.svd <- svd(x)
  
  v <- x.svd$v
  d <- x.svd$d

  diagonal <- diag(ifelse(d>0.0001, 1/d, d))
  xx <- v %*% (diagonal^2) %*% t(v)
  pseudoinversa <- xx %*% t(x)
  w <- pseudoinversa %*% as.numeric(etiq)
  c(w)
}

datos.train = cbind(intensidad.train, simetria.train)

plot(datos.train, xlab = "Intensidad Promedio", ylab = "Simetria",
     col = etiquetas15.train+3, main = "Regresión Lineal")

w = Regression_Lin(datos.train, etiquetas15.train)

abline(-w[1]/w[3], -w[2]/w[3], col = 5, lwd = 2)

norma <- function(w_old, w_new){
  sqrt(sum((w_old - w_new)^2))
}

SGD <- function(datos, etiquetas, vini = c(0,0,0), mu = 0.01, nIter = 500){ 
  
  w_nuevo <- vini
  w_anterior <- c(0,0,0)
  iters <- 0
  
  # Columna de 1's
  datos <- cbind(1, datos)
  seguir <- TRUE
 
  while (iters < nIter & seguir){
    w_anterior <- w_nuevo
    permutacion <- sample(1:length(etiquetas)) 
    
    for(i in permutacion){
      gradiente <- (-etiquetas[i]*datos[i,]) / (1 + exp(etiquetas[i] * w_nuevo %*%datos[i,]))
      w_nuevo <- w_nuevo - mu * gradiente
    }
    
    iters <- iters + 1
    
    if(norma(w_anterior, w_nuevo) < mu ) seguir <- FALSE
  }
      
  cat("Pesos: ", w_nuevo, "\nValores de la Recta: ", -w_nuevo[1]/w_nuevo[3], -w_nuevo[2]/w_nuevo[3], "\nIteraciones: ", iters-1)
  
  c(w_nuevo,iters)
  
}

set.seed(3)	# se establece la semilla
simula_unif = function (N=2,dims=2, rango = c(0,1)){
 m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
 nrow = N, ncol=dims, byrow=T)
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

set.seed(3)

datos.train <- cbind(intensidad.train, simetria.train)
plot(datos.train, xlab = "Intensidad Promedio", ylab = "Simetria",
     col = etiquetas15.train+3, main = "SGD TRAIN")
Sys.sleep(3)

pesos01.train <-Regression_Lin(datos.train, etiquetas15.train)
pesos02.train <- SGD(datos.train, etiquetas15.train, pesos01.train, 100)

abline(-pesos02.train[1]/pesos02.train[3], -pesos02.train[2]/pesos02.train[3], 
       col = 3, lwd = 2)


datos.test <- cbind(intensidad.test, simetria.test)

plot(datos.test, xlab = "Intensidad Promedio", ylab = "Simetria", 
     col = etiquetas15.test+3, main = "SGD TEST")
Sys.sleep(3)

abline(-pesos01.train[1]/pesos01.train[3], -pesos01.train[2]/pesos01.train[3],
       col = 5, lwd = 2) 

abline(-pesos02.train[1]/pesos02.train[3], -pesos02.train[2]/pesos02.train[3],  col = 3, lwd = 2)

recta_regresion03 <- c(-pesos02.train[1]/pesos02.train[3], 
                      - pesos02.train[2]/pesos02.train[3])

# Etiquetamos los puntos a partir de la recta de regresión
etiquetas_regresion01 <- sign(datos.train[,2] 
    - recta_regresion03[1]*datos.train[,1] 
    - recta_regresion03[2])


Ein <- 0
Ein <- sum(etiquetas15.train != etiquetas_regresion01) / length(etiquetas15.train)

Ein

recta_regresion03 = c(-pesos02.train[1]/pesos02.train[3], 
                      - pesos02.train[2]/pesos02.train[3])

# Etiquetamos los puntos a partir de la recta de regresión
etiquetas_regresion01 <- sign(datos.test[,2] - 
  recta_regresion03[1]*datos.test[,1] - recta_regresion03[2])

Etest <- 0
Etest = sum(etiquetas15.test != etiquetas_regresion01) / length(etiquetas15.test)

Etest

# Obtenemos el tamaño de los datos
N <- nrow(datos.train)

# Calculamos el segundo término de la fórmula
x <- sqrt( (8/N) * log((4*((2*N)^(3) + 1))/0.05) )

# Obtenemos el valor de Eout
cota_Ein_Eout <- Ein + x
cat ("Cota en E_in: ", cota_Ein_Eout)

# Obtenemos el tamaño de los datos
N <- nrow(datos.test)

# Calculamos el segundo término de la fórmula
x <- sqrt( (1/(2*N)) * log( 2 / 0.05) )

cota_Ein_Eout <- Etest + x
cat ("Cota en E_test: ", cota_Ein_Eout)
cota_Ein_Eout




