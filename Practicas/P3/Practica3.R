library("tinytex")
library("rmarkdown")
library("caret")
library("rpart")
library("leaps")
library("e1071")
library("maptree")
library("nnet")
library("FNN")
library("party")
library("gbm")
library("lattice")
set.seed(3)	# se establece la semilla


# Leemos las matrices.
train <- read.csv("./datos/optdigits_tra.csv", head =FALSE)
test  <- read.csv("./datos/optdigits_tes.csv", head =FALSE)

# Guardamos sus clases
train.clases = train[,dim(train)[2]]
test.clases = test[, dim(test)[2]]

# Cambiamos el tipo de dato de las clases y les ponemos nombre
# a las filas y columnas. 
train[,dim(train)[2]] <- as.factor(train[,dim(train)[2]])
test[,dim(test)[2]] <- as.factor(test[,dim(test)[2]])
colnames(train) <- c(paste("P.",1:64),"Y")
colnames(test) <- c(paste("P.",1:64),"Y")

# Leemos las matrices.
train <- read.csv("./datos/optdigits_tra.csv", head =FALSE)
test  <- read.csv("./datos/optdigits_tes.csv", head =FALSE)

# Guardamos sus clases
train.clases = train[,dim(train)[2]]
test.clases = test[, dim(test)[2]]

# Cambiamos el tipo de dato de las clases y les ponemos nombre
# a las filas y columnas. 
train[,dim(train)[2]] <- as.factor(train[,dim(train)[2]])
test[,dim(test)[2]] <- as.factor(test[,dim(test)[2]])
colnames(train) <- c(paste("P.",1:64),"Y")
colnames(test) <- c(paste("P.",1:64),"Y")

# Demasiados atributos para este problema? 
# Aplicamos la dimensionalidad con PCA que es un filtro de 
# características no supervisado es sensible al escalado y 
# valores grandes. Usaré YeoJohson, centrado y escalado

# Hago la transformacion en train para poner los datos con:
# media 0 y desviación 1 para la regresión logistica. 
objTrans = preProcess(train, method = c("center", "scale", "pca"))
# Obtengo el nuevo conjunto de datos
train = predict(objTrans, train)
test = predict(objTrans, test)

cat("Dim Train Tras PCA: ", dim(train), "\n")
cat("Dim Test  Sin  PCA: ", dim(test),  "\n")


# Dentro del PCA ahora vamos a ver cuales son las 
# caracteristicas más relevantes según el train
subsetPCA = regsubsets(train$Y ~ . , data = train,
                       nvmax = dim(train)[2], method = "forward")
summaryPCA = summary(subsetPCA)

par(mfrow = c(1,2))

plot(summaryPCA$cp, xlab = "Número de variables (con PCA).", 
     ylab = "CP", type = "l")
plot(summaryPCA$bic, xlab = "Número de variables (con PCA).", 
     ylab = "BIC", type = "l")

cat("Mejor número de características - CP (con PCA):",
    which.min(summaryPCA$cp), "\n")
cat("Mejor número de características - BIC (con PCA):", 
    which.min(summaryPCA$bic), "\n")

# Usamos el nuevo train obtenido:
train = train[, c(1, as.vector(which(summaryPCA$outmat[28,]
                                     == "*")) + 1)]
test = test[, c(1, as.vector(which(summaryPCA$outmat[28,]
                                   == "*")) + 1)]

########################
## 1. Modelo Regresion Logistica
######################################################################
modelo.rgl <- multinom(train$Y ~ ., data=train)
prediccion.rgl <- predict(modelo.rgl, newdata = train, type = "class")
error.rgl <- (sum(train$Y != prediccion.rgl)/nrow(train))

########################
## 2. Modelo RPART
######################################################################
modelo.RPART <- rpart(train$Y ~ ., method="class",data = train)

# Pintamos los arboles
plot(modelo.RPART, uniform = TRUE, main = "Classification (RPART)")
text(modelo.RPART, all = TRUE, cex = 0.60)
draw.tree(modelo.RPART, cex = 0.25, nodeinfo = TRUE, col = gray(0:8/8))

# Predecimos los datos sobre el test y los clasificamos. 
prediccion.RPART <- predict(modelo.RPART, newdata = train, type = "class")
error.RPART <- (sum(train$Y != prediccion.RPART)/nrow(train))

########################
## 3. Modelo con SVM
######################################################################
modelo.SVM <- svm(train$Y ~ ., method="class",data = train)
summary(modelo.SVM)

# Predecimos los datos sobre el train y los clasificamos. 
prediccion.SVM <- predict(modelo.SVM, newdata = train, type = "class")

error.SVM <- (sum(train$Y != prediccion.SVM)/nrow(train))


cat("E_in  RGL: ", error.rgl, "\n")
cat("Clasi RGL: ", (1-error.rgl)*100, "%\n\n")

cat("E_in  RPART: ", error.RPART, "\n")
cat("Clasi RPART: ", (1-error.RPART)*100, "%\n\n")

cat("Error SVM: ", error.SVM, "\n")
cat("Clasi SVM: ", (1-error.SVM)*100, "%\n")


# Predecimos los datos sobre el test y los clasificamos. 
prediccion.SVM_test <- predict(modelo.SVM, newdata = test, type = "class")

error.SVM <- (sum(test$Y != prediccion.SVM_test)/nrow(test))
cat("Eout SVM: ", error.SVM, "\n")
cat("Clasi: ", (1-error.SVM)*100, "%\n")

# Tambiem probamos los de la RGL porque se basa en el ajuste de modelos 
# lineales como la regresion logistica.

prediccion.RGL_test <- predict(modelo.rgl, newdata = test, type = "class")

error.RGL <- (sum(test$Y != prediccion.RGL_test)/nrow(test))
cat("Eout RGL: ", error.RGL, "\n")
cat("Clasi: ", (1-error.RGL)*100, "%\n")


datos <-  read.csv("./datos/airfoil_self_noise.csv", header=F)
colnames(datos) = c("Hz","ANG","Long","vFlujo","espesor","ruido")

# Correlacion de los datos
correlacion <- cor(datos)
summary(correlacion[upper.tri(correlacion)])
# Buscamos var correlada
correlacion.alta <- findCorrelation(correlacion, cutoff = .75)
sum(correlacion.alta)

xyplot(Hz~ANG,datos,grid=T,type = c("p", "smooth"),, col.line = "darkorange",lwd = 2)
xyplot(Long~ANG,datos,grid=T,type = c("p", "smooth"),, col.line = "darkorange",lwd = 2)
xyplot(vFlujo~ANG,datos,grid=T,type = c("p", "smooth"),, col.line = "darkorange",lwd = 2)
xyplot(espesor~ANG,datos,grid=T,type = c("p", "smooth"),, col.line = "darkorange",lwd = 2)

# Sacrificamos la caracteristica espesor:
datos = datos[,-5]

# Reducimos una caracteristica que se puede traducir
# por la viscosidad cinemática
nu <- 1.568e-5 

datos$Re <- datos$Long * datos$vFlujo / nu
datos <- datos[,-c(3,4)]
datos <- datos[,c(1:2,4,3)]

names(datos)


# Eliminamos las variables muy cercanas a 0 como antes:
nvz <- nearZeroVar(datos, saveMetrics = TRUE)
nvz

datos <- datos[,!nvz$nzv]


# Partimos el cnj en el 75% de los datos para train y 25 para el test:
cat("Dim Datos: ", dim(datos), "\n")
enEntrenamiento <- createDataPartition(y=datos$ruido, p=0.70, list = FALSE)

# Guardamos test y train
train <- datos[enEntrenamiento,]
test  <- datos[-enEntrenamiento,]
cat("Dim Train: ", dim(train), "\n")
cat("Dim Test: ", dim(test), "\n")

train_o = train
test_o = test

######################################
##  Modelo 1 Regresion lineal
###################################################

modelo.lm = lm(train_o$ruido ~ ., data=train_o)

train_o = train
test_o = test

#################################
## Modelo 2 Arboles de regresion
#######################################################

modelo.tree = rpart(train_o$ruido ~ ., data=train_o)

train_o = train
test_o = test

###########################
## Modelo 3 Boostin
#######################################################
control = trainControl(method = "cv", number = 5, verboseIter=F)
gbmGrid <- expand.grid(interaction.depth = c(20,30,45), n.trees = 500, 
                       shrinkage = .1, n.minobsinnode = 10)

#modelo.gbm = train(train_o$ruido ~ ., data = train_o, method = "gbm",
#                 trControl = control, verbose = FALSE, tuneGrid = gbmGrid)

#plot(modelo.gbm)

#res <- eval_model(modelo.gbm)
#cat("Ein : ", res[1], "\n")
#cat("Eout : ", res[2], "\n\n")

train_o = train
test_o = test


###########################
## Modelo 4 Bagging- Caret
#######################################################
modelo.bag <- bag(train_o[,-7],train_o$ruido, B = 10,
                  bagControl = bagControl(fit = ctreeBag$fit,
                                          predict = ctreeBag$pred,
                                          aggregate = ctreeBag$aggregate))
train_o = train
test_o = test

#########################################
## Modelo 5 RandomFores
######################################################
modelo.rf <- train(x=train[,-7],y=train$ruido,method="rf",
                   trControl=trainControl(method = "cv", number = 4),
                   data=train,do.trace=F,ntree=250)
train_o = train
test_o = test


# Función para pintar graficas usando un modelo ante los datos train 
# y test esta funcion ha sido encontrada en internet, en stackoverflow
eval_model <- function(model) {
  
  pred_train <- predict(model,newdata = train)
  pred_test <- predict(model,newdata = test)
  
  # Scatter plots of predictions on Train and test sets
  plot(pred_train,train$ruido,xlim=c(100,150),ylim=c(100,150),col=1,
       pch=19,xlab = "Predicted ruido (dB)",ylab = "Actual ruido(dB)")
  points(pred_test,test$ruido,col=2,pch=19) 
  leg <- c("Train","Test")
  legend(100, 150, leg, col = c(1, 2),pch=c(19,19))
  
  # Scatter plots of % error on predictions on Train and test sets
  par(mfrow = c(2, 2))
  par(cex = 0.6)
  par(mar = c(5, 5, 3, 0), oma = c(2, 2, 2, 2))
  plot((pred_train - train$ruido)* 100 /train$ruido,
       ylab = "% Error de Prediction", xlab = "Index",
       ylim = c(-5,5),col=1,pch=19)
  legend(0, 4.5, "Train", col = 1,pch=19)
  plot((pred_test-test$ruido)* 100 /test$ruido,
       ylab = "% Error de Prediction",  xlab = "Index",
       ylim = c(-5,5),col=2,pch=19)
  legend(0, 4.5, "Test", col = 2,pch=19)
  
  # Actual data Vs Predictions superimposed for Train and test Data
  plot(1:length(train$ruido),train$ruido,pch=21,col=1,
       main = "Train: Actual ruido Vs Predicted ruido",
       xlab = "Index",ylab = "ruido (dB)")
  points(1:length(train$ruido),pred_train,pch=21,col=2)
  #leg <- c("Train","Predicted Train")
  legend(0, 140, c("Actual","Predicted"), col = c(1, 2),pch=c(21,21))
  plot(1:length(test$ruido),test$ruido,pch=21,col=1,
       main = "Test: Actual ruido Vs Predicted ruido",
       xlab = "Index",ylab = "ruido (dB)")
  points(1:length(test$ruido),pred_test,pch=21,col="red")
  legend(0, 140, c("Actual","Predicted"), col = c(1, 2),pch=c(21,21))
  
  ## Line graph de errors
  plot(pred_train-train$ruido,type='l',ylim=c(-5,+5),
       xlab = "Index",ylab = "Actual - Predicted",main="Train")        
  plot(pred_test-test$ruido,type='l',ylim=c(-5,+5),
       xlab = "Index",ylab = "Actual - Predicted",main="Test")
  
  ISRMSE<- sqrt(mean((pred_train-train$ruido)^2))
  OSRMSE<- sqrt(mean((pred_test-test$ruido)^2))
  
  return(c( ISRMSE,OSRMSE))
}

# Evaluamos
res <- eval_model(modelo.lm)
cat("Ein - LM     :", res[1], "\n")
lm.Eout = res[2]

# Evaluamos
res <- eval_model(modelo.tree)
cat("Ein - Tree   :", res[1], "\n")
tree.Eout = res[2]

# Evaluamos
res <- eval_model(modelo.bag)
cat("Ein - BAG    :", res[1], "\n")
bag.Eout = res[2]

# Evaluamos
res <- eval_model(modelo.rf)
cat("Ein - RF     :", res[1], "\n")
rf.Eout = res[2]

# Mostramos el Eout de los modelos.
cat("Eout - LM  : ", lm.Eout, "\n")
cat("Eout - Tree: ", tree.Eout, "\n")
cat("Eout - Bag  : ", bag.Eout, "\n")
cat("Eout - RF  : ", rf.Eout, "\n")









































