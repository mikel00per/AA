knitr::opts_chunk$set(echo = TRUE)
library("tinytex")
library("rmarkdown")
library("caret")
library("rpart")
library("leaps")
library("e1071")
library("maptree")
library("FNN")
library("party")
library("gbm")
library("lattice")
library("randomForest")
library("MASS")
set.seed(3)	# se establece la semilla

datos <- read.csv("./datos/parkinsons_updrs.data", head=T, sep = ",")

# Eliminamos: sujeto, edad, sexo y test_time:
datos <- datos[,-(1:4)]
summary(datos)

# Elegimos los indices para el train, 80% de los datos 
idx.Train = sample(nrow(datos),size = nrow(datos)*0.8)

# Formamos loc conjuntos train y test. 
train <- as.data.frame(datos[idx.Train,])
test  <- as.data.frame(datos[-idx.Train,])

cat("Dim train : ", dim(train), "\n")
cat("Dim test  : ", dim(test ), "\n")
Sys.sleep(3)

nearZeroVar(train)
nearZeroVar(test)

perdidos.train <- sum(is.na(train))
perdidos.test  <- sum(is.na(test ))
cat("Num valores perdidos: ", perdidos.train, ",", perdidos.test,"\n")
Sys.sleep(3)

# Aprendemos del train
modelo.lm1 = lm(motor_UPDRS ~ . - total_UPDRS, data = train)
modelo.lm2 = lm(total_UPDRS ~ . - motor_UPDRS, data = train)

# Mostramos la interpretación que ha hecho el modelo
par (mfrow = c (2,2))
plot(modelo.lm1)
plot(modelo.lm2)
Sys.sleep(3)

# Aprendemos cual es el mejor valor libre de dos cifras. 
obj1 = best.tune(svm, motor_UPDRS ~ . - total_UPDRS, data = train)
obj2 = best.tune(svm, total_UPDRS ~ . - motor_UPDRS, data = train)
print(obj1)
print(obj2)
Sys.sleep(3)

# Aprendemos del train usando ese gamma
modelo.svm1 = svm(motor_UPDRS ~ . - total_UPDRS, 
                  data = train, gamma = 0.06, epsilon = 0.1)
modelo.svm2 = svm(total_UPDRS ~ . - motor_UPDRS, 
                  data = train, gamma = 0.06, epsilon = 0.1)

modelo.gbm1 = gbm(motor_UPDRS ~ . - total_UPDRS, data = train)
modelo.gbm2 = gbm(total_UPDRS ~ . - motor_UPDRS, data = train)
print(modelo.gbm1)
print(modelo.gbm2)

plot(modelo.gbm1)
plot(modelo.gbm2)
Sys.sleep(3)

# Aprendemos el modelo
modelo.rf1 = randomForest(motor_UPDRS ~ . - total_UPDRS, data = train)
modelo.rf2 = randomForest(total_UPDRS ~ . - motor_UPDRS, data = train)

par (mfrow = c (1,2))
plot(modelo.rf1)
plot(modelo.rf2)
Sys.sleep(3)

eval_model <- function(model,type=1, Boosting = F) {
          
  ein <- 0
  eout <- 0
  # Para boosting neceesitamos el número de arboles que van a ser
  # usados para pronosticar, que los ajustaremos a 100. 
  if(Boosting == FALSE){
    pred_train <- predict(model,newdata = train)
    pred_test <- predict(model,newdata = test)
    # Tipo indica si estamos hablando de motor_UDPRS o total_UDPRS
    if (type == 1) {
      ein <- sqrt(mean((pred_train-train$motor_UPDRS)^2))
      eout <- sqrt(mean((pred_test-test$motor_UPDRS)^2))
    }else{
      ein<- sqrt(mean((pred_train-train$total_UPDRS)^2))
      eout<- sqrt(mean((pred_test-test$total_UPDRS)^2))
    } 
  }else{
    pred_train <- predict(model,newdata = train, n.trees = 100)
    pred_test <- predict(model,newdata = test, n.trees = 100)

    if (type == 1) {
      ein<- sqrt(mean((pred_train-train$motor_UPDRS)^2))
      eout<- sqrt(mean((pred_test-test$motor_UPDRS)^2))
    }else{
      ein<- sqrt(mean((pred_train-train$total_UPDRS)^2))
      eout<- sqrt(mean((pred_test-test$total_UPDRS)^2))
    } 
  }
  
  return (c(ein,eout))
}

eval.lm1 = eval_model(modelo.lm1,1)
eval.lm2 = eval_model(modelo.lm2,2)

eval.svm1 = eval_model(modelo.svm1,1)
eval.svm2 = eval_model(modelo.svm2,2)

eval.rf1 = eval_model(modelo.rf1,1)
eval.rf2 = eval_model(modelo.rf2,2)

eval.gbm1 = eval_model(modelo.gbm1,1,T)
eval.gbm2 = eval_model(modelo.gbm2,2,T)

cat("LM    \n     motor_UPDRS - Ein: ", eval.lm1[1], 
    "      \n     total_UPDRS - Ein: ", eval.lm2[1],
    
    "\nSVM \n     motor_UPDRS - Ein: ", eval.svm1[1],
    "      \n     total_UPDRS - Ein: ", eval.svm2[1],

    "\nBST \n     motor_UPDRS - Ein: ", eval.gbm1[1],
    "      \n     total_UPDRS - Ein: ", eval.gbm2[1],
    
    "\nRF  \n     motor_UPDRS - Ein: ", eval.rf1[1], 
    "      \n     total_UPDRS - Ein: ", eval.rf2[1])
Sys.sleep(3)

cat(
    "\nLM  \n     motor_UPDRS - Ein: ", eval.lm1[2], 
    "      \n     total_UPDRS - Ein: ", eval.lm2[2],
    
    "\nSVM \n     motor_UPDRS - Ein: ", eval.svm1[2],
    "      \n     total_UPDRS - Ein: ", eval.svm2[2],

    "\nBST \n     motor_UPDRS - Ein: ", eval.gbm1[2],
    "      \n     total_UPDRS - Ein: ", eval.gbm2[2],
    
    "\nRF  \n     motor_UPDRS - Ein: ", eval.rf1[2], 
    "      \n     total_UPDRS - Ein: ", eval.rf2[2])
Sys.sleep(3)



# Calculamos la transformación
trans = preProcess(train, method = c("center","scale"))
trans2 = preProcess(train, method = c("center","scale", "pca"))

# Obtenemos el nuevo conunto de datos.
train = predict(trans, train)
test = predict(trans, test)

train.pca = predict(trans2, train)
test.pca = predict(trans2, test)

cat("Dim Train con PCA: ", dim(train.pca))
Sys.sleep(3)

# Aprendemos de nuevo los modelos
modelo2.lm1 = lm(motor_UPDRS ~ . - total_UPDRS, data = train)
modelo2.lm2 = lm(total_UPDRS ~ . - motor_UPDRS, data = train)
modelo2.svm1 = svm(motor_UPDRS ~ . - total_UPDRS,
                   data = train, gamma = 0.06, epsilon = 0.1)
modelo2.svm2 = svm(total_UPDRS ~ . - motor_UPDRS,
                   data = train, gamma = 0.06, epsilon = 0.1)
modelo2.rf1 = randomForest(motor_UPDRS ~ . - total_UPDRS, data = train)
modelo2.rf2 = randomForest(total_UPDRS ~ . - motor_UPDRS, data = train)
modelo2.gbm1 = gbm(motor_UPDRS ~ . - total_UPDRS, data = train)
modelo2.gbm2 = gbm(total_UPDRS ~ . - motor_UPDRS, data = train)

# Evaluamos los modelos
eval2.lm1 = eval_model(modelo2.lm1,1)
eval2.lm2 = eval_model(modelo2.lm2,2)
eval2.svm1 = eval_model(modelo2.svm1,1)
eval2.svm2 = eval_model(modelo2.svm2,2)
eval2.rf1 = eval_model(modelo2.rf1,1)
eval2.rf2 = eval_model(modelo2.rf2,2)
eval2.gbm1 = eval_model(modelo2.gbm1,1,T)
eval2.gbm2 = eval_model(modelo2.gbm2,2,T)

cat("LM    \n     motor_UPDRS - Ein: ", eval2.lm1[1], 
    "      \n     total_UPDRS - Ein: ", eval2.lm2[1],
    
    "\nSVM \n     motor_UPDRS - Ein: ", eval2.svm1[1],
    "      \n     total_UPDRS - Ein: ", eval2.svm2[1],

    "\nBST \n     motor_UPDRS - Ein: ", eval2.gbm1[1],
    "      \n     total_UPDRS - Ein: ", eval2.gbm2[1],
    
    "\nRF  \n     motor_UPDRS - Ein: ", eval2.rf1[1], 
    "      \n     total_UPDRS - Ein: ", eval2.rf2[1])
Sys.sleep(3)

cat(
    "\nLM  \n     motor_UPDRS - Eout: ", eval2.lm1[2], 
    "      \n     total_UPDRS - Eout: ", eval2.lm2[2],

    "\nSVM \n     motor_UPDRS - Eout: ", eval2.svm1[2],
    "      \n     total_UPDRS - Eout: ", eval2.svm2[2],

    "\nBST \n     motor_UPDRS - Eout: ", eval2.gbm1[2],
    "      \n     total_UPDRS - Eout: ", eval2.gbm2[2],
    
    "\nRF  \n     motor_UPDRS - Eout: ", eval2.rf1[2], 
    "      \n     total_UPDRS - Eout: ", eval2.rf2[2])
Sys.sleep(3)


# Aprendemos los modelos
rf.ajuste1 = randomForest(motor_UPDRS ~ . - total_UPDRS, data = train, 
                          ntree=200, importance = T, mtry=dim(train)[2])
rf.ajuste2 = randomForest(total_UPDRS ~ . - motor_UPDRS, data = train, 
                          ntree=200, importance = T, mtry=dim(train)[2])

rf.ajuste3 = randomForest(motor_UPDRS ~ . - total_UPDRS, data = train, 
                          ntree=200, importance = T, mtry=dim(train)[2] / 2)
rf.ajuste4 = randomForest(total_UPDRS ~ . - motor_UPDRS, data = train, 
                          ntree=200, importance = T, mtry=dim(train)[2] / 2)

rf.ajuste5 = randomForest(motor_UPDRS ~ . - total_UPDRS, data = train, 
                          ntree=200, importance = T, mtry=sqrt(dim(train)[2]))
rf.ajuste6 = randomForest(total_UPDRS ~ . - motor_UPDRS, data = train, 
                          ntree=200, importance = T, mtry=sqrt(dim(train)[2]))

# Los evaluamos 
rf.eval1 = eval_model(rf.ajuste1,1)
rf.eval2 = eval_model(rf.ajuste2,2)
rf.eval3 = eval_model(rf.ajuste3,1)
rf.eval4 = eval_model(rf.ajuste4,2)
rf.eval5 = eval_model(rf.ajuste5,1)
rf.eval6 = eval_model(rf.ajuste6,2)

cat("\nRF m=p 
          \n     motor_UPDRS - Ein: ", rf.eval1[1], 
         "\n     total_UPDRS - Ein: ", rf.eval2[1],
         "\n     motor_UPDRS - Eout: ", rf.eval1[2], 
         "\n     total_UPDRS - Eout: ", rf.eval2[2])

cat("\nRF m=p/2 
          \n     motor_UPDRS - Ein: ", rf.eval3[1], 
         "\n     total_UPDRS - Ein: ", rf.eval4[1],
         "\n     motor_UPDRS - Eout: ", rf.eval3[2], 
         "\n     total_UPDRS - Eout: ", rf.eval4[2])

cat("\nRF m=sqrt(p) 
          \n     motor_UPDRS - Ein: ", rf.eval5[1], 
         "\n     total_UPDRS - Ein: ", rf.eval6[1],
         "\n     motor_UPDRS - Eout: ", rf.eval5[2], 
         "\n     total_UPDRS - Eout: ", rf.eval6[2])
Sys.sleep(3)

