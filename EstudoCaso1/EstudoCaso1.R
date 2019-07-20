# Estudo de Caso 1 - Análise de Dados em Operadoras de Cartão de Crédito 

# Define pasta de trabalho
setwd("~/Dropbox/DSA/AnaliseEstatisticaI/Modulo02/EstudoCaso")

# Instala e carrega os pacotes
install.packages("mlbench")
install.packages("caret")
install.packages("e1071")
library(mlbench) 
library(caret)
library(e1071)

# Carrega o dataset
mydata <- read.csv("database.csv") 
View(mydata)

# Verifica o balanceamento das variáveis
skewness(mydata$LIMIT_BAL)
histogram(mydata$LIMIT_BAL)

# Sumariza os dados 
summary(mydata)
str(mydata)

# Calcula os parâmetros de pré-processamento
preprocessParams <- preProcess(mydata, method=c("BoxCox"))
print(preprocessParams)

# Transforma o dataset usando os parâmetros
transformed <- predict(preprocessParams, mydata)
mydata <- transformed

# Sumariza os dados
str(mydata)
skewness(mydata$LIMIT_BAL)
histogram(mydata$LIMIT_BAL)

# Transforma as variáveis categóricas
mydata$default.payment.next.month <- factor(mydata$default.payment.next.month)
mydata$SEX <- as.factor(mydata$SEX)
mydata$EDUCATION <-  as.factor(mydata$EDUCATION)
mydata$MARRIAGE <- as.factor(mydata$MARRIAGE)
mydata = na.omit(mydata)
summary(mydata)
str(mydata)

# Divide os dados em treino e teste
row <- nrow(mydata)
row 
set.seed(12345)
trainindex <- sample(row, 0.7*row, replace=FALSE)
training <- mydata[trainindex,]
validation <- mydata[-trainindex,]

trainingx<- training  
trainingy <- training[,24]            
validationx <- validation[,-24]
validationy <- validation[,24]

# Usa Random Forest para encontrar as variáveis mais relevantes
install.packages("randomForest")
library(randomForest)
rfModel = randomForest( training$default.payment.next.month ~ ., data=training, ntree=500 ) 
varImpPlot(rfModel) 


# Construção do Modelo com KNN

# Prepara os datasets com as melhores variáveis preditoras
trainingx<- training[,-c(2,3,4,5,24)] 
trainingy <- training[,24]            
validationx <- validation[,-c(2,3,4,5,24)]  
validationy <- validation[,24]

# Modelo KNN
knnModel = train(x=trainingx, y=trainingy, method="knn",
                 preProc=c("center","scale"),
                 tuneLength=10)

knnModel

# Plot da acurácia
plot(knnModel$results$k, 
     knnModel$results$Accuracy, 
     type="o",
     xlab="Número de Vizinhos Mais Próximos (K)",
     ylab="Acurácia", 
     main="Modelo KNN Para Previsão de Concessão de Cartão de Crédito")


# Fazendo previsões
knnPred = predict(knnModel, newdata=validationx)

knnPred
