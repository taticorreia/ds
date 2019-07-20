# Estudo de Caso em R - Detecção e Tratamento de Outliers

# Pasta de trabalho
setwd("~/Dropbox/DSA/AnaliseEstatisticaI/Modulo03/EstudoCaso")

# Para entender melhor as implicações dos outliers, vamos comparar o ajuste de um modelo de regressão linear simples 
# no conjunto de dados de carros com e sem outliers. 
# Para distinguir claramente o efeito, introduzimos manualmente valores extremos no conjunto de dados original dos carros. 
# Então, fazemos previsões em ambos os conjuntos de dados.

# Dataset cars (que vem junto com o pacote básico da linguagem R)
cars1 <- cars[1:30, ] 

# Injetando outliers nos dados
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  

# Novo dataset, agora com outliers
cars2 <- rbind(cars1, cars_outliers) 

# Plot dos dados com outliers
par(mfrow=c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), main="Com Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

# Plot dos dados originais sem outliers
plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Sem Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

# Analisando os gráficos fica evidente como os outliers afetam o resultado da análise.

# Detectando Outliers

# Dataset
ozone <- read.csv("ozone.csv")

# Para variável categórica

# # Padrão claro é perceptível.
boxplot(ozone_reading ~ Month, data=ozone, main="Ozone reading across months") 

# Isso pode não ser significativo, pois a variável dia da semana é um subconjunto do mê.
boxplot(ozone_reading ~ Day_of_week, data=ozone, main="Ozone reading for days of week")  

# Para variável contínua 
boxplot(ozone_reading ~ pressure_height, data=ozone, main="Boxplot for Pressure height (continuos var) vs Ozone")


# Tratamento
# Depois que os outliers forem identificados, você poderá corrigi-los usando uma das seguintes abordagens.

# Imputação
# Esse método consiste em substituir os outliers pela média / mediana / moda

# Capping
# Para valores missing que estão fora dos limites de 1,5 * IQR, poderíamos limitá-los, 
# substituindo as observações fora do limite inferior pelo valor de 5% percentil 
# e aquelas que estão acima do limite superior, com o valor de 95% percentil. 




