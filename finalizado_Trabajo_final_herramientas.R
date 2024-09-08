library(forecast)
library(latticeExtra)
library(ggplot2)
library(readxl)
library(lmtest)
library(dplyr)
Housing <- read_excel("~/Documents/datos_herramientas.xlsx")
View(Housing)  
#incluimos una secuencia trimestral desde el final del primer trimestre del 2002 
#donde comienza nuestra serie
Housing$PERIODS <- seq(as.Date("2002/3/1"), by="quarter", length.out=89)
#Graficamos las viviendas iniciadas para cada uno de los periodos, donde queda
#perfectamente reflejada la crisis del 2008 con el descenso acusado
ChartHousing<-ggplot(Housing, aes(x=PERIODS, y=HOUSING )) + 
  geom_line(colour="blue")  + 
  geom_point( size=2, shape=21, fill="white", colour="black") + 
  theme_minimal()
ChartHousing

#graficamos las dos variables, que deben mostar una mayor correlación, las viviendas 
#iniciadas y la demanda de construcción medida a través de las viviendas visadas
xyplot(HOUSING + BIDDING ~ PERIODS, Housing, type="l")
#creamos una variable con cada elemento para poder graficarlo conjuntamente
obj1 <- xyplot(HOUSING  ~ PERIODS, Housing, type = "l" , lwd=2, ylab="HOUSING",  xlab="PERIODS") 
obj2 <- xyplot(BIDDING ~ PERIODS, Housing, type = "l", lwd=2, ylab="ORDERS")
#graficamos ambas variables incluyendo un eje doble vertical
ChartHousing_Orders<-doubleYScale(obj1, obj2, add.ylab2 = TRUE)
ChartHousing_Orders


#Graficamos el inicio de viviendas contra la tasa de desempleo
xyplot(HOUSING + UNEMPLOYMENT ~ PERIODS, Housing, type="l") 
obj1 <- xyplot(HOUSING  ~ PERIODS, Housing, type = "l" , lwd=2, ylab="Viviendas iniciadas",  xlab="Periodo") 
obj2 <- xyplot(UNEMPLOYMENT ~ PERIODS, Housing, type = "l", lwd=2, ylab="Desempleo")  
ChartHousing_Unempl<-doubleYScale(obj1, obj2, add.ylab2 = TRUE) 
ChartHousing_Unempl
#Observamos una fuerte subida del desempleo emparejada a la bajada en el inicio 
#de viviendas como fruto de la crisis del modelo productivo español en el año 2008 
#y su fuerte dependencia del mercado inmobiliario

xyplot(HOUSING + INVESTMENT ~ PERIODS, Housing, type="l") 

obj1 <- xyplot(HOUSING  ~ PERIODS, Housing, type = "l" , lwd=2, ylab="Viviendas iniciadas",  xlab="Periodo") 
obj2 <- xyplot(INVESTMENT ~ PERIODS, Housing, type = "l", lwd=2, ylab="Formación bruta de capital")  

ChartHousing_Invest<-doubleYScale(obj1, obj2, add.ylab2 = TRUE) 
ChartHousing_Invest

#encontramos un alto nivel de correlación entre la formación bruta de capital y 
#las viviendas iniciadas hasta el año 2012, cuando España pasa de un mdoelo productivo 
#enfocado en el sector inmobiliario, al sector exterior


# Seleccionar las variables específicas
Housing_selected <- Housing_numeric %>% 
  select(INVESTMENT, GDP, HOUSING, BIDDING, INTEREST_RATE)

# Graficar los pares de las variables seleccionadas
pairs(Housing_selected)

#creamos varios modelos de regresión lineal con el objetivo de estudiar 
#cual puede resultar más optimo
model1<- lm(HOUSING~BIDDING+UNEMPLOYMENT+INFLATION+INTEREST_RATE+INVESTMENT+GDP, data=Housing)
summary(model1)

model2<- lm(HOUSING~INFLATION+INTEREST_RATE+INVESTMENT, data=Housing)
summary(model2)

model3<- lm(HOUSING~BIDDING+UNEMPLOYMENT+GDP, data=Housing)
summary(model3)

model4<- lm(HOUSING~BIDDING+UNEMPLOYMENT+INVESTMENT+GDP, data=Housing)
summary(model4)

model5<- lm (HOUSING~BIDDING+UNEMPLOYMENT+INVESTMENT+GDP+INTEREST_RATE, data=Housing)
summary(model5)


#realizamso el test anova con el objetivo de conocer las distintas variables
anova(model3,model1)

anova(model3,model4)

anova(model4,model5)

anova(model5,model1)

# Para poder validar el modelo seguiremos tres pasos, comenzando con la normalidad de los residuos
#realizamos pruebas tanto visuales a través de gráficos como histogramas o boxplots, 
#como cuantiativas como el shapiro test
residuals<-rstandard(model5) 
par(mfrow=c(1,3)) 
hist(residuals) 
boxplot(residuals) 
qqnorm(residuals) 
qqline(residuals)
shapiro.test(residuals)

# graficando podriamos ver indicios de normalidad en los residuos, sin embargo a través 
#del test de shapiro obtenemos un p value  de 0.014

# El segundo supuesto a analizar es la homocedasticidad de los residuos, lo cual haremos 
#graficando el error estandar de los residuos

par(mfrow=c(1,1))
ChartResidualsModel3<-plot(fitted.values(model1),rstandard(model5), xlab="valores ajustados", ylab="residuos estandarizados") + # chart 2D 
  abline(h=0) 

# se puede ver un patrón donde los residuos están agrupados al inicio (en valores ajustados pequeños) 
#y luego parecen alejarse de la línea a medida que aumentan los valores ajustados. 
#Esto indica que podría haber heterocedasticidad

# En el tercer supuesto tratamos de analizar la independencia de los residuos
plot(as.ts(residuals))
acf(residuals)
Box.test(residuals, lag=10, type="Ljung-Box") 
#Esto significa que hay evidencia significativa de que los residuos no son independientes 
#y que hay autocorrelación presente.

# Visto lo anterior, pese que a los valores de R cuadrado podrian sugerior que tenemos
#un modelo altamente significativo, a la hora tetestear el modelo mediante el análisis 
#de la homocedasticidad, heterocedasticidad e independencia de los residuos, 
#vemos que no podríamos dar el modelo como válido. 



