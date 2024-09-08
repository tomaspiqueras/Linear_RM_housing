library(forecast)
library(latticeExtra)
library(ggplot2)
library(readxl)
library(lmtest)
library(dplyr)
Housing <- read_excel("~/Documents/datos_herramientas.xlsx")
View(Housing)  

Housing$PERIODS <- seq(as.Date("2002/3/1"), by="quarter", length.out=89)

ChartHousing<-ggplot(Housing, aes(x=PERIODS, y=HOUSING )) + 
  geom_line(colour="blue")  + 
  geom_point( size=2, shape=21, fill="white", colour="black") + 
  theme_minimal()
ChartHousing


xyplot(HOUSING + BIDDING ~ PERIODS, Housing, type="l")

obj1 <- xyplot(HOUSING  ~ PERIODS, Housing, type = "l" , lwd=2, ylab="HOUSING",  xlab="PERIODS") 
obj2 <- xyplot(BIDDING ~ PERIODS, Housing, type = "l", lwd=2, ylab="ORDERS")

ChartHousing_Orders<-doubleYScale(obj1, obj2, add.ylab2 = TRUE)
ChartHousing_Orders


xyplot(HOUSING + UNEMPLOYMENT ~ PERIODS, Housing, type="l") 
obj1 <- xyplot(HOUSING  ~ PERIODS, Housing, type = "l" , lwd=2, ylab="Viviendas iniciadas",  xlab="Periodo") 
obj2 <- xyplot(UNEMPLOYMENT ~ PERIODS, Housing, type = "l", lwd=2, ylab="Desempleo")  
ChartHousing_Unempl<-doubleYScale(obj1, obj2, add.ylab2 = TRUE) 
ChartHousing_Unempl


xyplot(HOUSING + INVESTMENT ~ PERIODS, Housing, type="l") 

obj1 <- xyplot(HOUSING  ~ PERIODS, Housing, type = "l" , lwd=2, ylab="Viviendas iniciadas",  xlab="Periodo") 
obj2 <- xyplot(INVESTMENT ~ PERIODS, Housing, type = "l", lwd=2, ylab="Formación bruta de capital")  

ChartHousing_Invest<-doubleYScale(obj1, obj2, add.ylab2 = TRUE) 
ChartHousing_Invest


Housing_selected <- Housing_numeric %>% 
  select(INVESTMENT, GDP, HOUSING, BIDDING, INTEREST_RATE)


pairs(Housing_selected)

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

anova(model3,model1)

anova(model3,model4)

anova(model4,model5)

anova(model5,model1)

residuals<-rstandard(model5) 
par(mfrow=c(1,3)) 
hist(residuals) 
boxplot(residuals) 
qqnorm(residuals) 
qqline(residuals)
shapiro.test(residuals)

par(mfrow=c(1,1))
ChartResidualsModel3<-plot(fitted.values(model1),rstandard(model5), xlab="valores ajustados", ylab="residuos estandarizados") + # chart 2D 
  abline(h=0) 

plot(as.ts(residuals))
acf(residuals)
Box.test(residuals, lag=10, type="Ljung-Box") 



