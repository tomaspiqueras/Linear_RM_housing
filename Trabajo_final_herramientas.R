library(forecast)
library(latticeExtra)
install.packages("lattice")
library(ggplot2)
library(readxl)
library(lmtest)
Housing <- read_excel("~/Documents/datos_herramientas.xlsx")
View(datos_herramientas)  
#incluimos una secuencia trimestral desde el final del primer trimestre del 2002 donde comienza nuestra serie
Housing$PERIODS <- seq(as.Date("2002/3/1"), by="quarter", length.out=89)
#Graficamos las viviendas iniciadas para cada uno de los periodos, donde queda perfectamente reflejada la crisis del 2008 con el descenso acusado
ChartHousing<-ggplot(Housing, aes(x=PERIODS, y=HOUSING )) + 
  geom_line(colour="blue")  + 
  geom_point( size=2, shape=21, fill="white", colour="black") + 
  theme_minimal()
ChartHousing

xyplot(HOUSING + BIDDING ~ PERIODS, Housing, type="l") # Plot of two variables at the same time

obj1 <- xyplot(HOUSING  ~ PERIODS, Housing, type = "l" , lwd=2, ylab="HOUSING",  xlab="PERIODS") # Double Y-axis is added
obj2 <- xyplot(BIDDING ~ PERIODS, Housing, type = "l", lwd=2, ylab="ORDERS")  # We build each series separately
