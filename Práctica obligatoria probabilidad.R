rm(list=ls())
setwd("C:/Users/1/Desktop/M�ster en Data Science/Primer trimestre/Fundamentos de An�lisis de Datos/Pr�ctica obligatoria Probabilidad")

install.packages("PASWR2")
library(PASWR2)

datos <- BATTERY
life <- datos$lifetime
head(life)

########Actividad 1.


hist(life)

datosAB <- split(datos$lifetime,datos$facility)
#Dos posibles distribuciones, valor de separaci�n 190.
datosA <- datosAB$A
datosB <- datosAB$B


hist(datosA) #mejor por separado.
plot(density(datosA),
     ylab='Densidad', col='blue3', xlab='', las=1, lwd=4)
hist(datosB)
plot(density(datosB),
     ylab='Densidad', col='green', xlab='x', las=1, lwd=4)
#m�s cerca de la normal gr�ficamente.

####QQplots con bandas.
qqnorm(datosA)
qqline(datosA)
require(car)
qqPlot(datosA, pch=20, ylab='y',
       main='QQplot')
#Desv�o en el segundo cuartil y en el tercero de forma destacable.
qqnorm(datosB)
qqline(datosB)
par(mfrow=c(1, 1))
qqPlot(datosB, pch=20, ylab='y',
       main='QQplot')
#Mayor desv�o en el primero y el final del tercero.

##Pruebas de normalidad.
shapiro.test(datos$lifetime)#total no normalidad.
shapiro.test(datosA)#mantener H0 de normalidad.
shapiro.test(datosB)#mantener H0 de normalidad.

########Actividad 2
med1 <- mean(datosA)
sd1 <- sd(datosA)
med2 <- mean(datosB)
sd2 <- sd(datosB)

#Mayor que 210.
pnorm(210,med1,sd1, lower.tail=FALSE)
#Menor que 175
pnorm(175,med2,sd2)
#cuantil 0,03 
qnorm(0.03,med2,sd2)

########Actividad 3.
#Valores mayores que 175.REPASAR.
prob <- 1-pnorm(175,med2,sd2)
1-pbinom(0, size = 10, prob = 1-prob, lower.tail = FALSE) #0,8828033
#La quinta sea la primera defectuosa. Distribuci�n geom�trica.
pgeom(5, 1-prob, lower.tail = FALSE) #0,9279367
#3 defectuosas de 20. Muestra sin reposici�n 5.

dhyper(x=3, m=20, k=5, n=17) # 0,3556804

########Actividad 4.
#Se fabrican 1000 bater�as por d�a.1000p. Poisson. 12 defectuosas al d�a.
1-sum(dpois(x=0:20, lambda = 12)) #1 - Poisson menor que 20. 0.01159774

ppois(0, lambda = 12) #6.144212e-06



#CONTINUAR
########Actividad 5.
n=5000 #bater�a semana de trabajo
a=100
b=185
pweibull(n,a,b, lower.tail=TRUE)
