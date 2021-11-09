library(PASWR2)
library(ggplot2)
library(dplyr)
library(scales)
library(nortest)

datos <- BATTERY

head(datos)


# Actividad 1

# • Realiza un histograma de todas las filas de la variable lifetime y comprueba que efectivamente nos
# interesa separar los datos.

ggplot(datos,aes(x=lifetime))+
  geom_histogram(position="identity",alpha=0.5,binwidth=0.25)+
  theme_bw()


# • Crea dos conjuntos de datos diferentes para los dos tipos de baterías, por ejemplo datosA y datosB.

datosA <- datos %>% filter(facility == "A")
datosB <- datos %>% filter(facility == "B")

head(datosA)
head(datosB)


# • Realiza ahora un histograma de cada uno de los tipos y comenta si te parece que los datos siguen una
# distribucion normal

ggplot(datosA,aes(x=lifetime,group=facility,fill=facility))+
  geom_histogram(position="identity",alpha=0.5,binwidth=0.25)+
  theme_bw()

ggplot(datosB,aes(x=lifetime,group=facility,fill=facility))+
  geom_histogram(position="identity",alpha=0.5,binwidth=0.25)+
  theme_bw()

## or

ggplot(datos,aes(x=lifetime,group=facility,fill=facility))+
  geom_histogram(position="identity",alpha=0.5,binwidth=0.25)+
  theme_bw()

## Efectivamente parece que ambos tipos de batería siguen una distribución normal


# • Confirma tus conclusiones con alguna/s de las herramientas vistas en clase (test de normalidad, gráfico
# Quantil-Quantil, tests de normalidad, . . . )

hist(datosA$lifetime, main = expression("Histograma datos Lifetime para A"),
     xlab = expression("Lifetime"),
     col = "steelblue", 
     border = "white", 
     bg = "white", 
     freq = FALSE)
curve(dnorm(x, mean(datosA$lifetime), sd(datosA$lifetime)), 
      add = TRUE, 
      lwd = 2, 
      lty = 2)

hist(datosB$lifetime, main = expression("Histograma datos Lifetime para B"),
     xlab = expression("Lifetime"),
     col = "steelblue", 
     border = "white", 
     bg = "white", 
     freq = FALSE)
curve(dnorm(x, mean(datosB$lifetime), sd(datosB$lifetime)), 
      add = TRUE, 
      lwd = 2, 
      lty = 2)

## Comparando visulamente con una distribución normal que toma como inputs la media y desviación estándar de cada 
## uno de los tipos de batería, parece que ambos tipos siguen una distribución normals


qqnorm(datosA$lifetime, pch = 20, col = alpha("red4", 0.5), las = 1)
grid()
qqline(datosA$lifetime, lwd = 2)

qqnorm(datosB$lifetime, pch = 20, col = alpha("red4", 0.5), las = 1)
grid()
qqline(datosB$lifetime, lwd = 2)

## El método del Gráfico Q-Q también nos indica que ambas distribuciones siguen una distribución normal, dado que 
## la distribución de la variable es muy similar a la distribución de comparación, siendo sobre todo en el centro
## una línea recta


shapiro.test(datosA$lifetime)

shapiro.test(datosB$lifetime)

## El test de Shapiro-Wilk nos indica también que ambas distribuciones son normales, dado que p > 0.05 en ambos casos


ad.test(datosA$lifetime)

ad.test(datosB$lifetime)

## De nuevo, el test de Anderson-Darling, nos indica que ambas distribuciones son normales, dado que en ambos casos
## el p-valor es mayor a 0.05


# Actividad 2

# Ahora que sabemos que nuestros datos siguen aproximadamente una distribución normal, tendríamos que
# estimar sus parámetros µ y σ. A partir de ahí, podemos realizar cálculo de probabilidades de la normal.

# • Realiza una estimación puntual de la media y la desviación típica de la población de cada tipo de
# baterías

lifetimeA = datosA$lifetime
mean_lifetimeA = mean(lifetimeA)
mean_lifetimeA
sd_lifetimeA = sd(lifetimeA)
sd_lifetimeA

lifetimeB = datosB$lifetime
mean_lifetimeB = mean(lifetimeB)
mean_lifetimeB
sd_lifetimeB = sd(lifetimeB)
sd_lifetimeB


# • Calcula la probabilidad de que una batería tomada al azar del tipo A dure más de 210 horas

diffA = 210 - mean_lifetimeA
distA = diffA/sd_lifetimeA
distA

pnorm(q=distA, lower.tail=FALSE)

## or

A_mas_210 = pnorm(q = 210, mean = mean_lifetimeA, sd = sd_lifetimeA, lower.tail = FALSE)
A_mas_210

# • Calcula la probabilidad de que una batería tomada al azar del tipo B dure menos de 175 horas

diffB = 175 - mean_lifetimeB
distB = diffB/sd_lifetimeB
distB

pnorm(q=distB, lower.tail=TRUE)

## or

B_menos_175 = pnorm(q = 175, mean = mean_lifetimeB, sd = sd_lifetimeB, lower.tail = TRUE)
B_menos_175


# • Encuentra cuál es la duración máxima del 3% de las pilas del tipo B que duran menos (ayuda: esto es
# equivalente a encontrar el cuantil 0.03 de la distribución)

qnorm(p = .03,
      mean = mean_lifetimeB,
      sd = sd_lifetimeB,
      lower.tail = TRUE,
      log.p = FALSE)


# Actividad 3

# Vamos a centrarnos ahora en las baterías de tipo B. Supongamos que una duración por debajo de 175 horas
# no es aceptable para el usuario de la batería. En la actividad anterior hemos calculado la probabilidad p de
# que esto suceda. Entonces, si tomamos una batería del tipo B al azar y comprobamos si dura menos de 175
# horas, estamos realizando un experimento de Bernoulli con probabilidad p.

# • Calcula la probabilidad de que en un lote de 10 baterías, no haya ninguna defectuosa (ayuda: distribución
# binomial).

pbinom(q = 0, size = 10, prob = B_menos_175)

## or

(1 - B_menos_175)^10

# • Imagina que las baterías se fabrican en serie e independientemente. ¿Cuál es la probabilidad de que la
# batería producida en quinto lugar sea la primera defectuosa? (ayuda: distribución geométrica.)

## Distribución geométrica

pgeom(q = 5, prob = B_menos_175, lower.tail = FALSE)

## Distribución binomial negativa

pnbinom(q = 5, size = 1, prob = B_menos_175, lower.tail = FALSE)


# • Supongamos que en una caja de 20 baterías van 3 defectuosas. ¿Cuál es la probabilidad de que al
# tomar una muestra sin reposición de 5 baterías al menos una sea defectuosa? (ayuda: distribución
# hipergeométrica)

dhyper(x = 1, m = 3, k = 5, n = 20-3)


# Actividad 4

# Seguimos con las baterías de tipo B, pero en vez de hacer experimentos de Bernoulli queremos estudiar el
# número de baterías defectuosas fabricadas cada día. Supongamos que se fabrican 1000 baterías cada día.
# Entonces, cada día en promedio se estarán produciendo aproximadamente 1000 × p baterías, y el número de
# baterías defectuosas por día sigue una distribución de Poisson. Tomemos 12 como ese promedio de baterías
# defectuosas cada día. (ayuda: repasa qué modelo de distribución modeliza estos recuentos de eventos raros
# con una tasa media por unidad de tiempo)

# • ¿Cuál es la probabilidad de que un día se produzcan más de 20 baterías defectuosas?

ppois(q = 20, lambda = 12, lower.tail = FALSE)


# • ¿Cuál es la probabilidad de que un día no salga ninguna batería defectuosa de la fábrica?

ppois(q = 0, lambda = 12)


# • La fábrica funciona de lunes a viernes. ¿Qué distribución sigue el número de baterías defectuosas por
# semana? Justifica qué propiedad se aplica.

# Sigue también una distribución de Poisson. La distribución Poisson tiene la propiedad aditiva o reproductiva, esto es, la
# suma de variables aleatorias independientes con distribución Poisson de parámetro λi también tiene una distribución Poisson 
# de parámetro λ (suma de los diferente parámetros λ= λ1+ λ2+...+ λk).

# Fuente: https://www.cartagena99.com/recursos/alumnos/apuntes/2014%20-T1%20GradoADE_EstadisticaEmpresarialII_Tema1__Resumen_v1.pdf


# Actividad 5

# El departamento de I+D de la empresa que fabrica las baterías tipo B está investigando nuevos materiales y
# métodos para mejorar la vida útil de las baterías. En particular, quieren llegar a diseñar una batería cuya
# duración siga una distribución de Weibull con parámetros a = 100 y b = 185.

# • Realiza una simulación de la producción semanal de baterías (recuerda: 5 días de producción, a 1000
# baterías por día). Guarda los datos en un vector.

curve(dweibull(x, 100, 185), 
      from = 170, 
      to = 195, 
      n = 5000, 
      lwd = 2, 
      main = "Distribución Weibull a = 100 y b = 185")
legend(x = 192, y = .2, c("a = 100", "b = 185"))

weibull_vector = curve(dweibull(x, 100, 185), 
                       from = 170, 
                       to = 195, 
                       n = 5000, 
                       lwd = 2, 
                       main = "Distribución Weibull a = 100 y b = 185")


# • Con este nuevo proceso, ¿se mejora realmente la duración media de las baterías?
# (ayuda: puedes usar los datos simulados o la expresión de la esperanza de una Weibull)

mean_lifetimeB

esp_weibull = mean(rweibull(5000, shape = 100, scale = 185))
esp_weibull

# Efectivamente, con este proceso aumenta la duración media de las baterías, de 179.68 a alrededor de 184


# • Los ingenieros no lo tienen muy claro (parece que la diferencia no es tanta en promedio y los nuevos
# materiales son costosos). Para demostrarles que merece la pena, calcula la proporción de baterías
# defectuosas que producirá probablemente el nuevo proceso y compárala con el anterior (la p que
# calculamos en la actividad 2)

B_menos_175

B_menos_175_weibull = pweibull(175, 100, scale = 185)
B_menos_175_weibull

B_menos_175 / B_menos_175_weibull

# De la nueva manera la proporción de baterías defectuosas sería más de 3 veces menor