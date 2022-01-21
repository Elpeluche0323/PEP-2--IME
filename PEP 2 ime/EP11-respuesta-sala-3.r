# Nombres:
# Estefanía Alvarez 20.371.287-1
# Stephan Silva 20.298.778-8
# Francisco Moreno 19.892.183-1
# Maximiliano Araya 20.467.583-k
# Daniel Jara 20.113.716-0

# se importan librerías
require(ez)
library(ggpubr)
library(tidyr)
library(dplyr)
library(boot)
library(simpleboot)
library(bootES)



# librería para leer archivos .xls
if (!require(readxl)){
  install.packages(readxl)
  require (readxl)
}





#   ____                                          _               _ 
#  |  _ \   _ __    ___    __ _   _   _   _ __   | |_    __ _    / |
#  | |_) | | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |   | |
#  |  __/  | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |   | |
#  |_|     |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_|
#                         |___/                                     




# Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.




# INVESTIGACION: La media del ingreso per cápita, en la región metropolitana, entre profesionales hombres y
# mujeres es la misma.

# Denotando como µA al promedio del ingreso per cápita de profesionales hombres y como µB al promedio 
# del ingreso per cápita de profesionales mujeres, dentro de la region metropolitana, entonces:
# H0: µA - µB = 0
# HA: µA - µB != 0

# Función para hacer una permutación y calcular el estadístico
# de interés.
# Argumentos :
# - muestra _1 , muestra _2: vectores numéricos con las muestras a comparar .
# Valor :
# - diferencia E_1 - E _2.
permutar <- function(muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutación.
  permutacion <- sample (c( muestra_1 , muestra_2) , size = n_1 + n_2,
                         replace = FALSE )
  
  # Asignar elementos a los dos grupos .
  
  permutacion_1 <- permutacion [1 : n_1]
  permutacion_2 <- permutacion [n_1 + 1 : n_2]
  # Calcular y devolver la diferencia de medias .
  return(mean(permutacion_1) - mean(permutacion_2))
}




# Función para calcular el valor p.
# Argumentos :
# - distribución : distribución nula del estadístico de interés.
# - valor _ observado : valor del estad ? stico de inter ?s para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hip? tesis alternativa . "two. sided " para
# hip? tesis bilateral , " greater " o " less " para hip ? tesis unilaterales .
# Valor :
# - el valorp calculado .

calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}


# Función para graficar una distribución.
# Argumentos :
# - distribución : distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.
graficar_distribucion <- function(distribucion , ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones , x = "distribución",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", ...)
  
  qq <- ggqqplot(observaciones , x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura <- ggarrange(histograma, qq, ncol = 2 , nrow = 1)
  print(figura)
}

#----------------------------

set.seed(450)   # se fija una semilla aleatoria
# se obtienen los datos, por cierto, es necesario seleccionar el archivo desde la ventana que emerge

datos <- read_excel(file.choose())
datos <- sample_n(datos, 450)   # se seleccionan 500 hogares aleatoriamente
datos <- datos %>% select(region, sexo, e7.subarea, ytotcorh)
datosH <- datos %>% filter(region == "Región Metropolitana de Santiago" & e7.subarea != "NA" & sexo=="Hombre")
datosM <- datos %>% filter(region == "Región Metropolitana de Santiago" & e7.subarea != "NA" & sexo=="Mujer")
print(datosM)
print(datosH)


cat("Prueba de permutaciones \n\n")
cat("Hipótesis alternativa: \n")
observado <- mean(datosH$ytotcorh, na.rm = TRUE) - mean(datosM$ytotcorh, na.rm = TRUE) #se calcula la diferencia de las medias.
cat ("Valor observado: ", observado , "\n")
repeticiones = 5999
muestra_1 <- datosH$ytotcorh
muestra_2 <- datosM$ytotcorh
distribucion <- rep(NA, repeticiones)

for( i in 1: repeticiones ) {
  distribucion [ i ] <- permutar(muestra_1, muestra_2)
}

graficar_distribucion(distribucion)


valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")

cat("Valor p:", valor_p, "\n\n")



#                                                _               ____  
#  _ __    _ __    ___    __ _   _   _   _ __   | |_    __ _    |___ \ 
# | '_ \  | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |     __) |
# | |_) | | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |    / __/ 
# | .__/  |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_____|
# |_|                    |___/                                         




# Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping
# aunque este no sea necesario.



# INVESTIGACIÓN: Se requiere investigar en base a la encuesta Casen 2017, si las medias del número de viviendas en el sitio
# son similares en tres de las macro zonas de Chile (norte, centro, y sur).


# Información relevante sobre las macrozonas de chile:
# Macrozona Norte: Arica y Parinacota, Tarapacá, Antofagasta, Atacama. 
# Macrozona Centro: Coquimbo y Valparaíso.
# Macrozona Sur: La Araucanía, Los Lagos y Los Ríos.



set.seed(1313)   # se fija una semilla aleatoria

# se obtienen los datos, por cierto, es necesario seleccionar el archivo desde la ventana que emerge
datos <- read_excel(file.choose())

datos <- sample_n(datos, 500)   # se seleccionan 500 hogares aleatoriamente

datos <- datos %>% select(region, v8)   # se seleccionan las columnas de interés



# se separan los datos por tablas según la macrozona
norte <- datos %>% filter(region == "Región de Tarapacá" | region == "Región de Antofagasta" | region == "Región de Atacama")
centro <- datos %>% filter(region == "Región de Coquimbo" | region == "Región de Valparaíso")
sur <- datos %>% filter(region == "Región de La Araucanía" | region == "Región de Los Lagos")



# Hipótesis a contrastar:
# H0: El número de viviendas en el sitio son similares, en promedio, en las tres macrozonas examinadas de Chile.
# Ha: Existe al menos una macrozona con una media de viviendas en el sitio mayor a las demás.


Cantidad <- c(norte$v8, centro$v8, sur$v8)
Macrozona <- c(rep("norte", length(norte$v8)), rep("centro", length(centro$v8)), rep("sur", length(sur$v8)))
Macrozona <- factor(Macrozona)
datos2 <- data.frame(Cantidad, Macrozona)



# Las condiciones para aplicar esta prueba son las siguientes y todas se cumplen, por 
# lo tanto si se puede aplicar esta prueba:
# 1. La variable independiente debe tener a lo menos dos niveles 
# 2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
# 3. Las observaciones son independientes entre sí.




# prueba de kruskal wallis para comparar las medias de las macrozonas de los 3 grupos estudiados
prueba <- kruskal.test(Cantidad~Macrozona, data = datos2)
print (prueba)



# Kruskal-Wallis rank sum test

# data:  Cantidad by Macrozona
# Kruskal-Wallis chi-squared = 14.583, df = 4, p-value = 0.005648




# Conclusión test de Kruskal Wallis: Para empezar, esta prueba se realizó ya que en el enunciado se pide comparar las medias 
# de más de 3 grupos independientes entre sí, también ocurre que al menos en este caso las muestras no presentaron una 
# distribución normal, por lo que fue necesario aplicar una prueba no paramétrica. Luego, al obtener un valor p = 0.005 mucho menor
# que el nivel de significancia alfa = 0.01, por ahora se rechazaría la hipótesis nula (H0) en favor de la hipótesis alternativa (Ha), es decir, 
# se puede decir con un 99% de confianza que existe al menos una macrozona con una media de viviendas en el sitio mayor a las demás.

# Aun así, todavía es necesario verificar esto mediante la técnica de Bootstrap, ya que al graficar los datos con histogramas,
# no se aprecia de la mejor manera la distribución de los datos en cada grupo.

hist(norte$v8)
hist(centro$v8)
hist(sur$v8)





# Comprobación de normalidad de los grupos a través de un gráfico QQ
g1 <- ggqqplot (datos2,
                x = "Cantidad",
                y = "Macrozona",
                color = "Macrozona")

g1 <- g1 + facet_wrap (~Macrozona)
g1 <- g1 + rremove ( "x.ticks" ) + rremove ("x.text")
g1 <- g1 + rremove ( "y.ticks" ) + rremove ("y.text")
g1 <- g1 + rremove ("axis.title")
print (g1)



# aplicación de Shapiro test para cada grupo (doble comprobación)
shapiro.test(norte$v8)
shapiro.test(centro$v8)
shapiro.test(sur$v8)



# Luego, se puede comprobar que en todos los grupos existe una distribución que NO se asemeja a la normal



# Aun asi, ya que los grupos son medianamente pequeños, se aplicará bootstrap para respaldar el test de Kruskal Wallis
# y no cometer errores al momento de responder la pregunta


B = 2000 # número de repeticiones
alfa <- 0.01


# se calcula la diferencia de medias entre todas las macrozonas
diferencia = mean(norte$v8) - mean(centro$v8) - mean(sur$v8)



# Como se pide comparar las medias de más de 2 grupos, lo que se nos ocurrió fue sacar bootstrap por cada combinación posible
# entre las macrozonas (norte-centro, norte-sur, centro-sur), y después comparar los resultados de cada una, resultando en lo siguiente:




# //// Caso Macrozonas Norte v/s Centro ////

bootstrap.NC <- two.boot(norte$v8, centro$v8, FUN = mean, R = B)

valores.NC <- data.frame (bootstrap.NC$t)
colnames (valores.NC) <- "valores.NC"

histograma.NC <- gghistogram (valores.NC, x = "valores.NC", color = "red",
                               fill = "red", bins = 100,
                               xlab = "Diferencia de medias Macrozonas Norte y Centro",
                               ylab = "Frecuencia", add = "mean")
print (histograma.NC)


qq <- ggqqplot (valores.NC, x = "valores.NC", color = "red")
print (qq)


# intervalo de confianza
intervalo.NC <- boot.ci(bootstrap.NC, conf = 1 - alfa, type = "bca")
print (intervalo.NC)




# Intervals : 
#  Level       BCa          
# 99%   (-0.5116,  0.1625 )  
# Calculations and Intervals on Original Scale
# Some BCa intervals may be unstable






# //// Caso Macrozonas Norte v/s Sur ////

bootstrap.NS <- two.boot(norte$v8, sur$v8, FUN = mean, R = B)


valores.NS <- data.frame (bootstrap.NS$t)
colnames (valores.NS) <- "valores.NS"

histograma.NS <- gghistogram (valores.NS, x = "valores.NS", color = "blue",
                              fill = "blue", bins = 100,
                              xlab = "Diferencia de medias Macrozonas Norte y Sur",
                              ylab = "Frecuencia", add = "mean")

print (histograma.NS)



qq2 <- ggqqplot (valores.NS, x = "valores.NS", color = "blue")
print (qq2)


# intervalo de confianza
intervalo.NS <- boot.ci(bootstrap.NS, conf = 1 - alfa, type = "bca")
print (intervalo.NS)



# Intervals : 
# Level       BCa          
# 99%   (-0.4937,  0.0839 )  
# Calculations and Intervals on Original Scale
# Some BCa intervals may be unstable




# //// Caso Macrozonas Centro v/s Sur

bootstrap.CS <- two.boot(centro$v8, sur$v8, FUN = mean, R = B)


valores.CS <- data.frame (bootstrap.CS$t)
colnames (valores.CS) <- "valores.CS"

histograma.CS <- gghistogram (valores.CS, x = "valores.CS", color = "orange",
                              fill = "orange", bins = 100,
                              xlab = "Diferencia de medias Macrozonas Centro y Sur",
                              ylab = "Frecuencia", add = "mean")
print (histograma.CS)



qq3 <- ggqqplot (valores.CS, x = "valores.CS", color = "orange")
print (qq3)


# intervalo de confianza
intervalo.CS <- boot.ci(bootstrap.CS, conf = 1 - alfa, type = "bca")
print (intervalo.CS)





# Intervals : 
#  Level       BCa          
# 99%   (-0.3345,  0.2685 )  
# Calculations and Intervals on Original Scale
# Some BCa intervals may be unstable






# Finalmente, se realiza el procedimiento post-hoc de Holm en caso de encontrar diferencias significativas
if(prueba $p.value < alfa) {
  post_hoc <- pairwise.wilcox.test(datos2$Cantidad,
                                    datos2$Macrozona,
                                    p.adjust.method = "holm",
                                    paired = FALSE )
 print (post_hoc)
}



# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

# data:  datos2$Cantidad and datos2$Macrozona 

#       centro   norte  
# norte 0.10496  -      
# sur   0.02148  0.00018

# P value adjustment method: holm





# Conclusión: A partir de todo el desarrollo anteriormente realizado, se puede apreciar que tanto los intervalos de confianza
# resultantes de cada bootstrap calculado son distintos, esto junto a los rangos de las medias que se dieron en cada caso.
# De hecho, en los histogramas hechos se puede ver que los rangos son distintos para todas las macrozonas. 

# Por el lado del procedimiento post-hoc, hay evidencias de que al comparar las macrozonas sur y norte se forma un valor p
# menor que el nivel de significancia (0.00018 < 0.01). Siendo así que, en base a los resultados tanto de la prueba de 
# Kruskal Wallis como el bootstrap y el procedimiento post-hoc, es que efectivamente se rechaza la hipótesis nula (H0)
# en favor de la alternativa (Ha), es decir, se asegura con un 99% de confianza que efectivamente existe al menos una 
# macrozona con una media de viviendas en el sitio mayor a las demás.














