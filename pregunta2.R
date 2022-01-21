library(dplyr)
library(tidyverse)
library(ggpubr)
library(leaps)
library(corrplot)
require(car)
library (pROC)
library (caret)

# Pregunta N°2

# Se establece una semilla
set.seed(3349)

# Se lee el archivo de entrada.
datos <- read.csv(file.choose(), encoding = "UTF-8", sep = ";")
# Se establece un tamaño de la muestra igual a 400
n <- 400
# Se extraen los n datos para la muestra
muestra <- sample_n(datos, n)

# Solo se capturan las variables de interes, las cuales son:
# es_clon. -> N = no ; S = si.
# velocidad (porcentaje respecto al optimo)
# fuerza (porcentaje respecto al optimo)
# resistencia (porcentaje respecto al optimo)
# agilidad (porcentaje respecto al optimo)
datosInteres <- muestra %>% select(es_clon,velocidad,fuerza,resistencia,agilidad)


# Valor de alfa = 0.01 para usar la prueba de Durbin-Watson para un retardo.
alfa <- 0.01

# Hipotesis:
# H0:
# H1:


