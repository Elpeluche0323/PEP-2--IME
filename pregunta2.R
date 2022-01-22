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
datosInteres <- muestra %>% select(es_clon,velocidad,fuerza,resistencia)



# Hipotesis:
# Hipotesis nula H0: Es posible distinguir entre ambas clases el entrenamiento para clones y reclutas,
# Hipotesis alternativa H1: No posible distinguir entre ambas clases el entrenamiento para clones y reclutas.

# Valor de alfa = 0.01 para usar la prueba de Durbin-Watson para un retardo.
alfa <- 0.01

# Gráfico de cajas para visualizar la diferencias de velocidad.
ggplot(data = datosInteres, aes(x = es_clon, y = velocidad, colour = es_clon)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.50) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

# para visualizar la diferencias de fuerza.
ggplot(data = datosInteres, aes(x = es_clon, y = fuerza, colour = es_clon)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.50) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

# para visualizar la diferencias de resistencia.
ggplot(data = datosInteres, aes(x = es_clon, y = resistencia, colour = es_clon)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.50) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")


# Gráfico de densidad.
ggplot(data = datosInteres, aes(x = velocidad, y = fuerza, z = resistencia, fill = es_clon)) +
  geom_density(alfa) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  theme_bw() +
  theme(legend.position = "bottom")


# Se analizan los datos.
datosInteres %>% group_by(es_clon) %>% summarise(media = mean(velocidad),
                                       sd = sd(velocidad),
                                       mediana = median(velocidad))

datosInteres %>% group_by(es_clon) %>% summarise(media = mean(fuerza),
                                                 sd = sd(fuerza),
                                                 mediana = median(fuerza))

datosInteres %>% group_by(es_clon) %>% summarise(media = mean(resistencia),
                                                 sd = sd(resistencia),
                                                 mediana = median(resistencia))

#########################################################################################
# Conclusion:
