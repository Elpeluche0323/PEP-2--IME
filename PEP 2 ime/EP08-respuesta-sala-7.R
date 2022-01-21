# Nombres:
# Estefanía Alvarez 20.371.287-1
# Stephan Silva 20.298.778-8
# Francisco Moreno 19.892.183-1
# Maximiliano Araya 20.467.583-k
# Daniel Jara 20.113.716-0


if (!require(ggplot2)){
  install.packages("ggplot2", dependencies = TRUE )
  require (ggplot2)
}
if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}


# ////////////////////////// PREGUNTA 1 //////////////////////////

# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores no veganos. En el paquete
# datasets de R (importado nativamente) está el conjunto de datos chickwts con los resultados de un
# experimento hecho (supuestamente en 1948) para medir la efectividad de varios suplementos alimenticios
# en la tasa de crecimiento de las aves, en donde pollitos recién nacidos se separaron aleatoriamente en
# seis grupos, y a cada grupo se le dio un suplemento distinto por seis semanas. Se reportan los pesos, en
# gramos, alcanzados por los pollitos. Para productores de la 7º región, es especialmente importante saber
# si deberían usar suplementos basados en linaza (linseed), leche (casein), habas (horsebean) o girasol
# (sunflower).

# se importan los datos
datos <- chickwts


# se agrega columna con las instancias realizadas
datos[["instancia"]] <- factor(1:nrow(datos))

#suplemento <- factor(datos[[]])
#instancia <- factor(seq( 1, 71, by = 1))
#datos2 <- data.frame (instancia, feed, weight)

alfa <- 0.01


# Hipótesis nula:
# H0: La efectividad promedio de los suplementos es igual para todas las muestras

# Hipótesis alternativa:
# Ha: La efectividad promedio es diferente para al menos un suplemento


# /// Análisis para ver si se cumplen las condiciones para aplicar ANOVA de una vía ///
# para muestras indeopendientes

# 1. La variable dependiente tiene escala de intervalos iguales
# 2. Las muestras son independientes y obtenidas aleatoriamente
# 3. Se puede asumir que las poblaciones son aproximadamente normales
# 4. Las muestras tienen varianzas similares


# - Se puede afirmar que efectivamente la variable dependiente posee una escala de 
# intervalos iguales, por lo que la condici?n 1 si se cumple

# - Para la condición 2 el mismo enunciado se?ala que los pollitos estudiados fueron 
# seleccionados de manera aleatoria

# - Para la condición 3, esta se puede comprobar mediante un gráfico QQ

g <- ggqqplot(
  datos,
  x = "weight",
  color = "feed"
)

g <- g + facet_wrap(~ feed)
print(g)


# - Luego, se puede apreciar que, si bien hay algunos puntos un poco distantes, no hay desviaciones 
# importantes en los datos, por lo tanto, se cumplir?an todas las condiciones para realizar ANOVA



# Procedimiento ANOVA con aov
prueba <- aov (weight~feed,
               data = datos)
cat ("Resultado de la prueba ANOVA")
print (summary (prueba))


# Gráfico del tamaño del efecto
g2 <- ezPlot (
  data = datos,
  dv = weight,
  wid = instancia,
  between = feed,
  y_lab = "Peso promedio de los pollitos [g]",
  x = feed
  )

print (g2)

#Warning: muestra un warning en consola, esto es debido a que la cantidad de datos por grupo es diferente, 
#de todos modos no afecta el gráfico.


#Gráfico de cajas que compara los pesos de los pollitos por tipo de suplemento
g3 <- boxplot(weight ~ feed,
             data = datos,
             border = "red",
             col = "pink",
             ylab = "Pesos de los Pollitos [g]",
             xlab = "Suplemento")

print (g3)


cat ("\n\ nProcedimiento post - hoc de Holm \n\n")

holm<-pairwise.t.test ( datos[["weight"]],
                            datos[["feed"]],
                            p.adj = "holm",
                            pool.sd = TRUE,
                            paired = FALSE,
                            conf.level = 1 - alfa )
print(holm)


#data:  datos[["weight"]] and datos[["feed"]] 

#            casein  horsebean linseed meatmeal soybean
#  horsebean 2.9e-08 -         -       -        -      
#  linseed   0.00016 0.09435   -       -        -      
#  meatmeal  0.18227 9.0e-05   0.09435 -        -      
#  soybean   0.00532 0.00298   0.51766 0.51766  -      
#  sunflower 0.81249 1.2e-08   8.1e-05 0.13218  0.00298

#P value adjustment method: holm 

# Conclusión: Con valores p menores a la significancia utilizada, se puede rechazar la hipótesis nula a favor
# de la hipótesis alternativa, encontrando diferencia en la efectividad promedio para al menos un suplemento,
# esto se puede evidenciar al observar el gráfico de cajas, donde las medias son notoriamente diferentes para
# cada suplemento, por otra parte se revisa en los valores p obtenidos en el procedimiento post - hoc.
# Se puede concluir entonces con un 90% de confianza que la efectividad promedio es diferente para al menos un suplemento.


