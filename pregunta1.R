
  require (ggplot2)
  require (ez)
  require (ggpubr)
  require (readxl)

# ////////////////////////// PREGUNTA 1 //////////////////////////

#Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores
#(instructor, capit�n, comandante y general) califican a los recontroopers son similares,
#por lo que le ha solicitado estudiar si existen
#diferencias significativas en el promedio de la evaluaci�n realizada por cada uno de los oficiales.
#El Lord Sith ha sido muy
#claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten diferencias.


# Hip�tesis nula:
# H0: La exigencia de los distintos oficiales evaluadores es la misma
  
# Hip�tesis alternativa:
# Ha: La exigencia de al menos un oficial evaluador es distinta
  

  # /// An�lisis para ver si se cumplen las condiciones para aplicar ANOVA de una v�a ///
  # para muestras independientes
  
  # 1. La variable dependiente tiene escala de intervalos iguales
  # 2. Las muestras son independientes y obtenidas aleatoriamente
  # 3. Se puede asumir que las poblaciones son aproximadamente normales
  # 4. Las muestras tienen varianzas similares
  
  #la variable dependiente posee una escala de intervalos iguales por lo que esto se cumple
  
  #Se asume que las evaluaciones realizadas son aleatorias(tomando en cuenta la gran poblacion de clones y recluta en star wars)
  
  #para la condicion 3, se comprueba a traves de grafico Qq
  
 # g <- ggqqplot(
#    datos,
#    x = "eval_instructor",
#    color = "red"
#  )
  
#  g <- g + facet_wrap(~ eval_instructor)
#  print(g)
  
  
#se comprueba que son aproximadamente normales

  
#se comprueban las varianzas  
  
  
# se importan los datos
datos <- read.csv2(file.choose(), encoding = "UTF-8", sep = ";")


#se eligen los datos necesarios
datos <- datos %>% select(division, eval_instructor, eval_capitan, eval_comandante,eval_general)
#se filtran los datos para solo los recontrooper
datos2 <- datos %>% filter(division == "Recontrooper")

prueba <- aov (eval_instructor~division,
               data = datos)
cat ("Resultado de la prueba ANOVA")
print (summary (prueba))





# - Para la condición 3, esta se puede comprobar mediante un gráfico QQ

g <- ggqqplot(
  datos,
  x = "weight",
  color = "red"
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


