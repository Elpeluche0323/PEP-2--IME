
  require (ggplot2)
  require (ez)
  require (ggpubr)
  require (tidyr)
  require (dplyr)

# ////////////////////////// PREGUNTA 1 //////////////////////////

#Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores
#(instructor, capitán, comandante y general) califican a los recontroopers son similares,
#por lo que le ha solicitado estudiar si existen
#diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales.
#El Lord Sith ha sido muy
#claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten diferencias.


# Hipótesis nula:
# H0: La exigencia de los distintos oficiales evaluadores es la misma
  
# Hipótesis alternativa:
# Ha: La exigencia de al menos un oficial evaluador es distinta
  

  # /// Análisis para ver si se cumplen las condiciones para aplicar ANOVA de una vía ///
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
datos <- datos %>% select(id,division, eval_instructor, eval_capitan, eval_comandante,eval_general)
#se filtran los datos para solo los recontrooper
datos2 <- datos %>% filter(division == "Recontrooper")

datos3 <- datos2 %>% pivot_longer (c("eval_instructor", "eval_capitan", "eval_comandante","eval_general") ,
                                  names_to = "eval",
                                  values_to = "puntaje")
print(datos3)

#se realiza la prueba anova
  prueba2 <- ezANOVA (
  data = datos3 ,
  dv = puntaje ,
  between = eval ,
  wid = id ,
  return_aov = TRUE )

print(prueba2)


# Gráfico del tamaño del efecto
g2 <- ezPlot (
  data = datos3,
  dv = puntaje,
  wid = id,
  between = eval,
  y_lab = "puntaje promedio de los evaluadores",
  x = eval
)

print(g2)

#prueba post-hoc
alfa <- 0.01

holm<-pairwise.t.test ( datos3[["puntaje"]],
                        datos3[["eval"]],
                        p.adj = "holm",
                        pool.sd = TRUE,
                        paired = FALSE,
                        conf.level = 1 - alfa )
print(holm)




