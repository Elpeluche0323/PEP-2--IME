
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
  
  #1.la variable dependiente posee una escala de intervalos iguales por lo que esto se cumple
  
  #2.Se asume que las evaluaciones realizadas son aleatorias(tomando en cuenta la gran poblacion de clones y recluta en star wars)
  
  
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

#3.para la condicion 3, se comprueba a traves de grafico Qq
 g <- ggqqplot(
   datos3,
    x = "eval",
    color = "red"
  )
#4.la condicion 3 se cumple si bien existen unos pocos puntos pero es poca desviacion
print(g)

#viendo el grafico anterior tambien se logra ver que se cumple la condicion 4

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


#data:  datos3[["puntaje"]] and datos3[["eval"]] 

#               eval_capitan eval_comandante eval_general
#eval_comandante 0.80         -               -           
#  eval_general    <2e-16       <2e-16          -           
#  eval_instructor 0.58         0.80            <2e-16     

#no se porque solo aparecen 3 y no 4


#Conclusion

#Como el valor de p menor a la significancia utilizada,podemos rechazar la hipotesis nula
#a favor de la hipotesis alternativa,encontrando diferencias entre las evaluaciones promedio
#para al menos 1 evaluador, por otra parte se revisan los valores obtenidos en el procedimiento post-hoc.

#En lo que respecta a cual evaluador fue el que tuvo grandes diferencias,este fue el evaluador general


