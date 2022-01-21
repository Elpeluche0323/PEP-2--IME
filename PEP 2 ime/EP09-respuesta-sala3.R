# Nombres:
# Estefanía Alvarez 20.371.287-1
# Stephan Silva 20.298.778-8
# Francisco Moreno 19.892.183-1
# Maximiliano Araya 20.467.583-k
# Daniel Jara 20.113.716-0

# se importan librerias
library ( ggpubr )
library ( ez )
library ( tidyr )
library (emmeans)

#       ____                                          _               _ 
#     |  _ \   _ __    ___    __ _   _   _   _ __   | |_    __ _    / |
#    | |_) | | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |   | |
#   |  __/  | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |   | |
#  |_|     |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_|
#                         |___/                                      

# El siguiente código R
# carga los datos que aparecen en una tabla que compara las mejores soluciones
# encontradas por cuatro algoritmos para instancias del problema del vendedor viajero con solución óptima
# conocida, tomados desde una memoria de título del DIINF. Con estos datos responda la pregunta de
# investigación: ¿Hay algoritmos mejores que otros?

# Se importan los datos
texto <- (
"Instancia      Optimo  R       R2      R3      G
'brock400_2'    36      46.4    45.7    44.8    43
'brock400_4'    40      56.2    53.7    52.6    51
'C2000.9'       87      109.8   107.4   104.6   101
'c-fat500 10'   133     134     134     134     133
'hamming10-2'   519     687.8   611.9   608.6   519
'johnson32-2-4' 23      23      23      23      23
'keller6'       66      90.5    82      79.5    76.8
'MANN_a81'      1107    1124.8  1124.8  1124.8  1111.1
'p-hat1500-1'   19      24.1    22.9    22.1    21
'p-hat 1500 3'  101     119.2   117.3   110.4   109
'san1000'       22      29.4    29.4    29.3    27
'san400_0.7_1'  47      67.4    66.5    66.5    66
'san400_0.9_1'  107     162.9   152.6   150.6   115
'frb100-40'     107     140.6   130.3   126.5   125
'frb59-26-1'    66      85.8    79.1    76.7    77
'1et.2048'      323     406.6   370.6   358.1   346.6
'1zc.4096'      386     491.2   471.8   457.6   436.5
'2dc.2048'      31      39.4    36.3    35.1    34
")

datos <- read.table(textConnection(texto), header = TRUE)



# Se formulan las hipótesis:
# H0: no existen diferencias significativas entre los algoritmos
# Ha: existe al menos 1 algoritmo que presente una diferencia significativa

# DATOS EN FORMATO ANCHO
datos <- read.table(
  file= textConnection(texto), 
  header = TRUE,
  stringsAsFactors = FALSE
)


names(datos)<- c("Instancia", "Optimo", "R", "R2", "R3", "G")


# DATOS EN FORMATO LARGO
dl <- gather(
  data = datos,
  key = "Algoritmo",
  value = "Tiempo",
  -"Instancia"
)


dl[["Instancia"]] <- factor(dl[["Instancia"]])
dl[["Algoritmo"]] <- factor(dl[["Algoritmo"]])




# Comprobación de normalidad a través de un gráfico QQ
g1 <- ggqqplot ( dl,
                x = "Tiempo" ,
                y = "Algoritmo" ,
                color = "Algoritmo")

g1 <- g1 + facet_wrap (~ Algoritmo )
g1 <- g1 + rremove ( "x.ticks" ) + rremove ("x.text")
g1 <- g1 + rremove ( "y.ticks" ) + rremove ("y.text")
g1 <- g1 + rremove ( "axis.title" )
print ( g1 ) 



# Por el tipo de ejercicio donde nos piden identificar si existen algoritmos mejores que otros,
# es que se puede determinar que para resolver esta duda es necesario aplicar una prueba 
# ANOVA de una vía para muestras correlacionadas.


# Es por esto que se verificarán si se cumplen las condiciones para realizar este tipo de prueba:
# 1.- Se puede apreciar mediante el gráfico QQ que la variable dependiente presenta intervalos 
# iguales para cada muestra 
# 2.- Se puede decir que las muestras son independientes, ya que se encontraron en una memoria de titulo del DIINF
# 3.- A partir del gráfico QQ se puede ver que en las muestras, si bien existen algunos valores atípicos, estos son
# los mínimos en cada caso, siendo por esto que se puede concluir que las muestras siguen una distribución aproximadamente normal.
# 4.- Esta será comprobada mediante la prueba ezANOVA(), quedando en espera de comprobarse mientras tanto




# Ahora, se realizará la prueba ANOVA

cat ("Procedimiento ANOVA usando ezANOVA")
prueba <- ezANOVA(data = dl, dv = Tiempo , within = Algoritmo,
                    wid = Instancia, return_aov = TRUE)

# Error: Instancia
# Df  Sum Sq Mean Sq F value Pr(>F)
# Residuals 17 6647857  391050               

# Error: Instancia:Algoritmo
#            Df Sum SqMean Sq F value Pr(>F)    
# Algoritmo  4  12535  3133.7   9.134 5.76e-06  ***
# Residuals 68  23330  343.1  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1




print ( summary ( prueba $aov))
cat ("El resultado de la prueba de esfericidad de Mauchly :\n")
print (prueba[["Mauchly's Test for Sphericity"]])

# NULL



cat ("Y factores de corrección para cuando no se cumple la condición de esfericidad :")
print (prueba$'Sphericity Corrections')

#Effect       GGe         p[GG]          p[GG]<.05   HFe          p[HF]         p[HF]<.05
#2 Algoritmo  0.2894549    0.005205344   *           0.2973195   0.004817602    *




# Gráfico del tamaño del efecto
g2 <- ezPlot ( data = dl, dv = Tiempo, wid = Instancia, within = Algoritmo,
                  y_lab = "Tiempo promedio de ejecución [ms]", x = Algoritmo)
print (g2)



# Se define el nivel de significación
alfa <- 0.05


# Luego, se realiza el procedimiento post-hoc HSD de Tukey
mixto <- lme(Tiempo~Algoritmo, data = dl, random = ~1|Instancia)
medias <- emmeans(mixto, "Algoritmo")
tukey <- pairs(medias, adjust = "tukey")

cat ("\n\n Prueba HSD de Tukey \n\n")
print (tukey)


# contrast    estimate   SE df t.ratio  p.value
# G - Optimo     10.83 6.17 68   1.755  0.4083
# G - R         -23.56 6.17 68  -3.816  0.0027
# G - R2        -13.57 6.17 68  -2.198  0.1929
# G - R3        -10.54 6.17 68  -1.708  0.4361
# Optimo - R    -34.39 6.17 68  -5.571  <.0001
# Optimo - R2   -24.41 6.17 68  -3.953  0.0017
# Optimo - R3   -21.38 6.17 68  -3.462  0.0080
# R - R2          9.99 6.17 68   1.618  0.4914
# R - R3         13.02 6.17 68   2.108  0.2286
# R2 - R3         3.03 6.17 68   0.490  0.9880

# Degrees-of-freedom method: containment 
# P value adjustment: tukey method for comparing a family of 5 estimates



# Conclusión: Como se puede apreciar en los resultados del procedimiento post-hoc HSD de Tukey,
# en los casos optimo-R, optimo-R2, optimo-R3, y G-R se presentan valores bastante menores que 
# el nivel de significación (alfa = 0.05), siendo por eso que se rechazaría la hipótesis nula (H0)
# en favor de la hipótesis alternativa (Ha), es decir, existe al menos 1 algoritmo que sea
# mejor que los otros.

# Sin embargo, cuando se realizó el test de esfericidad de Mauchly, éste no arrojó resultados (NULL),
# sino que arrojó factores de corrección para cuando no se cumple la esfericidad. Esto significa que
# los resultados obtenidos puede que no estén 100% correctos, pues la 4ta condición del test ANOVA no 
# se cumplió. Siendo por esto que se recomienda ajustar la muestra y realizar un nuevo estudio.









#      ____                                          _               ____  
#    |  _ \   _ __    ___    __ _   _   _   _ __   | |_    __ _    |___ \ 
#   | |_) | | '__|  / _ \  / _` | | | | | | '_ \  | __|  / _` |     __) |
#  |  __/  | |    |  __/ | (_| | | |_| | | | | | | |_  | (_| |    / __/ 
# |_|     |_|     \___|  \__, |  \__,_| |_| |_|  \__|  \__,_|   |_____|
#                        |___/                                         


#El siguiente es (un resumen de) la descripción de un famoso experimento:

# Naming the ink color of color words can be difficult. For example, if asked to name the color of
# the word "blue" is difficult because the answer (red) conflicts with the word "blue." This
# interference is called "Stroop Interference" after the researcher who first discovered the
# phenomenon. This case study is a classroom demonstration. Students in an introductory
# statistics class were each given three tasks. In the "words" task, students read the names of 60
# color words written in black ink; in the "color" task, students named the colors of 60 rectangles;
# in the "interference" task, students named the ink color of 60 conflicting color words. The times
# to read the stimuli were recorded.

#El siguiente código R carga los datos que se obtuvieron en este estudio. Con estos datos, responda la
# siguiente pregunta de investigación: ¿hay diferencias en los tiempos entre tareas?

 
texto2 <- ("
words colors interfer
16    15     29
26    24     33
18    19     44
21    20     38 
9     25     38
17    23     34
21    19     44
15    19     38
13    22     47
19    15     31
21    17     35
23    17     37
17    21     31
21    19     32
9     14     42
16    16     36
")

datos2 <- read.table(textConnection(texto2), header = TRUE)


# Se formulan las hipótesis:
# H0: no existen diferencias significativas en los tiempos entre tareas
# Ha: existe al menos 1 tarea que presente una diferencia significativa en tiempo


# Se comienza por definir un nuevo data.frame con los datos acomodados 

a <- datos2$words
b <- datos2$colors
c <- datos2$interfer

Cantidad <- c(a, b, c)

Tarea <- c(rep("words", length(a)),
           rep("colors" , length(b)),
           rep("interfer", length(c)))

Tarea <- factor(Tarea)
Instancia2 <- factor(seq(1, 48, by = 1))
datos3 <- data.frame(Instancia2, Tarea, Cantidad)


# se define el nivel de significación 
alfa = 0.05


# Comprobación de normalidad a través de un gráfico QQ
g1 <- ggqqplot (datos3,
                 x = "Cantidad",
                 y = "Tarea",
                 color = "Tarea")

g1 <- g1 + facet_wrap (~Tarea)
g1 <- g1 + rremove ( "x.ticks" ) + rremove ("x.text")
g1 <- g1 + rremove ( "y.ticks" ) + rremove ("y.text")
g1 <- g1 + rremove ("axis.title")
print (g1)




# Por el tipo de ejercicio donde nos piden identificar si existen diferencias en los tiempos entre tareas,
# es que se puede determinar que para resolver esta duda es necesario aplicar una prueba 
# ANOVA de una vía para muestras correlacionadas.


# Es por esto que se verificarán si se cumplen las condiciones para realizar este tipo de prueba:
# 1.- Se puede apreciar mediante el gráfico QQ que la variable dependiente presenta intervalos 
# iguales para cada muestra 
# 2.- Se puede decir que las muestras son independientes, ya que las observaciones se obtuvieron de una clase de estadística introductoria
# 3.- A partir del gráfico QQ se puede ver que en las muestras no existen valores atípicos, 
# siendo por esto que se puede concluir que las muestras siguen una distribución normal.
# 4.- Esta no puede ser comprobada ya que en esta ocasión se utilizará la función aov de R



# Ahora, se realizará la prueba ANOVA con aov
prueba1 <- aov (Cantidad~Tarea,
                data = datos3)
cat ( " \ nResultado de la prueba ANOVA para muestras correlacionadas con aov \ n " )
print (summary (prueba1))



# Procedimiento post-hoc de holm 
holm <- pairwise.t.test( Cantidad,
                          Tarea,
                          p.adj = "holm" ,
                          paired = TRUE )

cat(" Corrección de holm \n ")
print(holm)


# Pairwise comparisons using paired t tests 

# data:  Cantidad and Tarea
#           colors    interfer
# interfer  1.2e-08   -       
# words     0.33      1.9e-07 
# P value adjustment method: holm 


# Conclusión: Como se puede ver en los resultados del procedimiento post-hoc de holm, los resultados entre
# interfer-colors y words-interfer  son bastante menores al nivel de significancia estipulado (alfa = 0.05),
# esto significando que existe un rechazo a la hipótesis nula (H0) en favor a la hipótesis alternativa (Ha),
# es decir, existe al menos 1 tarea que presente una diferencia significativa en tiempo.





