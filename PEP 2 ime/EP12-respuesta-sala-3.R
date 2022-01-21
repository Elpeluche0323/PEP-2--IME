# Nombres:
# Estefanía Alvarez 20.371.287-1
# Stephan Silva 20.298.778-8
# Francisco Moreno 19.892.183-1
# Maximiliano Araya 20.467.583-k
# Daniel Jara 20.113.716-0


# Se importan librerías 
library(tidyr)
library(dplyr)
library(leaps)
library(corrplot)
library (ggpubr)
require(car)



# //// Enunciado ////

# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres (Heinz et al., 2003). Estas
# mediciones están disponibles en el archivo Body EP1 2 csv que acompaña a este enunciado. El estudio
# incluyó nueve mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y doce
# mediciones de grosor (circunferencias) que incluyen el tejido.

# Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso, de acuerdo con las
# siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito verificador) 
# del integrante de menor edad del equipo

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la semilla es impar)

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la
# variable Peso, justificando bien esta selección

# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado
# en el paso anterior

# 6. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de
# regresión líneal simple obtenido en el paso 5

# 7. Evaluar los modelos y "arreglarlos" en caso de que tengan algún problema con las condiciones que
# deben cumplir

# 8. Evaluar el poder predictivo del modelo en datos no utilizados para construirlo (o utilizando
# validación cruzada)



# Realizando llamado al archivo .csv

datos <- read.csv(file.choose(), encoding = "UTF-8", sep = ";")


# //// Pasos 1 y 2 ////
# Se define la semilla según los úlimos 4 dígitos del run de Estefanía Alvarez
# y al ser un número impar, se selecciona una muestra aleatoria de 50 hombres
set.seed(1287)
datos <- sample_n(datos, 50)
datos <- datos %>% filter(datos[["Gender"]] == 1)


# //// Paso 3 ////
# Las variables aleaorias seleccionadas son las siguienes:
# 1.- Forearm.Girth
# 2.- Elbows.diameter
# 3.- Navel.Girth
# 4.- Ankles.diameter
# 5.- Thigh.Girth
# 6.- Ankle.Minimum.Girth
# 7.- Chest.diameter
# 8.- Knee.Girth



# //// Paso 4 ////
# Como grupo creemos que la variable "Height" puede ser de utilidad para predecir la variable Peso (Weight),
# ya que la estatura tiene una gran importancia para el peso de una persona, esto es mediante el IMC
# (Indice de Masa Corporal), cuya ecuación es la siguiente:

#   Peso [Kg] / Estatura [m^2]

# La importancia de este valor radica en que es el indicador más objetivo usado para 
# evaluar el tamaño del cuerpo, puesto que es una razón matemática que asocia la masa 
# y la talla de un individuo.




# //// Paso 5 ////

# se construye el modelo de regresión lineal simple con el predictor seleccionado Height
modelo <- lm(formula = Weight ~ Height, data = datos)
print(summary(modelo))



# Call:
#  lm(formula = Weight ~ Height, data = datos)

# Residuals:
#      Min        1Q   Median       3Q      Max 
# -18.5539  -4.4317   -0.7552   6.6499  11.2605 

# Coefficients:
#             Estimate  Std. Error  t value  Pr(>|t|)   
# (Intercept) -61.2627    42.7387  -1.433  0.16646   
# Height        0.7942     0.2392   3.321  0.00325 **

#  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 7.883 on 21 degrees of freedom
# Multiple R-squared:  0.3443,	Adjusted R-squared:  0.3131 
# F-statistic: 11.03 on 1 and 21 DF,  p-value: 0.00325



# Se grafica el modelo resultante
p <- ggscatter(datos, x = "Weight", y = "Height", color = "blue", fill = "blue",
               xlab = "Peso [Kg]", ylab = "Altura [cm]")

p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")
print (p)




# //// Paso 6 ////

# Se buscan los posibles predictores que se puedan incluir en el modelo (de los seleccionados en la parte 3)
# pudiendo ser entre 2 a 5
predictores <- regsubsets(Weight~., data = datos, nvmax = 5)
print(summary(predictores))

# A partir de la función regsubsets, se deciden agregar los siguientes predictores al modelo 
# - Thigh.Girth 
# - Knee.Girth


# Quedando de la siguiente manera:
nuevo_modelo <- lm(formula = Weight ~ Height + Thigh.Girth + Knee.Girth, data = datos)
print(summary(nuevo_modelo)) 


# Call:
#  lm(formula = Weight ~ Height + Thigh.Girth + Knee.Girth, data = datos)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -6.4845 -2.7504  0.1265  2.4624  7.5613 

# Coefficients:
#               Estimate  Std. Error  t value Pr(>|t|)    
#  (Intercept) -111.2521    25.7938  -4.313 0.000375 ***
#  Height         0.4464     0.1413   3.160 0.005155 ** 
#  Thigh.Girth    1.6024     0.2736   5.856 1.22e-05 ***
#  Knee.Girth     0.5527     0.5792   0.954 0.351985  

# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 4.299 on 19 degrees of freedom
# Multiple R-squared:  0.8236,	Adjusted R-squared:  0.7957 
# F-statistic: 29.57 on 3 and 19 DF,  p-value: 2.304e-07








# //// Paso 7 //// Los valores de variable de respuesta son independientes.
# se obtiene p = 0.748, se concluye que los residuos son independientes.

prueba_dur <- durbinWatsonTest(modelo)
print(prueba_dur)



# //// Paso 8 ////
set.seed(1287)
# Crear conjuntos de entrenamiento y prueba .
n <- nrow(datos)
n_entrenamiento <- floor(0.7 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

#variables[, !(colnames(variables) %in% predictoras), drop = FALSE]

# Ajustar modelo con el conjunto de entrenamiento.
modelo <- lm(Weight ~ Waist.Girth, data = entrenamiento)
print(summary(modelo))

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
mse_entrenamiento <- mean (modelo$residuals ** 2)
cat("MSE para el conjunto de entrenamiento: ", mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones <- predict(modelo, prueba)
# Calcular error cuadrado promedio para el conjunto de prueba.
error <- sapply(prueba[["Weight"]],as.double) - predicciones
mse_prueba <- mean(error ** 2)
cat("MSE para el conjunto de prueba: ", mse_prueba)

#Debido a que la diferencia entre los errores cuadrados promedios entre el conjunto de entrenamiento y el conjunto de prueba 
#no son muy diferentes entre si, se puede decir que el poder predictivo que tiene este metodo es lo suficiente mente bueno como para utilizarlo
 




