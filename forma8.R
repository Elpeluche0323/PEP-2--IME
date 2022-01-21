# Se importan librerías
library(tidyr)
library(dplyr)
library(leaps)
library(corrplot)
library (ggpubr)
require(car)
library (pROC)
library (caret)



# //// Enunciado ////

# Para esta actividad usaremos la misma muestra de medidas anatómicas seleccionada para el ejercicio práctico
# anterior desde los datos recolectados por Heinz et al. (2003). Como este ejercicio requiere de una variable
# dicotómica, vamos a realizar lo siguiente:

# 1. Crear la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida por el
# cuadrado de su estatura (en metros)

# 2. Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional (bajo peso, 
# normal, sobrepeso, obesidad, obesidad mórbida), para efectos de este ejercicio, usaremos dos clases:
# sobrepeso (IMC �???� 25,0) y no sobrepeso (IMC < 25,0)

# 3. Crear la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.


# Ahora podemos construir un modelo de regresión logística para predecir la variable EN, de acuerdo con las
# siguientes instrucciones:

# 1. Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en el ejercicio anterior.

# 2. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase EN,
# justificando bien esta selección.

# 3. Usando el entorno R, construir un modelo de regresión logística con el predictor seleccionado en el paso anterior.

# 4. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de
# entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo obtenido en el paso 3.

# 5. Evaluar los modelos y �???oarreglarlos�???� en caso de que tengan algún problema con las condiciones que deben cumplir.

# 6. Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando validación cruzada) 
# y revisar las respectivas curvas ROC.





# Realizando llamado al archivo.csv
datos <- read.csv(file.choose(), encoding = "UTF-8", sep = ";")


# Se seleccionan las variables peso y estatura de la tabla, guardándose
peso <- datos %>% select(Weight)
estatura <- datos %>% select(Height)

# Se pasan los datos de estatura a m 
estatura <- estatura / 100

# Se calcula el IMC
IMC <- peso / (estatura * estatura)


# Se crea una variable dicotómica respecto al Estado Nutricional según el IMC de la muestra
EN <- data.frame(EN = cut(IMC[["Weight"]], breaks = c(0, 25, Inf),
                          labels = c("no sobrepeso", "sobrepeso"),
                          right = FALSE))


# Se agrega la variable a la tabla de datos principal
datos$EN <- EN[["EN"]]



# //// Instrucciones para creación modelo de regresión logística ////


# /// Paso 1 ///
# Las variables aleaorias seleccionadas en el ejercicio anterior son las siguienes:

# 1.- Forearm.Girth
# 2.- Elbows.diameter
# 3.- Navel.Girth
# 4.- Ankles.diameter
# 5.- Thigh.Girth
# 6.- Ankle.Minimum.Girth
# 7.- Chest.diameter
# 8.- Knee.Girth



# /// Paso 2 ///

# La variable que consideramos como útil para predecir el EN corresponde a Waist.Girth,
# es decir, el grosor a la altura de la cintura (cm). Esto debido a que usualmente en las personas
# que tienen sobrepeso, su cuerpo tiende a almacenar grasa en la zona del abdomen, sobre todo
# si cuentan con alguna enfermedad crónica tal como la resistencia a la insulina, diabetes, etc.

# También, por otro lado, se puede mencionar que cuando la gente baja de peso, una manera de 
# comprobar si efectivamente está perdiendo grasa en el cuerpo es a través de medirse ya sea la
# cintura, piernas, brazos, etc., además de guiarse por la talla de la ropa, que termina contando 
# como una medida en la cintura para los pantalones, shorts, faldas, entre otros (para la variable que se eligió).





# /// Paso 3 ///

# 3. Usando el entorno R, construir un modelo de regresión logística con el predictor seleccionado en el paso anterior.


n <- nrow(datos)
n_entrenamiento <- floor (0.85 * n)
muestra <- sample.int(n = n, size = n_entrenamiento , replace = FALSE)
entrenamiento <- datos [muestra,]
prueba <- datos [-muestra,]



modelo <- glm (EN ~ Waist.Girth, family = binomial(link = "logit"), data = entrenamiento)
print (summary(modelo))


# Call:
#  glm(formula = EN ~ Waist.Girth, family = binomial(link = "logit"), 
#      data = entrenamiento)

# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.5083  -0.5046  -0.2202   0.3201   3.0760  

# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -18.69773    1.80399  -10.37   <2e-16 ***
#  Waist.Girth   0.22290    0.02204   10.11   <2e-16 ***
#  ---
#  Signif. codes:  0 �???~***�???T 0.001 �???~**�???T 0.01 �???~*�???T 0.05 �???~.�???T 0.1 �???~ �???T 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 525.34  on 429  degrees of freedom
# Residual deviance: 291.25  on 428  degrees of freedom
# AIC: 295.25

# Number of Fisher Scoring iterations: 6



probs_e <- predict (modelo, entrenamiento, type = "response")
umbral <- 0.5
preds_e <- sapply (probs_e, function (p) ifelse (p >= umbral , "1", "0"))
preds_e <- factor (preds_e, levels = levels (datos [["EN"]]))

ROC_e <- roc( entrenamiento [["EN"]], probs_e)
plot (ROC_e)


# Se crea la matriz de confusión

matriz_e <- confusionMatrix (preds_e, entrenamiento [["EN"]])
print (matriz_e)



# Evaluar el modelo con el conjunto de prueba 

probs_p <- predict (modelo, prueba, type = "response")

preds_p <- sapply (probs_p, function (p) ifelse (p >= umbral , "no sobrepeso", "sobrepeso"))
preds_p <- factor (preds_p, levels = levels (datos [["EN"]]))

ROC_p <- roc(entrenamiento [["EN"]], probs_e)
plot (ROC_p)

matriz_p <- confusionMatrix (preds_p, prueba [["EN"]])
print (matriz_p)


#paso 4
# Se buscan los posibles predictores que se puedan incluir en el modelo (de los seleccionados en la parte 3)
# pudiendo ser entre 2 a 5
predictores <- regsubsets(Weight~., data = datos, nvmax = 5)
print(summary(predictores))

# A partir de la funci�n regsubsets, se deciden agregar los siguientes predictores al modelo 
# - Thigh.Girth 
# - Knee.Girth


# Quedando de la siguiente manera:
nuevo_modelo <- lm(formula = Weight ~ Height + Thigh.Girth + Knee.Girth, data = datos)
print(summary(nuevo_modelo)) 

#Call:
#  lm(formula = Weight ~ Height + Thigh.Girth + Knee.Girth, data = datos)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-16.1098  -3.9657  -0.5822   3.4476  25.0386 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -163.85743    5.76379 -28.429   <2e-16 ***
#  Height         0.70185    0.03578  19.617   <2e-16 ***
#  Thigh.Girth    0.80394    0.08341   9.639   <2e-16 ***
#  Knee.Girth     1.85562    0.16593  11.183   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 6.068 on 503 degrees of freedom
#Multiple R-squared:  0.7945,	Adjusted R-squared:  0.7932 
#F-statistic: 648.1 on 3 and 503 DF,  p-value: < 2.2e-16




# //// Paso 5 //// Los valores de variable de respuesta son independientes.
# se obtiene p = 0.976, se concluye que los residuos son independientes.

prueba_dur <- durbinWatsonTest(modelo)
print(prueba_dur)



# //// Paso 6 ////
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
#no son muy diferentes entre si, se puede decir que el poder predictivo que tiene este metodo es lo suficientemente bueno como para utilizarlo