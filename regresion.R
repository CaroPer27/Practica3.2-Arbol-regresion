# Leer el conjunto de datos
data <- read.csv("C:\\Users\\carod\\OneDrive\\Documentos\\Carolina\\R_Mineria\\weather_regresion.csv")

targetdev <- function(target) {
  sd_value <- sd(target)
  cv <- (sd_value / mean(target)) * 100
  return(list(sd = sd_value, cv = cv))
}

atributos <- function(data, labels) {
  resultados <- list()
  for (label in labels) {
    media_atributo <- mean(data[[label]])
    desviacion_atributo <- sd(data[[label]])
    
    resultados[[label]] <- list(media = media_atributo, sd = desviacion_atributo)
  }
  return(resultados)
}

# Función para construir el árbol de decisión de regresión recursivamente
construir_arbol <- function(data, target_variable, max_profundidad, profundidad_actual = 0) {
  if (profundidad_actual == max_profundidad || nrow(data) <= 1) {
    return(list(prediccion = mean(data[[target_variable]])))
  }
  
  variables <- setdiff(names(data), target_variable)
  mejor_cv <- -Inf
  mejor_variable <- ""
  
  for (variable in variables) {
    cv <- targetdev(data[[variable]])$cv
    
    if (cv > mejor_cv) {
      mejor_cv <- cv
      mejor_variable <- variable
    }
  }
  
  if (mejor_cv == -Inf) {
    return(list(prediccion = mean(data[[target_variable]])))
  }
  
  umbral <- median(data[[mejor_variable]])
  datos_menores <- data[data[[mejor_variable]] <= umbral, ]
  datos_mayores <- data[data[[mejor_variable]] > umbral, ]
  
  subarbol_menores <- construir_arbol(datos_menores, target_variable, max_profundidad, profundidad_actual + 1)
  subarbol_mayores <- construir_arbol(datos_mayores, target_variable, max_profundidad, profundidad_actual + 1)
  
  return(list(
    variable = mejor_variable,
    umbral = umbral,
    menores = subarbol_menores,
    mayores = subarbol_mayores
  ))
}


# Construir el árbol de decisión de regresión
profundidad_maxima <- 3
arbol <- construir_arbol(data, target_variable = "Horas_Jugadas", max_profundidad = profundidad_maxima)

# Visualizar el árbol
print(arbol)




