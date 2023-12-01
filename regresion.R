# Seleccionar el archivo CSV
ruta_archivo <- file.choose()

# Leer el archivo CSV
datos <- read.csv(ruta_archivo)

# Función para dividir los datos en dos grupos según una variable y un umbral
dividir_datos <- function(datos, variable, umbral) {
  return(list(izquierda = datos[datos[, variable] <= umbral, ], 
              derecha = datos[datos[, variable] > umbral, ]))
}
# Función para calcular la reducción de la desviación estándar
reduccion_desviacion_estandar <- function(data, left, right) {
  sd_before <- sd(data$Hours_Played)
  sd_left <- sd(left$Hours_Played)
  sd_right <- sd(right$Hours_Played)
  
  n_before <- nrow(data)
  n_left <- nrow(left)
  n_right <- nrow(right)
  
  reduction <- sd_before - ((n_left/n_before) * sd_left + (n_right/n_before) * sd_right)
  
  return(reduction)
}

# Función para calcular el coeficiente de variación
coeficiente_variacion <- function(target) {
  return(sd(target) / mean(target))
}

# Función para encontrar la mejor división
encontrar_mejor_division <- function(datos) {
  mejor_variable <- NULL
  mejor_umbral <- NULL
  mejor_reduccion <- -Inf
  
  for (variable in colnames(datos[, -ncol(datos)])) {
    valores_unicos <- unique(datos[, variable])
    for (umbral in valores_unicos) {
      divisiones <- dividir_datos(datos, variable, umbral)
      if (nrow(divisiones$izquierda) > 0 && nrow(divisiones$derecha) > 0) {
        reduccion <- reduccion_desviacion_estandar(datos, divisiones$izquierda, divisiones$derecha)
        if (reduccion > mejor_reduccion) {
          mejor_reduccion <- reduccion
          mejor_variable <- variable
          mejor_umbral <- umbral
        }
      }
    }
  }
  
  return(list(variable = mejor_variable, umbral = mejor_umbral))
}

# Función para construir el árbol de regresión
construir_arbol_regresion <- function(datos, profundidad, max_profundidad) {
  if (profundidad >= max_profundidad || nrow(datos) <= 1) {
    # Hoja: devolver información de la hoja
    return(list(media = mean(datos$Hours_Played)))
  } else {
    # Dividir los datos en dos grupos
    division <- encontrar_mejor_division(datos)
    datos_izquierda <- dividir_datos(datos, division$variable, division$umbral)$izquierda
    datos_derecha <- dividir_datos(datos, division$variable, division$umbral)$derecha
    
    # Construir los sub-árboles recursivamente
    arbol_izquierda <- construir_arbol_regresion(datos_izquierda, profundidad + 1, max_profundidad)
    arbol_derecha <- construir_arbol_regresion(datos_derecha, profundidad + 1, max_profundidad)
    
    # Devolver un nodo con información de división y sub-árboles
    return(list(variable = division$variable,
                umbral = division$umbral,
                izquierda = arbol_izquierda,
                derecha = arbol_derecha))
  }
}

# Construir el árbol de regresión
profundidad_maxima <- 3
arbol_regresion <- construir_arbol_regresion(datos, profundidad = 2, max_profundidad = profundidad_maxima)

# Imprimir la estructura del árbol
print(arbol_regresion)




