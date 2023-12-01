# Definición de la clase Nodo
Nodo <- R6::R6Class(
  "Nodo",
  public = list(
    valor = NULL,
    prediccion = NULL,
    feature = NULL,
    umbral = NULL,
    izquierda = NULL,
    derecha = NULL,
    initialize = function(valor, prediccion, feature, umbral, izquierda, derecha) {
      self$valor <- valor
      self$prediccion <- prediccion
      self$feature <- feature
      self$umbral <- umbral
      self$izquierda <- izquierda
      self$derecha <- derecha
    }
  )
)

# Función para calcular la desviación estándar
calcular_desviacion_estandar <- function(data) {
  return(sd(data))
}

# Función para calcular la reducción de la desviación estándar (SDR)
calcular_sdr <- function(desviacion_total, desviacion_izquierda, desviacion_derecha) {
  n_total <- length(desviacion_total)
  n_izquierda <- length(desviacion_izquierda)
  n_derecha <- length(desviacion_derecha)
  
  sdr_izquierda <- (n_izquierda / n_total) * desviacion_izquierda
  sdr_derecha <- (n_derecha / n_total) * desviacion_derecha
  
  return(desviacion_total - (sdr_izquierda + sdr_derecha))
}

# Función para dividir el conjunto de datos en función de un umbral y una característica
dividir_datos <- function(data, feature, umbral) {
  izquierda <- data[data[, feature] <= umbral, ]
  derecha <- data[data[, feature] > umbral, ]
  return(list(izquierda, derecha))
}

# Función para encontrar el mejor umbral y característica para dividir los datos
# Función para encontrar el mejor umbral y característica para dividir los datos
encontrar_mejor_division <- function(data) {
  mejores_sdr <- 0
  mejor_caracteristica <- NULL
  mejor_umbral <- NULL
  
  desviacion_total <- calcular_desviacion_estandar(data[, "Hours_Played"])
  
  for (feature in colnames(data)) {
    if (feature != "Hours_Played") {
      for (valor in unique(data[[feature]])) {
        division <- dividir_datos(data, feature, valor)
        izquierda <- division[[1]][, "Hours_Played"]
        derecha <- division[[2]][, "Hours_Played"]
        
        desviacion_izquierda <- calcular_desviacion_estandar(izquierda)
        desviacion_derecha <- calcular_desviacion_estandar(derecha)
        
        cat("Feature:", feature, "\n")
        cat("Valor:", valor, "\n")
        cat("Desviación izquierda:", desviacion_izquierda, "\n")
        cat("Desviación derecha:", desviacion_derecha, "\n")
        
        if (!is.na(desviacion_izquierda) && !is.na(desviacion_derecha)) {
          sdr <- calcular_sdr(desviacion_total, desviacion_izquierda, desviacion_derecha)
          
          if (sdr > mejores_sdr || is.na(mejores_sdr)) {
            mejores_sdr <- sdr
            mejor_caracteristica <- feature
            mejor_umbral <- valor
          }
        }
      }
    }
  }
  
  return(list(mejor_caracteristica, mejor_umbral))
}

# Función para construir el árbol de decisión de regresión
construir_arbol <- function(data, profundidad_maxima) {
  if (profundidad_maxima <= 0 || nrow(data) <= 5) {  # Ajustar el número mínimo de filas para detener la división
    return(Nodo$new(mean(data[, "Hours_Played"]), NULL, NULL, NULL, NULL, NULL))
  }
  
  mejor_division <- encontrar_mejor_division(data)
  caracteristica <- mejor_division[[1]]
  umbral <- mejor_division[[2]]
  
  if (is.null(caracteristica) || is.null(umbral)) {
    return(Nodo$new(mean(data[, "Hours_Played"]), NULL, NULL, NULL, NULL, NULL))
  }
  
  division <- dividir_datos(data, caracteristica, umbral)
  izquierda <- construir_arbol(division[[1]], profundidad_maxima - 1)
  derecha <- construir_arbol(division[[2]], profundidad_maxima - 1)
  
  return(Nodo$new(NULL, mean(data[, "Hours_Played"]), caracteristica, umbral, izquierda, derecha))
}

# Función para imprimir la estructura del árbol resultante
imprimir_arbol <- function(nodo, nivel = 0) {
  cat(paste(rep(" ", nivel * 2), collapse = ""))
  
  if (is.null(nodo$valor)) {
    cat(paste(nodo$feature, "<=", nodo$umbral, "\n"))
    imprimir_arbol(nodo$izquierda, nivel + 1)
    imprimir_arbol(nodo$derecha, nivel + 1)
  } else {
    cat("Predicción:", nodo$prediccion, "\n")
  }
}

# Cargar el archivo CSV 'weather.csv'
# Definir la ruta del archivo CSV
ruta_archivo <- "C:/Users/ACER/Downloads/weather_regresion.csv"

weather <- read.csv(ruta_archivo)
weather

# Muestra los nombres de las columnas en tu conjunto de datos 'weather'
colnames(weather)

# Asegúrate de que los nombres de las columnas coincidan con los nombres esperados en el código previo
# Puedes verificar los nombres de las columnas con: colnames(weather)

# Construir el árbol de decisión de regresión
arbol <- construir_arbol(weather, profundidad_maxima = 3)

# Imprimir la estructura del árbol resultante
imprimir_arbol(arbol)




