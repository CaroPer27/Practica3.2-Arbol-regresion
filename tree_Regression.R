# Función para calcular la reducción de la desviación estándar (SDR)
reduccion_desviacion_estandar <- function(data, target_col) {
  sd_target <- sd(data[[target_col]])
  sdr_result <- list()
  
  predictors <- setdiff(names(data), target_col)
  
  for (predictor in predictors) {
    unique_vals <- unique(data[[predictor]])
    sdr <- numeric(length(unique_vals))
    
    for (i in seq_along(unique_vals)) {
      subset_data <- data[data[[predictor]] == unique_vals[i], ]
      sd_subset <- sd(subset_data[[target_col]])
      sdr[i] <- sd_target - sd_subset
    }
    
    weighted_sdr <- sum(sdr * table(data[[predictor]]) / length(data[[predictor]]))
    sdr_result[[predictor]] <- weighted_sdr
  }
  
  return(sdr_result)
}

# Función para construir el árbol de decisión de regresión
construir_arbol_regresion <- function(data, target_col, max_depth, min_elements, cv_threshold) {
  if (length(unique(data[[target_col]])) == 1 || nrow(data) <= min_elements) {
    # Nodo hoja: calcular la media como valor final para el objetivo
    return(mean(data[[target_col]]))
  } else {
    # Encontrar el predictor con la mayor reducción de desviación estándar
    sdr_values <- reduccion_desviacion_estandar(data, target_col)
    best_predictor <- names(sdr_values)[which.max(unlist(sdr_values))]
    
    # Crear el nodo de decisión
    node <- list()
    node$attribute <- best_predictor
    node$children <- list()
    
    unique_vals <- unique(data[[best_predictor]])
    for (val in unique_vals) {
      subset_data <- data[data[[best_predictor]] == val, ]
      if (length(subset_data[[target_col]]) > 1 && sd(subset_data[[target_col]]) / mean(subset_data[[target_col]]) > cv_threshold) {
        # Continuar la subdivisión recursiva si CV supera el umbral
        node$children[[as.character(val)]] <- construir_arbol_regresion(subset_data, target_col, max_depth - 1, min_elements, cv_threshold)
      } else {
        # Nodo hoja: calcular la media como valor final para el objetivo
        node$children[[as.character(val)]] <- mean(subset_data[[target_col]])
      }
    }
    return(node)
  }
}


# Seleccionar el archivo CSV
ruta_archivo <- file.choose()

# Leer el archivo CSV
data <- read.csv(ruta_archivo)
# Supongamos que tienes un vector llamado new_hours_played con los nuevos valores
new_hours_played <- c(25,30,46,45,52,23,43,35,38,46,48,52,44,30)  # Ejemplo de nuevos 

data <- data.frame(
  Outlook = c("rainy","rainy","overcast","sunny","sunny","sunny","overcast","rainy","rainy","sunny","rainy","overcast","overcast","sunny"),
  Temp = c("hot","hot","hot","mild","cool","cool","cool","mild","cool","mild","mild","mild","hot","mild"),
  Humidity = c("high","high","high","high","normal","normal","normal","high","normal","normal","normal","high","normal","high"),
  Windy = c(FALSE, TRUE, FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE),
  Hours_Played = c(25,30,46,45,52,23,43,35,38,46,48,52,44,30)
)


# Asignar los nuevos valores a la columna hours_played
data$Hours_Played <- new_hours_played

arbol_regresion <- construir_arbol_regresion(data, "Hours_Played", max_depth = 3, min_elements = 3, cv_threshold = 0.1)

# Imprimir el árbol de decisión (puedes crear una función para imprimir el árbol)
print(arbol_regresion)


