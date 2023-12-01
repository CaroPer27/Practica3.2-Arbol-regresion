file_path <- file.choose()

# Se lee el archivo CSV
data <- read.csv(file_path)


# Función para calcular la reducción de desviación estándar (SDR)
sdr_reduction <- function(data, target_name) {
  total_sd <- sd(data[[target_name]])
  
  # Calculando la reducción de desviación estándar después de la división
  vals <- unique(data[[target_name]])
  weighted_sd_reduction <- 0
  for (val in vals) {
    subset_data <- data[data[[target_name]] == val, ]
    weighted_sd_reduction <- weighted_sd_reduction + 
      (nrow(subset_data) / nrow(data)) * sd(subset_data[[target_name]])
  }
  
  sdr_reduction <- total_sd - weighted_sd_reduction
  return(sdr_reduction)
}

# Función para calcular el coeficiente de variabilidad
coefficient_of_variation <- function(data, target_name) {
  mean_val <- mean(data[[target_name]])
  sd_val <- sd(data[[target_name]])
  cv <- sd_val / mean_val
  return(cv)
}

# Función para calcular la ganancia de SDR
sdr_gain <- function(data, split_attribute_name, target_name) {
  total_sdr <- sdr_reduction(data, target_name)
  
  # Calculando la SDR después de la división
  vals <- unique(data[[split_attribute_name]])
  weighted_sdr <- 0
  for (val in vals) {
    subset_data <- data[data[[split_attribute_name]] == val, ]
    weighted_sdr <- weighted_sdr + 
      (nrow(subset_data) / nrow(data)) * sdr_reduction(subset_data, target_name)
  }
  
  sdr_gain <- total_sdr - weighted_sdr
  return(sdr_gain)
}

# Función para encontrar el mejor atributo para dividir
find_best_split <- function(data, target_name, attribute_names) {
  best_gain <- -Inf
  best_attribute <- NULL
  
  for (attribute in attribute_names) {
    gain <- sdr_gain(data, attribute, target_name)
    if (is.finite(gain) && gain > best_gain) {
      best_gain <- gain
      best_attribute <- attribute
    }
  }
  
  return(best_attribute)
}

# Clase Nodo para construir el árbol
Node <- function(data, target_name, attribute_names, depth, max_depth) {
  node <- list()
  node$split_attribute <- NULL
  node$children <- list()
  
  # Verificar si los datos son homogéneos o no
  if (length(unique(data[[target_name]])) == 1 || depth >= max_depth) {
    node$label <- mean(data[[target_name]])
    return(node)
  }
  
  # Encontrar el mejor atributo para dividir
  node$split_attribute <- find_best_split(data, target_name, attribute_names)
  vals <- unique(data[[node$split_attribute]])
  
  # Crear nodos hijos y recursión
  for (val in vals) {
    subset_data <- data[data[[node$split_attribute]] == val, ]
    subset_data <- subset_data[, !names(subset_data) %in% node$split_attribute]
    node$children[[as.character(val)]] <- Node(subset_data, target_name, attribute_names, depth + 1, max_depth)
  }
  
  return(node)
}

# Construir el árbol de regresión
#max_depth <- 3
#regression_tree <- Node(data, target_name = "Hours_Played", attribute_names = c("Outlook", "Temp", "Humidity", "Windy"), depth = 0, max_depth = max_depth)

# Imprimir la estructura del árbol
print(regression_tree)
