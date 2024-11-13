# Función personalizada para calcular la media según los criterios
calc_custom_mean <- function(row) {
  # Eliminar valores NA
  row <- na.omit(row)
  
  # Si hay ceros en la fila, considerar solo los dos valores mayores a cero
  if (any(row == 0)) {
    row <- row[row != 0]  # Eliminar ceros
    if (length(row) >= 2) {
      row <- sort(row, decreasing = TRUE)[1:2]  # Tomar los dos valores mayores
    }
  }
  
  # Calcular la media de los valores restantes
  return(mean(row))
}

