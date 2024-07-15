# Función para extraer solo la parte numérica de una cadena
extraer_numeros <- function(cadena) {
  # Usar gsub para eliminar cualquier cosa que no sea un dígito
  numeros <- gsub("[^0-9]", "", cadena)
  return(numeros)
}

