convertir_a_fecha <- function(vector_fecha) {
  formatos <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y")

  for (formato in formatos) {
    intento <- try(parse_date_time(vector_fecha, formato), silent = TRUE)
    if (!inherits(intento, "try-error")) {
      # Convertir a la clase "Date" usando lubridate
      fecha_convertida <- parse_date_time(vector_fecha, formato) #%>% as_date()
      return(fecha_convertida)
    }
  }

  return(NULL)  # Si no se encuentra un formato válido
}
