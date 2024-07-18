check_descarga_sisesat_data <- function(data) {
  # Definir los patrones para fecha y hora
  pattern_date <- "^(\\d{1,2}/\\d{1,2}/\\d{4})$"
  pattern_time <- "^(\\d{1,2}:\\d{2}:\\d{2})$"

  # Verificar las fechas de arribo
  if (!all(grepl(pattern_date, data$fecha_arribo))) {
    stop("Error: La columna 'fecha_arribo' debe estar en formato dmy (ej. 25/12/2023).")
  }

  # Verificar las horas de arribo
  if (!all(grepl(pattern_time, data$hora_arribo))) {
    stop("Error: La columna 'hora_arribo' debe estar en formato hms (ej. 14:30:00).")
  }

  # Verificar las fechas de zarpe
  if (!all(grepl(pattern_date, data$fecha_zarpe))) {
    stop("Error: La columna 'fecha_zarpe' debe estar en formato dmy (ej. 25/12/2023).")
  }

  # Verificar las horas de zarpe
  if (!all(grepl(pattern_time, data$hora_zarpe))) {
    stop("Error: La columna 'hora_zarpe' debe estar en formato hms (ej. 14:30:00).")
  }

  # Si todo está correcto, devolver un mensaje de éxito
  return("Las columnas de fecha y hora tienen las estructuras adecuadas.")
}




# Función para estandarizar la hora
estandarizar_hora <- function(horas) {
  # Verificar si cada elemento de horas está en formato HH:MM y añadir ":00" si es necesario
  horas <- ifelse(grepl("^\\d{1,2}:\\d{2}$", horas), paste0(horas, ":00"), horas)
  return(horas)
}
