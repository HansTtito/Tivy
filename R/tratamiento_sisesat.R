check_data_sisesat <- function(data_sisesat) {
  # Definir las columnas requeridas
  required_columns <- c("id_nave", "fecha_hora", "velocidad", "longitud_decimal", "latitud_decimal", "rumbo")

  # Encontrar las columnas que faltan
  missing_columns <- setdiff(required_columns, colnames(data_sisesat))

  # Si faltan columnas, generar un error indicando cuáles faltan
  if (length(missing_columns) > 0) {
    stop(paste("Error: Faltan las siguientes columnas:", paste(missing_columns, collapse = ", ")))
  }

  # Verificar la columna fecha y hora
  formato_fecha_hora <- all(grepl("^\\d{1,2}/\\d{2}/\\d{4} (\\d{2}:\\d{2}:\\d{2}|\\d{2}:\\d{2})$", data_sisesat$fecha_hora))

  if (!formato_fecha_hora) {
    stop("Error: La columna 'fecha_hora' debe estar en el formato %d/%m/%Y %H:%M:%S.")
  }

  # Verificar tipo y signo de longitud_decimal y latitud_decimal
  if (!all(is.numeric(data_sisesat$longitud_decimal) & data_sisesat$longitud_decimal < 0)) {
    stop("Error: La columna 'longitud_decimal' debe ser numérica y contener valores negativos.")
  }
  if (!all(is.numeric(data_sisesat$latitud_decimal) & data_sisesat$latitud_decimal < 0)) {
    stop("Error: La columna 'latitud_decimal' debe ser numérica y contener valores negativos.")
  }

  # Verificar tipo y signo de rumbo
  if (!all(is.numeric(data_sisesat$rumbo) & data_sisesat$rumbo >= 0)) {
    stop("Error: La columna 'rumbo' debe ser numérica y contener valores positivos.")
  }

  # Si todo está correcto, devolver un mensaje de éxito
  return("Los datos son adecuados y cumplen con las especificaciones.")
}



identificar_viajes <- function(data_sisesat, tiempo_entre_emision = 40) {
  # Ordenar los datos por id_nave y fecha_hora
  data_sisesat <- data_sisesat[order(data_sisesat$id_nave, data_sisesat$fecha_hora), ]

  # Inicializar el contador de viajes y el número de viaje para cada id_nave
  num_viaje <- 1
  ultimo_id_nave <- NULL
  num_viaje_id_nave <- 1

  # Crear vector para almacenar los números de viaje
  viajes <- rep(NA, nrow(data_sisesat))

  # Iterar sobre cada fila de los datos
  for (i in 1:nrow(data_sisesat)) {
    # Si cambia el id_nave, reiniciar el contador de viaje para ese id_nave
    if (!identical(data_sisesat$id_nave[i], ultimo_id_nave)) {
      num_viaje_id_nave <- 1
      ultimo_id_nave <- data_sisesat$id_nave[i]
    }

    # Si no es la primera fila y la diferencia es mayor a tiempo_entre_emision minutos, incrementar el número de viaje
    if (i > 1) {
      diff_minutos <- as.numeric(difftime(data_sisesat$fecha_hora[i], data_sisesat$fecha_hora[i - 1], units = "mins"))
      if (diff_minutos > tiempo_entre_emision) {
        num_viaje_id_nave <- num_viaje_id_nave + 1
      }
    }

    # Asignar el número de viaje actual
    viajes[i] <- num_viaje_id_nave
  }

  # Agregar la columna de viajes al data frame original
  data_sisesat$num_viaje <- viajes

  # Devolver el data frame con los viajes identificados
  return(data_sisesat)
}
