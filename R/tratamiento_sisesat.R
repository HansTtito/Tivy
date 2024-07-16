## revisa que tenga todas las columnas de manera adecuada

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


## Función para identificar viajes

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


# Función para identificar viajes paralelamente
identificar_viajes_paralelo <- function(data_sisesat, tiempo_entre_emision = 40, nucleos = 6) {
  # Ordenar los datos por id_nave y fecha_hora
  data_sisesat <- data_sisesat %>%
    arrange(id_nave, fecha_hora)

  # Crear un plan de ejecución para paralelización
  plan(multisession, workers = nucleos)  # Cambiar a plan(multicore) en sistemas Unix para mejor rendimiento

  # Función interna para procesar un solo id_nave
  procesar_nave <- function(subset_nave) {
    num_viaje_id_nave <- 1
    viajes <- rep(NA, nrow(subset_nave))

    for (i in 1:nrow(subset_nave)) {
      if (i > 1) {
        diff_minutos <- as.numeric(difftime(subset_nave$fecha_hora[i], subset_nave$fecha_hora[i - 1], units = "mins"))
        if (diff_minutos > tiempo_entre_emision) {
          num_viaje_id_nave <- num_viaje_id_nave + 1
        }
      }
      viajes[i] <- num_viaje_id_nave
    }
    subset_nave$num_viaje <- viajes
    return(subset_nave)
  }

  # Aplicar la función a cada grupo de id_nave en paralelo
  data_sisesat_split <- split(data_sisesat, data_sisesat$id_nave)
  data_sisesat_list <- future_lapply(data_sisesat_split, procesar_nave)

  plan(sequential)

  # Combinar los resultados en un solo data frame
  data_sisesat <- bind_rows(data_sisesat_list) %>% as.data.frame()

  return(data_sisesat)
}


# Función para evaluar la duración de los viajes y eliminar los no válidos
evaluar_y_eliminar_viajes <- function(data_sisesat, tiempo_minimo_horas = 40) {

  # Evaluar la duración de cada viaje
  data_sisesat <- data_sisesat %>%
    group_by(id_nave, num_viaje) %>%
    mutate(duracion_viaje_horas = as.numeric(difftime(max(fecha_hora, na.rm = TRUE),
                                                      min(fecha_hora, na.rm = TRUE), units = "hours"))) %>%
    ungroup() %>%
    filter(duracion_viaje_horas >= tiempo_minimo_horas) %>%
    select(-duracion_viaje_horas)

  return(data_sisesat)
}


## Función para identificar calas

identificar_calas <- function(data_sisesat, distancia_minima = 15, velocidad_maxima = 3) {

  data_sisesat <- data_sisesat[order(data_sisesat$id_nave, data_sisesat$num_viaje, data_sisesat$fecha_hora), ]

  calas <- rep(NA, nrow(data_sisesat))  # Inicializar vector para marcar calas con 0

  for (i in 2:nrow(data_sisesat)) {
    if (data_sisesat$distancia_costa[i] > distancia_minima &&
        data_sisesat$velocidad[i] < velocidad_maxima) {
      calas[i] <- 1  # Marcar como cala con 1 si cumple las condiciones
    }
  }

  data_sisesat$num_cala <- calas
  return(data_sisesat)
}

## revisa que las calas duren un tiempo determinado
evaluar_bloques <- function(data_sisesat, tiempo_minimo_cala = 60) {
  # Inicializar el vector de salida con los mismos valores que num_cala
  resultado <- data_sisesat$num_cala
  num_cala <- data_sisesat$num_cala
  # Convertir fecha_hora a objeto de fecha y hora
  fecha_hora <- data_sisesat$fecha_hora

  i <- 1
  while (i <= length(num_cala)) {
    if (!is.na(num_cala[i]) && num_cala[i] == 1) {
      # Encontrar el final del bloque de 1s
      bloque_inicio <- i
      while (i <= length(num_cala) && !is.na(num_cala[i]) && num_cala[i] == 1) {
        i <- i + 1
      }
      bloque_fin <- i - 1

      # Calcular la diferencia de tiempo entre el primer y último registro del bloque
      if (!is.na(fecha_hora[bloque_inicio]) && !is.na(fecha_hora[bloque_fin])) {
        diferencia_tiempo <- difftime(fecha_hora[bloque_fin], fecha_hora[bloque_inicio], units = "mins")

        # Si la diferencia es menor a 1 hora, convertir el bloque de 1s a NA
        if (!is.na(diferencia_tiempo) && diferencia_tiempo < tiempo_minimo_cala) {
          resultado[bloque_inicio:bloque_fin] <- NA
        }
      }
    }
    i <- i + 1
  }

  data_sisesat$num_cala = resultado

  return(data_sisesat)
}


## Enumera las calas
numerar_bloques <- function(vector) {
  # Inicializar el vector de salida con NAs
  resultado <- rep(NA, length(vector))

  # Inicializar el número de bloque
  num_bloque <- 1

  # Iterar sobre el vector
  for (i in 1:length(vector)) {
    # Si el valor es 1 y el valor anterior es NA o i es 1, asignar el número de bloque
    if (!is.na(vector[i]) && vector[i] == 1) {
      if (i == 1 || is.na(vector[i - 1])) {
        resultado[i] <- num_bloque
        num_bloque <- num_bloque + 1
      } else {
        resultado[i] <- resultado[i - 1]
      }
    }
  }

  return(resultado)
}


## Enumera las calas por viaje

numerar_calas_por_viaje <- function(data) {
  data %>%
    group_by(id_nave, num_viaje) %>%
    mutate(num_cala = numerar_bloques(num_cala)) %>%
    ungroup() %>% as.data.frame()
}


## Procesa data total de sisesat, identifica viajes y calas

tratamiento_datos_sisesat = function(data_sisesat, tiempo_entre_emision = 40, tiempo_minimo_horas_viaje = 24, distancia_costa_minima = 10, velocidad_maxima = 6, tiempo_minimo_cala = 60, cores = 4){

  data_sisesat$fecha_hora = parse_date_time(data_sisesat$fecha_hora, c("%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M"))

  data_sisesat$distancia_costa = distancia_costa_vectorizado_paralelo(lon = data_sisesat$longitud_decimal, lat = data_sisesat$latitud_decimal, nucleos = cores)

  data_sisesat = identificar_viajes(data_sisesat = data_sisesat, tiempo_entre_emision = tiempo_entre_emision)

  data_sisesat = evaluar_y_eliminar_viajes(data_sisesat = data_sisesat, tiempo_minimo_horas = tiempo_minimo_horas_viaje)

  data_sisesat = identificar_viajes(data_sisesat = data_sisesat, tiempo_entre_emision = tiempo_entre_emision)

  data_sisesat <- identificar_calas(data_sisesat = data_sisesat, distancia_minima = distancia_costa_minima, velocidad_maxima = velocidad_maxima)

  data_sisesat = evaluar_bloques(data_sisesat = data_sisesat, tiempo_minimo_cala = tiempo_minimo_cala)

  data_sisesat = numerar_calas_por_viaje(data = data_sisesat)

  return(data_sisesat)

}

