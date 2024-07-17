
# Duracion Promedio calas -------------------------------------------------

calcular_duracion_promedio_cala <- function(data) {
  data %>%
    filter(!is.na(num_cala)) %>%
    mutate(fecha = as.Date(fecha_hora)) %>%
    reframe(
      duracion_cala = as.numeric(difftime(max(fecha_hora), min(fecha_hora), units = 'hours')),
      .by = c('nave', 'num_viaje', 'num_cala')
    ) %>%
    reframe(
      duracion_promedio_cala = mean(duracion_cala),
      .by = c('nave', 'num_viaje')
    )
}



# Duracion viaje y numero de calas ----------------------------------------

contar_calas <- function(num_cala) {
  bloques <- rle(num_cala)
  calas_unicas <- unique(bloques$values[!is.na(bloques$values)])
  return(length(calas_unicas))
}

calcular_dur_viaje_n_calas <- function(data) {
  data %>%
    mutate(fecha = as.Date(fecha_hora)) %>%
    reframe(
      n_calas = contar_calas(num_cala),
      duracion_viaje = as.numeric(difftime(max(fecha_hora), min(fecha_hora), units = 'hours')),
      fecha_zarpe = as.Date(min(fecha_hora)),
      fecha_arribo = as.Date(max(fecha_hora)),
      .by = c('nave', 'num_viaje')
    )
}



# Unir ambos --------------------------------------------------------------

calcular_esfuerzo_sisesat <- function(data_sisesat) {

  dur_promedio_cala <- calcular_duracion_promedio_cala(data_sisesat)
  dur_viaje_n_calas <- calcular_dur_viaje_n_calas(data_sisesat)
  esfuerzo_sisesat <-   full_join(dur_promedio_cala, dur_viaje_n_calas, by = c('nave', 'num_viaje'))

  return(esfuerzo_sisesat)
}

