


# Calas -------------------------------------------------------------------

processing_calas = function(data_calas){

  data_calas <- data_calas %>% select(3,4,5,8,9,10,11,12,13,14,15,16,17,18)

  names(data_calas) <- c("codigo_faena", "n_cala", "fecha_inicio", "fecha_fin", "latitud_inicio", "longitud_inicio", "latitud_fin", "longitud_fin","tipo_arte", "descripcion","catch","estado","origen_cala","fecha_registro")

  data_calas <- data_calas %>% mutate(
    descripcion = stri_trim(descripcion),
    lat_inicial = lat_long(latitud_inicio),
    lon_inicial = lat_long(longitud_inicio),
    lat_final = lat_long(latitud_fin),
    lon_final = lat_long(longitud_fin))


  return(data_calas)
}



processing_calas_2 = function(data_calas)
{

  data_calas <- data_calas[,-1]

  names(data_calas) <- c("codigo_faena", "n_cala", "fecha_inicio",
                         "fecha_fin", "latitud_inicio", "longitud_inicio", "latitud_fin",
                         "longitud_fin", "tipo_arte", "descripcion", "catch",
                         "estado", "origen_cala", "fecha_registro")

  data_calas <- data_calas %>%
    mutate(descripcion = stri_trim(descripcion),
           lat_inicial = lat_long(latitud_inicio),
           lon_inicial = lat_long(longitud_inicio),
           lat_final = lat_long(latitud_fin),
           lon_final = lat_long(longitud_fin))

  return(data_calas)

}


# Faenas ------------------------------------------------------------------


processing_faenas = function(data_faenas) {

  data_faenas <- data_faenas %>% select(11, 4, 3, 7)

  names(data_faenas) <- c("codigo_faena", "embarcacion", "armador","matricula")

  return(data_faenas)

}


processing_faenas_2 = function(data_faenas)
{

  data_faenas <- data_faenas %>% select(8, 3, 2, 4)
  names(data_faenas) <- c("codigo_faena", "embarcacion", "armador",
                          "matricula")
  return(data_faenas)

}

processing_faenas_3 = function(data_faenas)
{

  data_faenas <- data_faenas %>% select(7, 2, 1, 3)
  names(data_faenas) <- c("codigo_faena", "embarcacion", "armador",
                          "matricula")
  return(data_faenas)

}


# Tallas calas ------------------------------------------------------------


processing_tallas = function(data_tallas) {

  data_tallas <- data_tallas %>% dplyr::select(3,4,5,8,10)

  names(data_tallas) <- c("codigo_faena","n_cala","descripcion","talla","freq")

  data_tallas = data_tallas %>% mutate(
    descripcion = str_trim(descripcion))

  data_tallas <- data_tallas %>% spread(talla, freq)

  return(data_tallas)

}


processing_tallas_2 = function(data_tallas)
{

  data_tallas = data_tallas[,-c(1,6)]

  names(data_tallas) <- c("codigo_faena", "n_cala", "descripcion", "talla", "freq")

  data_tallas = data_tallas %>%
    mutate(talla = as.numeric(talla),
           freq = as.numeric(freq),
           descripcion = str_trim(descripcion))

  data_tallas <- data_tallas %>% spread(talla, freq)

  return(data_tallas)

}


# Procesando data final ---------------------------------------------------

merge_tallasfaenas_calas = function(data_calas, data_tallasfaenas){

  tallas = grep(pattern = "[1-9]", x = names(data_tallasfaenas), value = TRUE)

  catch_sps = data_calas %>%
    filter(!is.na(descripcion), descripcion != "") %>%
    mutate(catch = as.numeric(catch)) %>%
    group_by(codigo_faena, n_cala, descripcion) %>%
    reframe(catch = sum(catch)) %>%
    spread(descripcion, catch, sep = "_")

  names(catch_sps) = gsub(pattern = "descripcion", replacement = "catch", x = names(catch_sps))

  min_sps = data_tallasfaenas %>%
    filter(!is.na(descripcion), descripcion != "") %>%
    mutate(min_rango = apply(data_tallasfaenas[tallas], 1, min_range)) %>%
    spread(descripcion, min_rango, sep = "_") %>%
    select("codigo_faena","n_cala", starts_with("descripcion_"))


  names(min_sps) = gsub(pattern = "descripcion", replacement = "min", x = names(min_sps))

  max_sps = data_tallasfaenas %>%
    mutate(max_rango = apply(data_tallasfaenas[tallas], 1, max_range)) %>%
    spread(descripcion, max_rango, sep = "_") %>%
    select("codigo_faena","n_cala", starts_with("descripcion_"))

  names(max_sps) = gsub(pattern = "descripcion", replacement = "max", x = names(max_sps))

  min_max_sps = merge(min_sps, max_sps, by = c("codigo_faena","n_cala"), all = TRUE)

  tallas_total = merge(data_tallasfaenas, min_max_sps, by = c("codigo_faena","n_cala"), all = TRUE)

  total_data  = merge(catch_sps, tallas_total, by = c("codigo_faena","n_cala"), all = TRUE)

  final_data = merge(data_calas %>% select(-grep(pattern = "catch", x = names(data_calas))), total_data, by = c("codigo_faena","n_cala","descripcion"), all = TRUE)

  final_data = final_data[!duplicated(final_data[,c("codigo_faena","n_cala")]),]

  return(final_data)

}



generar_data_final = function(data){

  tallas = grep(pattern = "[1-9]", x = names(data), value = TRUE)

  catch_col = grep(pattern = "catch_", x = names(data), value = TRUE)

  min_col = grep(pattern = "min_", x = names(data), value = TRUE)

  max_col = grep(pattern = "max_", x = names(data), value = TRUE)

  names_data_final = c("n","armador","embarcacion","n_cala","ini_fecha","hora_cala",catch_col,"profundidad","prof_sd","lon_inicial","lat_inicial","muestra","zona_pesca","porc_especie_incidental","sps_incidental","moda_inci",min_col, max_col,"juv","enmalle",tallas,"depredadores_superiores","observaciones","coeficientes","fp","moda","dc","dc_cat","promedio")


  data_final = as.data.frame(matrix(NA, ncol = length(names_data_final)))


  names(data_final) = names_data_final

  data_final = merge(data_final, data, all = TRUE)

  data_final = data_final[,names_data_final]

  return(data_final)

}



agregar_variables = function(data){

  # data$n = 1:nrow(data)

  tallas = grep(pattern = "[1-9]", x = names(data), value = TRUE)

  data$juv = apply(data[,tallas], 1, Porc_Juveniles, tallas = as.numeric(tallas), juvLim = 12)

  data$muestra = apply(data[,tallas], 1, sum, na.rm = TRUE)

  data$dc = distancia_costa_vectorizado(lon = data$lon_inicial, lat = data$lat_inicial)

  data = data %>%
    mutate(dc_cat = case_when(dc >= 5  & dc < 15 ~ "05-15 mn",
                              dc >= 15 & dc < 30 ~ "15-30 mn",
                              dc >= 30 & dc < 50 ~ "30-50 mn",
                              dc >= 50 & dc < 100 ~ "50-100 mn"))

  return(data)

}



ponderacion_by_row = function(data, tallas, a, b, colCatch){

  new_data = data[, c(names(data)[colCatch], as.character(tallas))]
  pesos = mapply(`*`, new_data[, as.character(tallas)], Length_weight(Length = tallas, a = a, b = b))
  FP = new_data[, 1]/apply(pesos, 1, sum, na.rm = TRUE)
  tallas_ponderadas = new_data[, as.character(tallas)] * FP[[1]]
  out = data[, setdiff(names(data), tallas)]
  tallas_ponderadas = cbind(out, tallas_ponderadas)

  return(tallas_ponderadas)

}



ponderacion_by_row_2 = function(data, tallas, a, b, colCatch){

  new_data = data[, c(names(data)[colCatch], as.character(tallas))]

  vector_pesos_talla = Length_weight(Length = tallas, a = a, b = b)

  pesos = t(apply(new_data[, as.character(tallas)], 1, function(x) x * vector_pesos_talla))

  pesos_row = if_else(apply(pesos, 1, sum, na.rm = TRUE) %in% 0, NA, apply(pesos, 1, sum, na.rm = TRUE))

  FP = new_data[, 1]/pesos_row

  tallas_ponderadas = new_data[, as.character(tallas)] * FP

  out = data[, setdiff(names(data), as.character(tallas))]

  tallas_ponderadas = cbind(out, tallas_ponderadas)

  return(tallas_ponderadas)

}



# checking data -----------------------------------------------------------

check_datos <- function(tibble_obj, tipo_archivo) {

  tipos_esperados <- switch(tipo_archivo,

                            calas = c("numeric", "character", "numeric", "POSIXct", "POSIXct", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "POSIXct"),
                            tallas_calas = c("numeric", "character", "numeric", "character", "numeric", "character", "numeric"),
                            faenas = c("numeric", "character", "character", "character", "character", "POSIXct", "POSIXct", "character", "numeric", "character", "character", "POSIXct"),
                            stop("Tipo de archivo no válido.")
  )

  tipos_columnas <- sapply(tibble_obj, function(col) {
    if (all(is.na(col))) {
      return("Solo NA")
    } else {
      col_no_na <- col[!is.na(col)]
      if (length(unique(col_no_na)) == 1 && !is.na(unique(col_no_na))) {
        return(class(col))
      } else {
        if ("POSIXct" %in% class(col)) {
          return("POSIXct")
        } else {
          return(class(col))
        }
      }
    }
  })

  columnas_no_coinciden <- names(tipos_columnas[tipos_columnas != tipos_esperados])

  if (!identical(as.character(tipos_columnas), tipos_esperados)) {
    mensaje_error <- paste("Error: Los tipos de datos no coinciden. Columnas problemáticas:\n", paste(columnas_no_coinciden, collapse = "\n "))
    stop(mensaje_error)
  } else {
    print("Los tipos de datos son correctos.")
  }
}


