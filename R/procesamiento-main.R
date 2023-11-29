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

processing_incidental_catch_tallas = function(data_total){

  tallas = grep(pattern = "[1-9]", x = names(data_total), value = TRUE)

  catch_sps = data_total %>%
    mutate(catch = as.numeric(catch)) %>%
    group_by(codigo_faena, n_cala, descripcion) %>%
    reframe(catch = sum(catch)) %>%
    spread(descripcion, catch)
  names(catch_sps) = c(names(catch_sps)[c(1,2)],paste0("catch_",names(catch_sps)[-c(1,2)]))

  min_sps = data_total  %>%
    mutate(min_rango = apply(data_total[tallas], 1, min_range)) %>%
    select(codigo_faena, n_cala, descripcion, min_rango) %>%
    spread(descripcion, min_rango)

  names(min_sps) = c(names(min_sps)[c(1,2)],paste0("min_",names(min_sps)[-c(1,2)]))

  max_sps = data_total   %>%
    mutate(max_rango = apply(data_total[tallas], 1, max_range)) %>%
    select(codigo_faena, n_cala, descripcion, max_rango) %>%
    spread(descripcion, max_rango)
  names(max_sps) = c(names(max_sps)[c(1,2)],paste0("max_",names(max_sps)[-c(1,2)]))


  data_final = data_total %>%
    full_join(catch_sps, by = c("codigo_faena","n_cala")) %>%
    full_join(min_sps, by = c("codigo_faena","n_cala"))%>%
    full_join(max_sps, by = c("codigo_faena","n_cala")) %>%
    filter(descripcion %in% c("ANCHOVETA"))

  return(data_final)

}



generar_data_final = function(data){

  tallas = grep(pattern = "[1-9]", x = names(data), value = TRUE)

  catch_col = grep(pattern = "catch_", x = names(data), value = TRUE)

  min_col = grep(pattern = "min_", x = names(data), value = TRUE)

  max_col = grep(pattern = "max_", x = names(data), value = TRUE)

  names_data_final = c("n","armador","embarcacion","n_cala","fecha_inicio","hora_cala",catch_col,"profundidad","prof_sd","lon_inicial","lat_inicial","muestra","zona_pesca","porc_especie_incidental","sps_incidental","moda_inci",min_col, max_col,"juv","enmalle",tallas,"depredadores_superiores","observaciones","coeficientes","fp","moda","dc","dc_cat","promedio")


  data_final = as.data.frame(matrix(NA, ncol = length(names_data_final)))


  names(data_final) = names_data_final

  data_final = merge(data_final, data, all = TRUE)

  data_final = data_final[,names_data_final]

  return(data_final)


}



agregar_variables = function(data){

  data$n = 1:nrow(data)

  tallas = grep(pattern = "[1-9]", x = names(data), value = TRUE)

  data$juv = apply(data[,tallas], 1, Porc_Juveniles, tallas = tallas, juvLim = 12)

  data$dc = distancia_costa_vectorizado(lon = data$lon_inicial, lat = data$lat_inicial)

  data = data %>%
    mutate(dc_cat = case_when(dc >= 5  & dc < 15 ~ "05-15 mn",
                              dc >= 15 & dc < 30 ~ "15-30 mn",
                              dc >= 30 & dc < 50 ~ "30-50 mn",
                              dc >= 50 & dc < 100 ~ "50-100 mn"))

  return(data)

}
