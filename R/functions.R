
processing_tallas = function(data_tallas) {

  data_tallas <- data_tallas %>% dplyr::select(3,4,5,8,10)

  names(data_tallas) <- c("codigo_faena","n_cala","descripcion","talla","freq")

  data_tallas = data_tallas %>% mutate(
    descripcion = str_trim(descripcion))

  data_tallas <- data_tallas %>% spread(talla, freq)

  return(data_tallas)

}


processing_faenas = function(data_faenas) {

  data_faenas <- data_faenas %>% select(11, 4, 3, 7)

  names(data_faenas) <- c("codigo_faena", "embarcacion", "armador","matricula")

  return(data_faenas)

}


lat_long = function(lat_lon){

  lat_lon_final = apply(str_split_fixed(lat_lon, " ", n = 3),2, function(x)as.numeric(gsub(pattern = "[' °]*", replacement = "", x = x)))

  lat_lon = -1*lat_lon_final[,1] - lat_lon_final[,2]/60 - lat_lon_final[,3]/3600

  return(lat_lon)
}



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
