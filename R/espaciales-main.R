lat_long = function(lat_lon){

  lat_lon_final = apply(str_split_fixed(lat_lon, " ", n = 3),2, function(x)as.numeric(gsub(pattern = "[' °]*", replacement = "", x = x)))

  lat_lon = -1*lat_lon_final[,1] - lat_lon_final[,2]/60 - lat_lon_final[,3]/3600

  return(lat_lon)

}


distancia_costa_vectorizado <- function(lon, lat) {
  grados2mn  <- 60 * 180 / pi
  grados2rad <- pi / 180

  shore_rad <- Shoreline_Peru * grados2rad
  x_rad <- lon * grados2rad
  y_rad <- lat * grados2rad

  distancias <- apply(cbind(x_rad, y_rad), 1, function(coords) {
    xy_rad <- sin(coords[2]) * sin(shore_rad$Lat)
    yx_rad <- cos(coords[2]) * cos(shore_rad$Lat) * cos(shore_rad$Long - coords[1])
    return(min(acos(xy_rad + yx_rad) * grados2mn))
  })

  return(distancias)
}


# Define la función original
distancia_costa_vectorizado <- function(lon, lat) {
  grados2mn  <- 60 * 180 / pi
  grados2rad <- pi / 180

  shore_rad <- Shoreline_Peru * grados2rad
  x_rad <- lon * grados2rad
  y_rad <- lat * grados2rad

  distancias <- apply(cbind(x_rad, y_rad), 1, function(coords) {
    xy_rad <- sin(coords[2]) * sin(shore_rad$Lat)
    yx_rad <- cos(coords[2]) * cos(shore_rad$Lat) * cos(shore_rad$Long - coords[1])
    return(min(acos(xy_rad + yx_rad) * grados2mn))
  })

  return(distancias)
}

# Compila la función

puntos_tierra <- function(x_punto, y_punto, linea_costa) {

  linea_costa_correspondiente <- linea_costa[match(round(y_punto, 2), round(linea_costa$Lat, 2)), ]
  linea_costa_correspondiente[is.na(linea_costa_correspondiente)] <- 0  # Cambiar NA a 0

  datos_combinados <- data.frame(LonL = linea_costa_correspondiente$Long, LatL = linea_costa_correspondiente$Lat, LonP = round(x_punto, 2))

  # Calcular el producto cruzado para determinar si está a la izquierda o a la derecha
  producto_cruzado <- (datos_combinados$LonP - datos_combinados$LonL) * (datos_combinados$LatL)
  resultados <- ifelse(producto_cruzado > 0, "ok",
                       ifelse(producto_cruzado < 0, "en tierra", "Sobre la línea"))

  return(resultados)
}
