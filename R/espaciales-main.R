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
