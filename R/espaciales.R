lat_long = function(lat_lon){

  lat_lon_final = apply(str_split_fixed(lat_lon, " ", n = 3),2, function(x)as.numeric(gsub(pattern = "[' °]*", replacement = "", x = x)))

  lat_lon = -1*lat_lon_final[,1] - lat_lon_final[,2]/60 - lat_lon_final[,3]/3600

  return(lat_lon)

}
