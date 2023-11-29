Length_weight <- function(Length ,a , b) {
  w = a * (Length^b)
  return(w)
}


# Función de Ponderación --------------------------------------------------


Ponderacion <- function(frecuencia, captura, tallas, a, b) {

  if (sum(frecuencia, na.rm = TRUE) != 0) {

    peso <- Length_weight(Length = tallas,a = a,b = b) * frecuencia
    talla_ponderada <- (captura / sum(peso, na.rm = TRUE)) * frecuencia

  } else {

    talla_ponderada <- rep(0, length(tallas))

  }

  return(talla_ponderada)

}


# Convirtiendo de número a peso -------------------------------------------


number_to_weight <- function(data, tallas, a, b){

  peso <- as.data.frame(t(apply(data[, as.character(tallas)], 1 , function(x)Length_weight(Length = tallas,a = a,b = b)*x)))
  id <- setdiff(names(data), as.character(tallas))
  peso <- cbind(data[,id], peso)
  return(peso)

}


add_peso_juv_adulto = function(data, tallas){

  data$juv_peso = apply(data[,as.character(tallas[tallas<12])],1,sum, na.rm = TRUE)
  data$adu_peso = apply(data[,as.character(tallas[tallas>=12])],1,sum, na.rm = TRUE)
  return(data)

}


Porc_Juveniles <- function(data, tallas, juvLim = 12) {

  juv <- 100*(sum(data[tallas < juvLim], na.rm = TRUE)/sum(data, na.rm = TRUE))

  return(juv)

}


min_range <- function(data) {

  data[data==0] <- NA
  min_v <- min(as.numeric(names(data)[!is.na(data)]))

  return(min_v)

}


max_range <- function(data) {

  data[data==0] <- NA
  max_v <- max(as.numeric(names(data)[!is.na(data)]))

  return(max_v)

}
