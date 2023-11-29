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
