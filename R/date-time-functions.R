

convertir_a_fecha <- function(vector_fecha) {

  formatos_fecha <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y/%m/%d",
    "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M", "%d-%m-%Y",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
    "%m-%d-%Y %H:%M:%S", "%m-%d-%Y %H:%M", "%m-%d-%Y",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
    "%b %d, %Y %H:%M:%S", "%b %d, %Y %H:%M", "%b %d, %Y",
    "%d %b %Y %H:%M:%S", "%d %b %Y %H:%M", "%d %b %Y",
    "%d-%b-%Y %H:%M:%S", "%d-%b-%Y %H:%M", "%d-%b-%Y",
    "%d/%b/%Y %H:%M:%S", "%d/%b/%Y %H:%M", "%d/%b/%Y"
  )

  formatos_finales = Date()

  for(fecha in vector_fecha){

    dats = Date()

    for(formato in formatos_fecha){

      intento <- tryCatch({
        parse_date_time(fecha, orders = formato)
      }, warning = function(w) {
        return(NA)
      }, error = function(e) {
        return(NA)
      })

      dats = c(dats, intento[!is.na(intento)])

    }

    formatos_finales = c(formatos_finales, dats[1])

  }

  return(formatos_finales)

}


vector_fecha <- c("2023-06-28 12:34:56", "25/06/2023 12:34", "2023-06-24", "Jun 28, 2023 12:34:56", "28-Jun-2023 12:34")

convertir_a_fecha(vector_fecha)
