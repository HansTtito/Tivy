% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date-time-functions.R
\name{convertir_a_fecha}
\alias{convertir_a_fecha}
\title{Convierte un vector de fechas en diferentes formatos a un formato de fecha estándar}
\usage{
convertir_a_fecha(vector_fecha)
}
\arguments{
\item{vector_fecha}{Un vector de caracteres que contiene fechas en diversos formatos.}
}
\value{
Un vector de objetos \code{Date} o \code{NA} si la fecha no puede ser convertida.
}
\description{
La función \code{convertir_a_fecha()} toma un vector de fechas en distintos formatos y las convierte a un formato de fecha estándar en R.
La función intenta analizar cada fecha utilizando una serie de formatos predefinidos y devuelve la primera fecha válida encontrada para cada entrada.
Si una fecha no puede ser interpretada por ninguno de los formatos, se asigna como \code{NA}.
}
\examples{
fechas <- c("2025-04-10", "10/04/2025", "April 10, 2025")
fechas_convertidas <- convertir_a_fecha(fechas)
print(fechas_convertidas)

}
