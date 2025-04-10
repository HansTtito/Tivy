% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{min_range}
\alias{min_range}
\title{Talla mínima observada con frecuencia positiva}
\usage{
min_range(frecuencia, tallas)
}
\arguments{
\item{frecuencia}{Un vector numérico con las frecuencias de tallas.}

\item{tallas}{Vector de tallas correspondiente.}
}
\value{
Valor mínimo de talla con frecuencia mayor que cero.
}
\description{
Talla mínima observada con frecuencia positiva
}
\examples{
min_range(c(0,0,1,2,3), c(5,6,7,8,9))
}
