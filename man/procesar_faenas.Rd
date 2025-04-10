% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing-faenas.R
\name{procesar_faenas}
\alias{procesar_faenas}
\title{Procesamiento de datos de faenas pesqueras}
\usage{
procesar_faenas(data_faenas, formato = "xlsx")
}
\arguments{
\item{data_faenas}{Data frame con los datos crudos de faenas.}

\item{formato}{Formato del archivo de entrada: "xlsx" (por defecto) o "csv".}
}
\value{
Un data frame con columnas estandarizadas: codigo_faena, embarcacion, armador, matricula, fecha_inicio (opcional), fecha_fin (opcional).
}
\description{
Procesa datos de faenas pesqueras provenientes de las bitácoras de PRODUCE,
en formato CSV o XLSX. Devuelve un data frame limpio y estandarizado.
}
\examples{
procesar_faenas(data_faenas = faenas, formato = "xlsx")
}
