% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliar.R
\name{fetch_announcements_batch}
\alias{fetch_announcements_batch}
\title{Obtener lote de anuncios}
\usage{
fetch_announcements_batch(
  start_index,
  batch_size,
  token,
  cookies,
  main_url,
  tipo = 2,
  start_date = "",
  end_date = "",
  verbose = TRUE
)
}
\arguments{
\item{start_index}{Índice de inicio para paginación}

\item{batch_size}{Tamaño del lote}

\item{token}{Token de seguridad}

\item{cookies}{Cookies para la petición}

\item{main_url}{URL de referencia}

\item{tipo}{Tipo de anuncio}

\item{start_date}{Fecha de inicio (opcional)}

\item{end_date}{Fecha de fin (opcional)}

\item{verbose}{Mostrar información detallada}
}
\value{
DataFrame con los anuncios o NULL si hay error
}
\description{
Recupera un lote de anuncios usando el token
}
\keyword{internal}
