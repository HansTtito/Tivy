#' Línea costera de Perú
#'
#' Conjunto de datos que contiene la línea costera de Perú representada como un objeto espacial.
#'
#' @format Un data.frame con los siguientes campos:
#' \describe{
#'   \item{Long}{Longitud de la línea de costa del Perú}
#'   \item{Lat}{Latitud de la línea de costa del Perú}
#' }
#'
#' @source Instituto del Mar del Perú (IMARPE)
#'
#' @examples
#' data(linea_costa_peru)
"linea_costa_peru"


#' Áreas isoparalitorales
#'
#' Conjunto de datos que contiene áreas isoparalitorales de la costa peruana.
#' Estas áreas representan zonas con características similares a lo largo del litoral.
#'
#' @format Un data.frame con los siguientes atributos:
#' \describe{
#'   \item{lon}{Longitud correspondiente del área isoparalitoral}
#'   \item{lat}{Latitud correspondiente del área isoparalitoral}
#'   \item{area}{Código del Área Isoparalitoral}
#'   \item{grad}{Categoría de la latitud cada 0.5 grados (3 - 19.5)}
#'   \item{dc}{Categoría de distancia a la costa (10-200)}
#' }
#'
#' @source Instituto del Mar del Perú (IMARPE)
#'
#' @examples
#' data(areas_isoparalitorales_peru)
"areas_isoparalitorales_peru"


#' Calas de pesca
#'
#' Conjunto de datos que contiene información sobre calas de pesca en el mar peruano.
#' Las calas son lugares específicos donde se realizan actividades de pesca.
#'
#' @format Un data.frame con datos de ejemplo de las calas realizadas por embarcaciones en el mar peruano.
#'
#' @source Datos generados de manera aleatoria.
#'
#' @examples
#' data(calas_bitacora)
"calas_bitacora"


#' Faenas de pesca
#'
#' Conjunto de datos que contiene información sobre faenas de pesca realizadas en el litoral peruano.
#'
#' @format Un data.frame con datos de ejemplo de las faenas realizadas por embarcaciones en el mar peruano.
#'
#' @source Datos generados de manera aleatoria.
#'
#' @examples
#' data(faenas_bitacora)
"faenas_bitacora"


#' Datos de tallas muestreadas provenientes de las calas realizadas por las embarcaciones en el mar peruano.
#'
#' Conjunto de datos que contiene información sobre tallas de especies marinas
#' capturadas en el litoral peruano.
#'
#' @format Un data.frame con datos de ejemplo de tallas muestreadas provenientes de las calas realizadas por embarcaciones en el mar peruano.
#'
#' @source Indicar la fuente de los datos
#'
#' @examples
#' data(tallas_bitacora)
"tallas_bitacora"


#' Líneas paralelas a la costa peruana
#'
#' Conjunto de datos que contiene líneas paralelas a la costa peruana a diferentes distancias.
#' Estas líneas son útiles para análisis espaciales relacionados con la gestión pesquera.
#'
#' @format Una lista de data.frames, cada uno representa una línea paralela a la costa peruana.
#' \describe{
#' Cada data frame contiene:
#'   \item{lon}{Longitud correspondiente del área isoparalitoral}
#'   \item{lat}{Latitud correspondiente del área isoparalitoral}
#'   \item{dc}{Categoría de distancia a la costa (10-200)}
#' }
#'
#' @source Indicar la fuente de los datos
#'
#' @examples
#' data(paralelas_costa_peru)
"paralelas_costa_peru"



