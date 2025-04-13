#' Línea costera de Perú
#'
#' Conjunto de datos que contiene la línea costera de Perú representada como un objeto espacial.
#'
#' @format Un objeto espacial (sf o sp) con los siguientes atributos:
#' \describe{
#'   \item{geometry}{Geometría lineal que representa la costa peruana}
#'   \item{...}{Otros atributos relevantes de la costa}
#' }
#'
#' @source Indicar la fuente de los datos: "Institución que proporcionó los datos"
#'
#' @examples
#' data(linea_costa_peru)
"linea_costa_peru"


#' Áreas isoparalitorales
#'
#' Conjunto de datos que contiene áreas isoparalitorales de la costa peruana.
#' Estas áreas representan zonas con características similares a lo largo del litoral.
#'
#' @format Un objeto espacial (sf o sp) o data.frame con los siguientes atributos:
#' \describe{
#'   \item{id}{Identificador único del área}
#'   \item{...}{Otros atributos relevantes de las áreas isoparalitorales}
#' }
#'
#' @source Indicar la fuente de los datos
#'
#' @examples
#' data(areas_isoparalitorales_peru)
#' # Código de ejemplo para explorar los datos
"areas_isoparalitorales_peru"

#' Calas de pesca
#'
#' Conjunto de datos que contiene información sobre calas de pesca en el litoral peruano.
#' Las calas son lugares específicos donde se realizan actividades de pesca.
#'
#' @format Un data.frame o objeto espacial con los siguientes atributos:
#' \describe{
#'   \item{id}{Identificador único de la cala}
#'   \item{lat}{Latitud de la cala (si es un data.frame)}
#'   \item{lon}{Longitud de la cala (si es un data.frame)}
#'   \item{geometry}{Geometría del punto (si es un objeto espacial)}
#'   \item{fecha}{Fecha de registro}
#'   \item{...}{Otros atributos relevantes de las calas}
#' }
#'
#' @source Indicar la fuente de los datos
#'
#' @examples
#' data(calas_bitacora)
#' # Código de ejemplo para explorar los datos
"calas_bitacora"

#' Faenas de pesca
#'
#' Conjunto de datos que contiene información sobre faenas de pesca realizadas en el litoral peruano.
#'
#' @format Un data.frame o objeto espacial con los siguientes atributos:
#' \describe{
#'   \item{id}{Identificador único de la faena}
#'   \item{fecha}{Fecha de la faena}
#'   \item{embarcacion}{Identificador o nombre de la embarcación}
#'   \item{...}{Otros atributos relevantes de las faenas de pesca}
#' }
#'
#' @source Indicar la fuente de los datos
#'
#' @examples
#' data(faenas_bitacora)
#' # Código de ejemplo para explorar los datos
"faenas_bitacora"

#' Líneas paralelas a la costa peruana
#'
#' Conjunto de datos que contiene líneas paralelas a la costa peruana a diferentes distancias.
#' Estas líneas son útiles para análisis espaciales relacionados con la gestión pesquera.
#'
#' @format Un objeto espacial (sf o sp) con los siguientes atributos:
#' \describe{
#'   \item{id}{Identificador único de la línea}
#'   \item{distancia}{Distancia a la costa en millas náuticas o km}
#'   \item{geometry}{Geometría lineal}
#'   \item{...}{Otros atributos relevantes}
#' }
#'
#' @source Indicar la fuente de los datos
#'
#' @examples
#' data(paralelas_costa_peru)
#' if(requireNamespace("sf", quietly = TRUE)) {
#'   plot(paralelas_costa_peru)
#' }
"paralelas_costa_peru"

#' Datos de tallas de especies marinas
#'
#' Conjunto de datos que contiene información sobre tallas de especies marinas
#' capturadas en el litoral peruano.
#'
#' @format Un data.frame con los siguientes atributos:
#' \describe{
#'   \item{id}{Identificador único del registro}
#'   \item{especie}{Nombre de la especie}
#'   \item{talla}{Longitud o talla del individuo en cm o mm}
#'   \item{fecha}{Fecha de captura o medición}
#'   \item{ubicacion}{Ubicación donde se realizó la medición}
#'   \item{...}{Otros atributos relevantes}
#' }
#'
#' @source Indicar la fuente de los datos
#'
#' @examples
#' data(tallas_bitacora)
"tallas_bitacora"
