#' Peruvian coastline
#'
#' Dataset containing the coastline of Peru represented as a spatial object.
#'
#' @format A data.frame with the following fields:
#' \describe{
#'   \item{Long}{Longitude of the Peruvian coastline}
#'   \item{Lat}{Latitude of the Peruvian coastline}
#' }
#'
#' @source Marine Institute of Peru (IMARPE)
#'
#' @examples
#' data(peru_coastline)
"peru_coastline"


#' Isoparalittoral areas
#'
#' Dataset containing isoparalittoral areas of the Peruvian coast.
#' These areas represent zones with similar characteristics along the littoral.
#'
#' @format A data.frame with the following attributes:
#' \describe{
#'   \item{lon}{Longitude corresponding to the isoparalittoral area}
#'   \item{lat}{Latitude corresponding to the isoparalittoral area}
#'   \item{area}{Isoparalittoral Area code}
#'   \item{grad}{Latitude category every 0.5 degrees (3 - 19.5)}
#'   \item{dc}{Distance to coast category (10-200)}
#' }
#'
#' @source Marine Institute of Peru (IMARPE)
#'
#' @examples
#' data(peru_isoparalitoral_areas)
"peru_isoparalitoral_areas"


#' Fishing hauls
#'
#' Dataset containing information about fishing hauls in the Peruvian sea.
#' Hauls are specific locations where fishing activities are conducted.
#'
#' @format A data.frame with sample data of hauls made by vessels in the Peruvian sea.
#'
#' @source Randomly generated data.
#'
#' @examples
#' data(calas_bitacora)
"calas_bitacora"


#' Fishing trips
#'
#' Dataset containing information about fishing trips conducted along the Peruvian littoral.
#'
#' @format A data.frame with sample data of fishing trips made by vessels in the Peruvian sea.
#'
#' @source Randomly generated data.
#'
#' @examples
#' data(faenas_bitacora)
"faenas_bitacora"


#' Length data sampled from hauls made by vessels in the Peruvian sea.
#'
#' Dataset containing information about lengths of marine species
#' captured along the Peruvian littoral.
#'
#' @format A data.frame with sample data of lengths sampled from hauls made by vessels in the Peruvian sea.
#'
#' @source Indicate the data source
#'
#' @examples
#' data(tallas_bitacora)
"tallas_bitacora"


#' Lines parallel to the Peruvian coast
#'
#' Dataset containing lines parallel to the Peruvian coast at different distances.
#' These lines are useful for spatial analyses related to fishery management.
#'
#' @format A list of data.frames, each representing a line parallel to the Peruvian coast.
#' \describe{
#' Each data frame contains:
#'   \item{lon}{Longitude corresponding to the isoparalittoral area}
#'   \item{lat}{Latitude corresponding to the isoparalittoral area}
#'   \item{dc}{Distance to coast category (10-200)}
#' }
#'
#' @source Indicate the data source
#'
#' @examples
#' data(peru_coast_parallels)
"peru_coast_parallels"

