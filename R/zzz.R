#' @keywords internal
"_PACKAGE"

## Quiet global variable warnings using utils::globalVariables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "peru_coastline",
    "peru_coast_parallels",
    "peru_isoparalitoral_areas",
    "calas_bitacora",
    "faenas_bitacora",
    "tallas_bitacora"
  ))
}
