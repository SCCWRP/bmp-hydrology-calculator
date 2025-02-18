#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' import leaflet
my_func <- function(x){
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(lng = x, lat = x)
  return(x + 1)
}
