#' Calculation of soil K factor based on soil data
#'
#' This function uses Singapore-specific K values and assigns them to land cover categories
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @return Map of K values
#' @export

reclass.soilK <- function(lcm){

  K <- raster::reclassify(lcm, as.matrix(ncs2020::looktbl[,c(11,8)]))
K
}

