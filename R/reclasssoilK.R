#' Calculation of soil K factor based on soil data
#'
#' This function uses Singapore-specific K values and assigns them to land cover categories
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @return Map of K values
#' @export

reclass.soilK <- function(lcm){

  # This model uses the old land cover map classes so needs correction using reclassify
  lcm<- raster::reclassify(lcm, cbind(ncs2020::looktbl$gaw.2019.code, ncs2020::looktbl$old.code))



  K <- raster::reclassify(lcm, as.matrix(ncs2020::looktbl[,c(11,8)]))
K
}

