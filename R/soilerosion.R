#' Modelling of soil erosion protection by ecosystems
#'
#' This function models soil erosion
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @param R Rainfall erosivity (MJ/  mm / year). Can be either a raster or single value
#' @param K Soil erodibility factor. Can be either a raster with K values for different land cover types or a single value
#' @param LS Topographic factors. Must be a raster. Creating an LS raster is straightforward but computationally expensive in R, so a function is not given.
#' @param ndvi Normalised difference vegetation index
#' @return List of three rasters - the ecosystem service ES, structural impact SI, and absolute soil erosion A
#' @export

soilerosion.model <- function(lcm, R, K, LS, ndvi){

  # first calculate C from ndvi as per Knijff et al 2000
  ndvi[ ndvi < 0] <- 0

  C <- exp(-2 *(ndvi/(1-ndvi)))
  C[lcm ==1 ] <- NA
  C[lcm ==2 ] <- NA


  # guerra et al 2014 ES estimation
  # units are tons per ha
  # SI (structural impact) is R * LS * K (Guerra et al 2014)
  SI <- R * LS * K
  SI[lcm ==1 ] <- NA
  SI[lcm ==2 ] <- NA

  # calculate A = absolute soil erosion
  A <- R * LS * K *C
  A[lcm ==1 ] <- NA
  A[lcm ==2 ] <- NA
  quantile(A, c(0.5, 0.75,0.95,0.99))
  # 180 000 tonnes or about 160714 metric tons is about 1m depth of 1 ha


  # calculate ES as SI- fraction of eroded soil after considering C
  ES <-SI-A

  # convert to actual tonnes rather than tonnes per ha
  # area of a cell in m is res(eastcoast)[1] * res(eastcoast)[2] assuming map units are m
  ES <- ES * ((res(lcm)[1] * res(lcm)[2])/10000)
  SI <- SI * ((res(lcm)[1] * res(lcm)[2])/10000)
  A <- A * ((res(lcm)[1] * res(lcm)[2])/10000)

  # remove 0 values - no soil loss on concrete
  SI[lcm == 3] <- NA
  ES[lcm == 3] <- NA
  A[lcm == 3] <- NA

# outputs
list(ES, SI, A)
}


#plot(soilerosion.model(eastcoast$lcm, 50, 30, eastcoast$ls, eastcoast$ndvi)[[1]]/
 # soilerosion.model(eastcoast$lcm, 50, 30, eastcoast$ls, eastcoast$ndvi)[[2]])
