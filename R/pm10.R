#' Modelling of PM10 removal by vegetation
#'
#' This function models PM10 removal by vegetation
#' @param lai Leaf area index - raster grid with cell sizes in metre units
#' @param pbl Height of the planetary boundary layer in metres
#' @param pm10 PM10 concentration in atmosphere (micrograms per m3)
#' @return List with one raster of Daily  PM10 removal in micrograms. Second is the percentage decrease in PM10 concentration.
#' @export

pm10.model <- function(lai, lcm, pbl, pm10){
# use the equation Q = F*L*T*0.5 from Manes et al .2016
# where F = V*C
#C=concentration in air
#V = dry deposition velocity
#L = LAI
#T = period of time
# Lovett et al. 1994 suggest 0.0064m/s
# but zhongyu et al 2019 20 spp from Singapore median  = 0.9671

# take numbers from NEA dataset pm10_2nd_maximum_24hourly_mean
# microgram per m3
# 75 in 2014
# 215 in 2013
# urbancok et al 2017 a low day would be 32-70. Haze day 72 -310+

qDAY75 <-  ((0.0064/6) * pm10 * 1e-6 ) * (raster::res(lai)[1] * raster::res(lai)[2]) *   (24*60 * 60) * 0.5
# units are now in grams already

# fit strongly to boundary
qDAY75  <- raster::mask(qDAY75, lcm)
#qDAY32  <- mask(qDAY32, leon10)

qDAY75[qDAY75  < 0]<-0

# this one will do the percentage removal.
totalPM10_DAY75 <- pm10* 1e-6 * pbl * (sum(!is.na(getValues(qDAY75 )))* raster::res(lai)[1] * raster::res(lai)[2])

# percentage reduction in concentration
list(qDAY75, ((sum(raster::getValues(qDAY75),na.rm=T)) / totalPM10_DAY75 )*100)

}

