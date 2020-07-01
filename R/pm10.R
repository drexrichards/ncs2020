#' Modelling of PM10 removal by vegetation
#'
#' This function models PM10 removal by vegetation
#' @param lai Leaf area index - raster grid with cell sizes in metre units
#' @param lcm Land cover map using Gaw et al. typology
#' @param pbl Height of the planetary boundary layer in metres
#' @param pm10 PM10 concentration in atmosphere (micrograms per m3)
#' @return List with one raster of Daily  PM10 removal in micrograms. Second is the percentage decrease in PM10 concentration.
#' @export

pm10.model <- function(lai, lcm, pbl, pm10){

  # This model uses the old land cover map classes so needs correction using reclassify
  lcm<- raster::reclassify(lcm, cbind(ncs2020::looktbl$gaw.2019.code, ncs2020::looktbl$old.code))


qDAY75 <- ((0.0064/6) * pm10* 1e-6 ) * ( (raster::res(lai)[1] * raster::res(lai)[2])) * lai *  (24*60 * 60) * 0.5
# units are now in grams already

# fit to boundary
qDAY75  <- raster::mask(qDAY75, lcm)

qDAY75[qDAY75  < 0]<-0

# this one will do the percentage removal.
totalPM10_DAY75 <- pm10* 1e-6 * pbl * (sum(!is.na(getValues(qDAY75 )))* raster::res(lai)[1] * raster::res(lai)[2])

# convert to grams per m2
grams <- qDAY75/ (raster::res(lai)[1] * raster::res(lai)[2])

# percentage reduction in concentration
list(grams, ((sum(raster::getValues(qDAY75),na.rm=T)) / totalPM10_DAY75 )*100)

}

