#' Modelling of PM10 removal by vegetation
#'
#' This function models PM10 removal by vegetation
#' @param lai Leaf area index - raster grid with cell sizes in metre units
#' @param pbl Height of the planetary boundary layer in metres
#' @param pm10 PM10 concentration in atmosphere (microgram per m3)
#' @return List with one raster ofDaily  PM10 removal in micrgrams. Second is the percentage decrease in PM10 concentration.
#' @export

pm10 <- function(lai, pbl, pm10){
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

qDAY75 <- (lai * (0.9671 * pm10) * (24*60*60) * 0.5) * res(eastcoast)[1] * res(eastcoast)[2]
#qDAY32 <- (lai * (0.9671 * 32) * (24*60*60) * 0.5) * 10 * 9.93


# convert to gram
#q1<- q1*1e-6
#sum(getValues(qDAY75 ),na.rm=T) # national total in microgram

# calculate removal from air
# based on tallis etal 2011 model
# based on Li et al 2013 Singapore PBL/ Mixing height model.
# a night average would be 100 m
# a day average would be 700 m
# a rough average would be 400 m

# mean annual pm10 conc * mean annaul mixing layer height *
# 8760 (hours per year) * land area covered by urban canopy

# this one will do the annual removal. But first we need mean annual conc
totalPM10_DAY75 <- pm10* pbl * 24*60*60 * (sum(!is.na(getValues(qDAY75 )))* res(eastcoast)[1] * res(eastcoast)[2])

# percentage reduction in concentration
#((sum(getValues(qDAY75),na.rm=T)) / totalPM10_DAY75 )*100

list(qDAY75, ((sum(getValues(qDAY75),na.rm=T)) / totalPM10_DAY75 )*100)
}

