#' Modelling of noise attenuation by vegetation - model for fast modelling of veg only
#'
#' This function speeds up models of noise attenuation by vegetation cover, if only the veg cover is changing in the new scenario. It is used to update an existing modelled scenario with identical noise sources, buildings, topography, etc.
#' @param lcm2 Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @param roadsX Rasterised road network in same resolution as land cover map.
#' @param dB Decibels for road traffic noise sources.
#' @param erandom Number of random noise sources to generate (has direct impact on processing time)
#' @param humidity Humidity of the study area in %, defaults to Singapore typical conditions.
#' @param temperature Temperature of the study area in degrees celsius, defaults to Singapore typical conditions.
#' @param frequency Frequency of the road traffic noise for analysis. Defaults to typical road traffic noise conditions.
#' @param ssl SSL layer from output of noise.model
#' @param aal AAL layer from output of noise.model
#' @param bar BAR layer from output of noise.model
#' @return A stack of 6 raster maps, representing (1) noise after Spherical spreading, (2) noise after Atmospheric absorption, (3) noise after Barrier loss, (4) noise after Vegetation loss, (5) the vegetation ecosystem service, and (6) the location of noise sources.
#' @export


veg.noise.model<- function (lcm2, dB = 70, erandom = 100,
                            humidity = 70, temperature = 30, frequency = 1000,
                            roadsX,
                            ssl,
                            aal,
                            bar)
{
  lcm2 <- raster::reclassify(lcm2, cbind(ncs2020::looktbl$gaw.2019.code,
                                         ncs2020::looktbl$old.code))

  # constant raster of value 1
  ccRaster <- lcm2
  ccRaster[,]<-1

  # reclassif based on foliage and ground cover loss rates
  veglossrate <- reclassify(lcm2, looktbl[,c(2,9)])

  # calculate cost raster for veg loss by distance
  # need m to feet for some reason
  scalefactor <- (10)*3.28084
  vegcost <- ((veglossrate/ (eucdist*3.28084))/ scalefactor) + 1

  # second round using a simpler method based on Harris 1980 it seems
  veglossrateH <- reclassify(lcm2, looktbl[,c(2,10)])
  veglossrateH <- veglossrateH*10 # conver to pixel loss from m loss
  # harrison et al 1980 think if less than 75 ft then cannot lose any sound
  veglossrateH[eucdist < 22.86] <- 0

  veglossrateH <- 1 +veglossrateH
  novegtrans <-lcm2
  novegtrans[,] <- 1

  #maxlrh <- cellStats(veglossrateH,"max")

  removeTmpFiles()
  vegtrans  <- transition(veglossrateH, transitionFunction = function(x){1/mean(x)}, directions = 8)
  removeTmpFiles()
  # novegtrans <- transition(novegtrans, transitionFunction = function(x){1}, directions = 8)
  vegtrans    <- geoCorrection(vegtrans)
  # removeTmpFiles()
  # novegtrans    <- geoCorrection(novegtrans)

  roadsX2<- rasterToPoints(roadsX)
  roadsX2<-roadsX2[,1:2]


  #pathdistH1 <- accCost(novegtrans, roadsX2)
  pathdistH2 <- accCost(vegtrans, roadsX2)
  #pathdistH[pathdistH >200]<-NA
  #plot(stack(pathdistH1,pathdistH2))
  #plot(pathdistH2 - pathdistH1)
  Hloss <- (pathdistH2 - pathdistH1)

  # from their script
  # Cap total vegetation loss at 14 dB
  Hloss[Hloss > 14] <- 14
  Hloss[is.na(Hloss)]<-0
  Hloss[is.na(lcm2)]<-0

  plotted <- raster::stack(dB - ssl,
                           dB - ssl - aal,
                           dB - ssl - aal - bar,
                           dB - ssl - aal - bar - Hloss)
  names(plotted) <- c("Spherical spreading", "Atmospheric absorption",
                      "Barrier loss", "Vegetation loss")
  plotted[plotted < 0] <- 0
  plotted <- raster::stack(plotted, plotted[[3]] - plotted[[4]],
                           roadsX)
  names(plotted) <- c("Spherical spreading", "Atmospheric absorption",
                      "Barrier loss", "Vegetation loss", "vegES", "noiseSources")
  plotted
}



