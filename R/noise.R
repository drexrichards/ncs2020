#' Modelling of noise attenuation by vegetation
#'
#' This function models soil erosion
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @param dem Raster digital elevation model in same resolution as land cover map.
#' @param buildings PBuildings given as a SpatialPointsDataFrame with one field for building height.
#' @param roads Rasterised road network in same resolution as land cover map.
#' @param dB Decibels for road traffic noise sources.
#' @param erandom Number of random noise sources to generate (has direct impact on processing time)
#' @param humidity Humidity of the study area in %, defaults to Singapore typical conditions.
#' @param temperature Temperature of the study area in degrees celsius, defaults to Singapore typical conditions.
#' @param frequency Frequency of the road traffic noise for analysis. Defaults to typical road traffic noise conditions.
#' @return A stack of 6 raster maps, representing (1) noise after Spherical spreading, (2) noise after Atmospheric absorption, (3) noise after Barrier loss, (4) noise after Vegetation loss, (5) the vegetation ecosystem service, and (6) the location of noise sources.
#' @export


noise.model <- function(lcm, dem, buildings, roads, dB = 70, erandom = 100,
                        humidity = 70, temperature = 30, frequency = 1000){

  # This model uses the old land cover map classes so needs correction using reclassify
  lcm<- raster::reclassify(lcm, cbind(ncs2020::looktbl$gaw.2019.code, ncs2020::looktbl$old.code))

  roads[is.na(lcm)] <- NA
  roadsX <- raster::sampleRandom(roads, erandom, asRaster =T)
  eucdist <- raster::distance(roadsX) #in metres already

  # make 0 values to be 1m
  eucdist[eucdist == 0]<-1

  # calculate spherical spreading loss
  ssl <- 20 * log10(eucdist)
  # ------------------------------------------------------#

  # ------------------------------------------------------#
  # atmospheric absorbtion loss
  temperatureK <- temperature + 273.15

  # convert elevation to atmospheric pressure
  p_a <- 101.325 * (1- (2.25577 * (10 ** (-5)) * dem)) ** 5.25588

  # convert relative humidity to molar conc of water vapour
  C = (-6.8346 * ((273.16 / temperatureK) ** 1.261)) + 4.6151
  psat_pr <- 10 ** C
  h = (humidity) * (psat_pr) * ((p_a / 101.325) ** (-1))
  # calculate derived values for following equations
  pa_pr = p_a / 101.325
  T_Tr = temperatureK / 293.15
  e = 2.7182818284

  # Calculate frO (equation 3)
  frO = ((pa_pr) * ((24 + (4.04 * 10000)) * h) * (0.02 + h)) / (0.391 + h)

  # Calculate frN (equation 4)
  frN = pa_pr * (T_Tr ** (-0.5)) * (9 + (280 * h * (e ** (-4.170 * ((T_Tr ** (-0.33333)) - 1)))))

  # Calculate alpha (equation 5)
  term1 = 1.84 * (10 ** (-11)) * (pa_pr ** (-1)) * (T_Tr ** 0.5)
  term2 = (T_Tr ** (-2.5)) * (0.01275 * (e ** (-2239.1 / temperatureK)) * (frO / ((frO ** 2) + (frequency ** 2))))
  term3 = 0.1068 * (e ** (-3352 / temperatureK)) * (frN / ((frN ** 2) + (frequency ** 2)))
  alpha = 8.686 * (frequency ** 2)*(term1 + term2 + term3)

  # times alpha by eucdist to get real loss
  # but do it based on the average value because need to take average altitude
  # not a big deal for SG
  aal <- mean(raster::getValues(alpha),na.rm=T) * eucdist  #convert to feet
  rm(frN)
  rm(frO)
  rm(h)

  rm(alpha)

  # ------------------------------------------------------#

  # ------------------------------------------------------#
  # foliage and ground cover loss
  # constant raster of value 1
  ccRaster <- lcm
  ccRaster[,]<-1

  # subtract max value of euclidean grid and take absolute value
  eucdistz <- abs(eucdist - max(raster::getValues(eucdist)))

  # reclassif based on foliage and ground cover loss rates
  veglossrate <- raster::reclassify(lcm, ncs2020::looktbl[,c(2,9)])

  # calculate cost raster for veg loss by distance
  # need m to feet for some reason
  scalefactor <- (10)*3.28084
  vegcost <- ((veglossrate/ (eucdist*3.28084))/ scalefactor) + 1
  # testing add big bloc of veg
  #plot(vegcost)
  #e<-drawExtent()
  #cropper <- crop(vegcost, e)
  #cropper[,]<-1.19
  #cropper <- extend(cropper, extent(vegcost))
  #vegcost[cropper == 1.5] <- cropper[ cropper== 1.5]

  # need the accumulated veg cost fro mthe point sources
  # one with no veg cost
  #vegtrans  <- transition(vegcost, transitionFunction = function(x){1}, directions = 8)
  #vegtrans    <- geoCorrection(vegtrans)
  #roads4 <- rasterToPoints(roads)
  #roads4 <- roads4[roads4[,3] == 1,1:2]
  #pathdist1 <- accCost(vegtrans, roads4)

  # one with veg cost
  #vegtrans  <- transition(vegcost, transitionFunction = function(x){(x[2]+x[1])/2}, directions = 8)
  #vegtrans    <- geoCorrection(vegtrans)
  #plot(raster(vegtrans))
  #roads4 <- rasterToPoints(roads)
  #roads4 <- roads4[roads4[,3] == 1,1:2]
  #pathdist2 <- accCost(vegtrans, roads4)

  # subrtact 1 from 2
  #path2minuspath1 <- pathdist2-pathdist1
  # Cap total vegetation loss at 14 dB
  #path2minuspath1[path2minuspath1 < -14] <- -14


  # second round using a simpler method based on Harris 1980 it seems
  veglossrateH <- raster::reclassify(lcm, ncs2020::looktbl[,c(2,10)])
  veglossrateH <- veglossrateH*10 # conver to pixel loss from m loss
  # harrison et al 1980 think if less than 75 ft then cannot lose any sound
  veglossrateH[eucdist < 22.86] <- 0

  veglossrateH <- 1 +veglossrateH
  novegtrans <-lcm
  novegtrans[,] <- 1

  #maxlrh <- cellStats(veglossrateH,"max")
  vegtrans  <- gdistance::transition(veglossrateH, transitionFunction = function(x){1/mean(x)}, directions = 8)
  novegtrans <- gdistance::transition(novegtrans, transitionFunction = function(x){1}, directions = 8)
  vegtrans    <- gdistance::geoCorrection(vegtrans)
  novegtrans    <- gdistance::geoCorrection(novegtrans)

  roadsX2<- raster::rasterToPoints(roadsX)
  roadsX2<-roadsX2[,1:2]

  pathdistH1 <- gdistance::accCost(novegtrans, roadsX2)
  pathdistH2 <- gdistance::accCost(vegtrans, roadsX2)
  #pathdistH[pathdistH >200]<-NA
  #plot(stack(pathdistH1,pathdistH2))
  #plot(pathdistH2 - pathdistH1)
  Hloss <- (pathdistH2 - pathdistH1)

  # from their script
  # Cap total vegetation loss at 14 dB
  Hloss[Hloss > 14] <- 14
  Hloss[is.na(Hloss)]<-0
  Hloss[is.na(lcm)]<-0
  #plot(Hloss)
  #plot(stack(Hloss,path2minuspath1))
  # very comparable actually
  # ------------------------------------------------------#


  # ------------------------------------------------------#
  # topographic effects and barrier loss

  # assume that
  buildings2<-velox(lcm)
  buildingspts <- sp::SpatialPointsDataFrame(sp::coordinates(buildings),
                                         proj4string = raster::crs(buildings),
                                         data= data.frame("bdgrh_9090" = buildings$bdgrh_9090  ) )
  buildingspts <- rgeos::gBuffer(buildingspts, width = 5,byid=T)
  buildings2$rasterize(spdf=buildingspts,  field="bdgrh_9090",
                       background=0, small=T)
  buildings2 <- buildings2$as.RasterLayer(band = 1)
  #plot(stack(buildpoly, buildings2))
  # this is a key step in reducing the number of pixels tht need to be distanced later

  #buildings2[is.na(buildings2)] <- 0
  # the buildings rasterize is the very slow step - now sped up with velox

  dem2<-buildings2+dem


  # make noise source elevation map
  elevsource <- dem2
  elevsource[,]<-NA
  #elevsource[roads == 1] <- dem2[roads == 1]
  elevsource[roadsX ==1] <- dem2[roadsX == 1]

  es<- raster::rasterToPoints(elevsource)
  #es<- es[sample(1:length(es[,1]), erandom, F),]
  es2 <- dismo::voronoi(es, ext = raster::extent(elevsource))
  es2 <- maptools::spCbind(es2, es[,3])

  # another rasterize, can't be helped
  elevsource<-velox(lcm)
  elevsource$rasterize(spdf=es2,  field="es...3.",
                       background=0)
  elevsource <- elevsource$as.RasterLayer(band = 1)
  #elevsource <- rasterize(es2, lcm, "es...3.")
  rm(es)
  #rm(es2)

  # potential barriers
  potbar <- (dem2-elevsource)
  potbar[potbar < 10] <- 0
  #plot(potbar)

  # euc distance from barriers
  ps <- potbar
  ps[potbar ==0] <- NA
  bardist <- raster::distance(ps) #in metres already
  # make so cannot quite be 0
  bardist[bardist ==0]<-1
  rm(ps)

  # calculate slope between source and receiver
  slope <- (dem - elevsource)/eucdist

  # calculate elevation of source-receiver line under barrier
  elevsr <- (slope* bardist) + elevsource #eucdist is a change from their code, but it surely makes sense because used for slope
  #rm(slope)
  # it wont work. why not just set it at dem height for now?
  #elevsr <- dem

  # make height of blockage surface and reclass so cannot be negative
  hb <- raster::extract(potbar, sp::coordinates(buildingspts))
  hb<-sp::SpatialPointsDataFrame(sp::coordinates(buildingspts),
                             proj4string = raster::crs(buildingspts),
                             data = data.frame(hb=hb))
  hb<-hb[!is.na(hb$hb),]
  hb<-hb[!hb$hb ==0,]

  hb2 <- dismo::voronoi(hb, ext = raster::extent(lcm))

  hb3<-velox(lcm)
  hb3$rasterize(spdf=hb2,  field="hb",
                background=0)
  hb3 <- hb3$as.RasterLayer(band = 1)


  # so we need to find the areas that are not barriered -
  # ie those that do not have any obstructions from closest sound source

  # calculate x and y of each cell
  xm<-matrix(raster::xFromCell(lcm,c(1:raster::ncell(lcm))),nrow=raster::nrow(lcm),byrow=TRUE)
  xras<-raster::raster(xm,xmn=raster::xmin(lcm), xmx=raster::xmax(lcm),ymn=raster::ymin(lcm),ymx=raster::ymax(lcm))
  raster::projection(xras)<- raster::crs(lcm)

  ym<-matrix(raster::yFromCell(lcm,c(1:raster::ncell(lcm))),nrow=raster::nrow(lcm),byrow=TRUE)
  yras<-raster::raster(ym,xmn=raster::xmin(lcm), xmx=raster::xmax(lcm),ymn=raster::ymin(lcm),ymx=raster::ymax(lcm))
  raster::projection(yras)<- raster::crs(lcm)
  rm(xm)
  rm(ym)

  hb2$rowg <- 1:length(hb2)
  hb4<-velox(lcm)
  hb4$rasterize(spdf=hb2,  field="rowg",
                background=0)
  closestsound <- hb4$as.RasterLayer(band = 1)

  # add x and y
  locx <- raster::reclassify(closestsound,
                     as.matrix(data.frame(1:length(hb2), sp::coordinates(hb2)[,1])))
  locy <- raster::reclassify(closestsound,
                     as.matrix(data.frame(1:length(hb2), sp::coordinates(hb2)[,2])))

  # calculate euclidean directions from barriers
  bardir <- raster::atan2((yras-locy), (xras-locx) )
  # units are in radians
  bardir <- bardir * 57.2958
  # units are in degrees. degrees to face the building/ obstruction



  # and do the same for euclid dir from the point sources
  es2$rowg <- 1:length(es2)
  es4<-velox(lcm)
  es4$rasterize(spdf=es2,  field="rowg",
                background=0)
  closestsound <- es4$as.RasterLayer(band = 1)
  locx <- raster::reclassify(closestsound,
                     as.matrix(data.frame(1:length(roadsX2[,1]), roadsX2[,1])))
  locy <- raster::reclassify(closestsound,
                     as.matrix(data.frame(1:length(roadsX2[,1]), roadsX2[,2])))


  # calculate euclidean directions from sounds
  eucdir <- raster::atan2((yras-locy), (xras-locx) )
  # units are in radians
  eucdir <- eucdir * 57.2958


  # ok so now get the difference in angles from each
  difdir <- abs(eucdir-bardir)
  # if less than 90 degrees different, we consider it the same
  # and things that are the same are blocked


  #hb <- potbar#-elevsr # our potbar is already the height of blockage
  #hb[hb < 0]<-0
  # make hb so that it is within zones, not just at the blocking sites
  # do it via a focal within a 500m radius
  #500/10

  # add a function for the buffering
  # this function draws a circle in a matrix and you can set the radius
  #drawImage <- function(mat, center, radius) {
  #  grid <- mat
  #  x.index <- round(center + radius * cos(seq(0, 2*pi, length = 360)))
  #  y.index <- round(center + radius * sin(seq(0, 2*pi, length = 360)))
  #
  #  xyg <- data.frame(xind = round(center + radius * cos(seq(0, 2*pi, length = 360))),
  #                   yind =round(center + radius * sin(seq(0, 2*pi, length = 360))))

  #  for (i in seq(x.index)){

  #    fg <- range(xyg$yind[which(xyg$xind == xyg$xind[i])])
  #    grid[xyg$xind[i], fg[1]:fg[2]]<- 1
  #  }
  #  grid
  #image(grid)
  #}  # end drawImage

  # create buffer matrix - approximately a 500m buffer
  #buffer500 <- drawImage(matrix(0, 101, 101), 51, 50)  # call example
  #image(buffer50)

  # take the mean of the individual building heights
  #mean2 <- function(x){mean(unique(x),na.rm=T)}

  # run focal - cannot use velox - so quite slow
  #hb2 <- focal(hb, buffer500, mean2,pad = T)

  #hb2<-hb
  #hb2[is.na(hb2)]<-0
  #hb2<-velox(hb2)
  #hb2$meanFocal(weights=buffer500, bands=c(1))
  #hb2 <- hb2$as.RasterLayer(band = 1)

  #hb2[hb2 ==0 ]<- mean(getValues(hb2)[getValues(hb2) > 1])
  #hb2[hb2 ==1 ]<- mean(getValues(hb2)[getValues(hb2) > 1])

  #plot(hb2)

  # use hb3
  hb2 <- hb3
  rm(hb3)

  # do the math for N
  term1 = sqrt((hb2^2) + (bardist^2))
  term2 = sqrt((hb2^2) + ((eucdist-bardist)^2))
  # term 3 is just eucdist
  bpd <- term1 + term2 - eucdist

  # barrier factor (N)
  L = ((0.0000000000005*frequency**4) - (0.000000001*frequency**3) - (0.0000004*frequency**2) + (0.0028*frequency) - (0.3051))
  N <- bpd * L

  # use coeficients from power regression?
  bar <- (13.573) * (N^0.2299 )
  #plot(bar)

  # if barrier is closer than the closest sound, then it is blocked
  th<-( eucdist- bardist)
  th[th < 0]<- NA
  th[th==0] <-1
  #plot(th)
  # if not, it is not blocked
  bar[is.na(th)] <- 0
  # if barrier is within 90 degrees same direction, it is blocked
  bar[difdir > 90] <- 0

  # ------------------------------------------------------#



  # plotting test
  plotted <- raster::stack(dB - ssl ,
                   dB - ssl - aal ,
                   dB - ssl - aal - bar,
                   dB - ssl - aal - bar - Hloss)
  names(plotted) <- c("Spherical spreading",
                      "Atmospheric absorption",
                      "Barrier loss",
                      "Vegetation loss")
  plotted[plotted < 0] <- 0 # logic check - should be fine though

  #spplot(plotted)

  #Sys.time()
  # its running about 3 hours now

  #effect of veg
  #plot(stack(plotted[[3]] - plotted[[4]],
  #    roads))

  plotted<-raster::stack(plotted, plotted[[3]] - plotted[[4]], roadsX)
  names(plotted)<- c("Spherical spreading",
                     "Atmospheric absorption",
                     "Barrier loss",
                     "Vegetation loss",
                     "vegES",
                     "noiseSources")

plotted
}


