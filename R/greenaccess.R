#' Modelling of public access to green space and natural ecosystems and ocean
#'
#' This function models soil erosion
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @param R Rainfall erosivity (MJ/  mm / year). Can be either a raster or single value
#' @param K Soil erodibility factor. Can be either a raster with K values for different land cover types or a single value
#' @param LS Topographic factors. Must be a raster. Creating an LS raster is straightforward but computationally expensive in R, so a function is not given.
#' @param ndvi Normalised difference vegetation index
#' @return List of three rasters - the ecosystem service ES, structural impact SI, and absolute soil erosion A
#' @export


access.model <- function(lcm, noentry = NULL, pop){

  # This model uses the old land cover map classes so needs correction using reclassify
  lcm<- raster::reclassify(lcm, cbind(ncs2020::looktbl$gaw.2019.code, ncs2020::looktbl$old.code))


  if(is.null(noentry)){
noentry <-lcm
noentry[,]<-0
  }

# get all green spaces
  gr1ha <- lcm
  gr1ha[gr1ha == 1]<-12
  gr1ha[gr1ha == 9 |
          gr1ha == 10 |
          gr1ha == 7 |
          gr1ha == 8 |
          gr1ha == 5 |
          gr1ha == 6 ] <- 1
  gr1ha[(gr1ha != 1)] <- NA

  #remove innaccessible spaces
  gr1ha[noentry>0]<-NA

  gr1ha <-raster::clump(gr1ha, directions = 4)


  grpc <- table(raster::getValues(gr1ha))


  # 1 ha is 10000 m2 so how many pixels?
  10000/ (raster::res(gr1ha)[1]*raster::res(gr1ha)[2])

  grpc <- as.numeric(names(grpc)[which( grpc > floor(10000/ (raster::res(gr1ha)[1]*raster::res(gr1ha)[2])))])
  # about 100.7 pixels

  gr1ha[is.na(raster::match(gr1ha,grpc))] <- NA
  gr1ha[!is.na(gr1ha)]<-1


  vx <- velox(gr1ha)
  #checkgr <- function(x){x2<-x[!is.na(x)]
  #  1 %in% x2}
  greenextract <- vx$extract(sp=pop2016buf, fun=mean2)
  table(greenextract>0)
  greenextract<-greenextract[!is.na(pop2016buf$pop) ]
  popextract2$green <- greenextract
  popextract2$green[popextract2$green >0]<-1
  popextract2$green[popextract2$green <=0]<-0

  # save output
  lookup$nmagnitude[lookup$service == "Recreation"]<-
    as.vector(tapply(popextract2$pop, popextract2$green, sum)/
                sum(tapply(popextract2$pop, popextract2$green, sum)))[2]


  # solve the population problem
  Sys.time()

  # it is a very challenging problem
  # can simplify by using points and aggregate and rasterize?

  # points is pop2016

  popD2 <- leon10

  popD2 <- aggregate(leon10, 300/10)

  pop2016 <- SpatialPointsDataFrame(pop2016, data.frame(pop=pop2016buf$pop))
  sum2<-function(x){sum(x,na.rm=T)}
  popD2<-rasterize(x=pop2016,
                   y = popD2,
                   field = "pop",
                   fun=sum,
                   na.rm=T)

  # wow that was fast
  # ok try it the focal way
  popD3<-rasterize(x=pop2016,
                   y = leon10,
                   field = "pop",
                   fun=sum,
                   na.rm=T)
  # focal
  buffer300 <- drawImage(matrix(0, 31, 31), 16, 15)  # call example
  popD3<-focal(popD3, buffer300, sum,na.rm=T)
  #plot(popD3)

  #plot(gr1ha)

  gr1haPOP <- popD3
  gr1haPOP[gr1ha == 0 ] <- 0

}


#plot(soilerosion.model(eastcoast$lcm, 50, 30, eastcoast$ls, eastcoast$ndvi)[[1]]/
 # soilerosion.model(eastcoast$lcm, 50, 30, eastcoast$ls, eastcoast$ndvi)[[2]])
