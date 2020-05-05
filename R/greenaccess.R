#' Modelling of public access to green space and natural ecosystems and open water
#'
#' This function models soil erosion
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @param noentry Raster indicating areas of no entry with values of 1. Function can also be used without this value, defaults to no inaccessible space in the study area.
#' @param pop Population given as a SpatialPointsDataFrame with one field for population, which is named "pop"
#' @return A stack of three raster maps indicating the population pressure (number of people) with access to each pixel of (1) accessible green space in a patch larger than 1 ha, (2) blue space, (3) natural space.
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


# add functions for buffering
  sum2<-function(x){sum(x,na.rm=T)}

  drawImage <- function(mat, center, radius) {
    grid <- mat
    x.index <- round(center + radius * cos(seq(0, 2*pi, length = 360)))
    y.index <- round(center + radius * sin(seq(0, 2*pi, length = 360)))

    xyg <- data.frame(xind = round(center + radius * cos(seq(0, 2*pi, length = 360))),
                      yind =round(center + radius * sin(seq(0, 2*pi, length = 360))))

    for (i in seq(x.index)){

      fg <- range(xyg$yind[which(xyg$xind == xyg$xind[i])])
      grid[xyg$xind[i], fg[1]:fg[2]]<- 1
    }
    grid
    #image(grid)
  }  # end drawImage


# rasterize the point data
  popD3<-raster::rasterize(x=pop,
                   y = lcm,
                   field = "pop",
                   fun=sum,
                   na.rm=T)
  # focal
  buffer300 <- drawImage(matrix(0, 31, 31), 16, 15)  # call example
  popD3<-raster::focal(popD3, buffer300, sum,na.rm=T)
  #plot(popD3)

  #plot(gr1ha)

  gr1haPOP <- popD3
  gr1ha[is.na(gr1ha)]<-0
  gr1haPOP[gr1ha != 1 ] <- 0



  # get all blue
  blue <- lcm
 blue[blue > 2]<-NA
 blue[blue < 3]<-1
 blue[noentry>0]<-NA

bluePOP <- popD3
blue[is.na(blue)]<-0
bluePOP[blue != 1 ] <- 0


# get all natural areas
natural <- lcm
natural[natural == 1]<-12
natural[natural == 9 |
        natural == 10 |
        natural == 5 |
        natural == 6 ] <- 1
natural[(natural != 1)] <- NA
natural[noentry>0]<-NA

naturalPOP <- popD3
natural[is.na(natural)]<-0
naturalPOP[natural != 1 ] <- 0

out<- raster::stack(
  # population within 300m of 1 ha green space
  gr1haPOP,
  # population within 300m of a blue space
  bluePOP,
  # population within 300m of a natural space
  naturalPOP)

names(out)<-c("green1ha", "blue", "naturalgreen")
out

}


