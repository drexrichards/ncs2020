#' Modelling of public access to green space and natural ecosystems and open water - Totals
#'
#' This function models public access to green space, natural ecosystems, and open water and gives the total population with access to each.
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.
#' @param noentry Raster indicating areas of no entry with values of 1. Function can also be used without this value, defaults to no inaccessible space in the study area.
#' @param pop Population given as a SpatialPointsDataFrame with one field for population, which is named "pop"
#' @return A stack of three raster maps indicating the population pressure (number of people) with access to each pixel of (1) accessible green space in a patch larger than 1 ha, (2) blue space, (3) natural space.
#' @export


access.model.total <- function (lcm, noentry = NULL, pop)
{
  sum2 <- function(x) {
    sum(x, na.rm = T)
  }
  drawImage <- function(mat, center, radius) {
    grid <- mat
    x.index <- round(center + radius * cos(seq(0, 2 * pi,
                                               length = 360)))
    y.index <- round(center + radius * sin(seq(0, 2 * pi,
                                               length = 360)))
    xyg <- data.frame(xind = round(center + radius * cos(seq(0,
                                                             2 * pi, length = 360))), yind = round(center + radius *
                                                                                                     sin(seq(0, 2 * pi, length = 360))))
    for (i in seq(x.index)) {
      fg <- range(xyg$yind[which(xyg$xind == xyg$xind[i])])
      grid[xyg$xind[i], fg[1]:fg[2]] <- 1
    }
    grid
  }

  buffer300 <- drawImage(matrix(0, 31, 31), 16, 15)

  lcm <- raster::reclassify(lcm, cbind(ncs2020::looktbl$gaw.2019.code,
                                       ncs2020::looktbl$old.code))
  if (is.null(noentry)) {
    noentry <- lcm
    noentry[, ] <- 0
  }
  gr1ha <- lcm
  gr1ha[gr1ha == 1] <- 12
  gr1ha[gr1ha == 9 | gr1ha == 10 | gr1ha == 7 | gr1ha == 8 |
          gr1ha == 5 | gr1ha == 6] <- 1
  gr1ha[(gr1ha != 1)] <- NA
  gr1ha[noentry > 0] <- NA
  gr1ha <- raster::clump(gr1ha, directions = 4)
  grpc <- table(raster::getValues(gr1ha))
  10000/(raster::res(gr1ha)[1] * raster::res(gr1ha)[2])
  grpc <- as.numeric(names(grpc)[which(grpc > floor(10000/(raster::res(gr1ha)[1] *
                                                             raster::res(gr1ha)[2])))])
  gr1ha[is.na(raster::match(gr1ha, grpc))] <- NA
  gr1ha[!is.na(gr1ha)] <- 1


  # blue
  blue <- lcm
  blue[blue > 2] <- NA
  blue[blue < 3] <- 1
  blue[noentry > 0] <- NA
  blue[is.na(blue)] <- 0


  # natural
  natural <- lcm
  natural[natural == 1] <- 12
  natural[natural == 9 | natural == 10 | natural == 5 | natural ==
            6] <- 1
  natural[(natural != 1)] <- NA
  natural[noentry > 0] <- NA
  natural <- raster::clump(natural, directions = 4)
  grpc <- table(raster::getValues(natural))
  10000/(raster::res(natural)[1] * raster::res(natural)[2])
  grpc <- as.numeric(names(grpc)[which(grpc > floor(10000/(raster::res(natural)[1] *
                                                             raster::res(natural)[2])))])
  natural[is.na(raster::match(natural, grpc))] <- NA
  natural[!is.na(natural)] <- 1

  # buffer all to 300 and then simplify and stack
  gr1ha <- raster::focal(gr1ha, buffer300, sum, na.rm = T)
  blue<- raster::focal(blue, buffer300, sum, na.rm = T)
  natural <- raster::focal(natural, buffer300, sum, na.rm = T)

  os <- raster::stack(gr1ha > 0,
                      blue > 0,
                      natural >0)

  # extract from buffers
  spoint <- sp::SpatialPoints(coords=sp::coordinates(pop), proj4string = raster::crs(pop))

  out <- raster::extract(os, spoint)
  out[,1]<-out[,1] * pop$pop
  out[,2]<-out[,2] * pop$pop
  out[,3]<-out[,3] * pop$pop

  out<- apply(out,2,sum,na.rm=T)
  out<-c(out, sum(pop$pop, na.rm=T))
  names(out)<- c("green", "blue", "natural","totalpop")
  out
}
