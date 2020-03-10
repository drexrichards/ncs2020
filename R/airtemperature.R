#' Modelling of reduction in air temperature caused by ecosystems
#'
#' This function models reduction in air temperature caused by ecosystems, in comparison to urban land cover
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl)
#' @param lai Leaf area index - raster grid with cell sizes in metre units
#' @return One raster indicating average reduction in temperature at 15:00
#' @export

air.temp.model <- function(lcm, lai){


  # This model uses the old land cover map classes so needs correction using reclassify
lcm<- raster::reclassify(lcm, cbind(looktbl$gaw.2019.code, looktbl$old.code))



  # add a function for the buffering
  # this function draws a circle in a matrix and you can set the radius
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

  # create buffer matrix - approximately a 50m buffer
  buffer50 <- drawImage(matrix(0, 11, 11), 6, 5)  # call example
  #image(buffer50)

  buffer100 <- drawImage(matrix(0, 21, 21), 11, 10)  # call example
  #image(buffer100)

  buffer200 <- drawImage(matrix(0, 41, 41), 21, 20)  # call example
  #image(buffer200)

  # this sets up a new raster stack with single layers for the different vegetation types
  lcmempty <- lcm
  lcmempty[,]<-0
  lcmdisag <- stack(lcmempty,lcmempty,lcmempty,lcmempty,lcmempty)
  lcmdisag[[1]][lcm == 5| lcm == 6] <- 1 # wetland and mangrove
  lcmdisag[[2]][lcm == 9| lcm == 10] <- 1 # unmanaged
  lcmdisag[[3]][lcm == 7] <- 1 # managed
  lcmdisag[[4]][lcm == 8] <- 1 # grass
  lcmdisag[[5]][lcm == 1| lcm == 2] <- 1 # water

  # this applies the buffer (focal stats) across each layer
  # to calculate the % cover of the veg types, and the mean LAI
  lcmdisag[[1]] <- raster::focal(lcmdisag[[1]], buffer200, mean)
  lcmdisag[[2]] <- raster::focal(lcmdisag[[2]], buffer200, mean)
  lcmdisag[[3]] <- raster::focal(lcmdisag[[3]], buffer50, mean)
  #lcmdisag[[4]] <- focal(lcmdisag[[4]], buffer100, mean)
  lcmdisag[[5]] <- raster::focal(lcmdisag[[5]], buffer100, mean)

  # add lai
  lai <- raster::focal(lai, buffer100, mean,na.rm=T)

  lcmdisag <- raster::stack(lcmdisag, lai)
  names(lcmdisag) <- c("wetland200", "unmanaged200", "managed50", "grass100", "water0", "lai100")
  # plot(lcmdisag)

  lcmdisag$unmanaged200T <- lcmdisag$unmanaged200 + lcmdisag$wetland200

  # check that the values do not exceed the ranges from the data

  lcmdisag$unmanaged200T[lcmdisag$unmanaged200T < 0 ]<- 0
  lcmdisag$unmanaged200T[lcmdisag$unmanaged200T > 0.479 ]<- 0.479
  lcmdisag$managed50[lcmdisag$managed50 < 0] <- 0
  lcmdisag$managed50[lcmdisag$managed50 > 0.545] <- 0.545
  #lcmdisag$grass100 range is 0 to 1
  #lcmdisag$lai100 range is 0 to 1

  # make prediction based on equation from gam modelling
  # predict over map
  #tempmap <-  28.2244 + #intercept
  #  ((lcmdisag$unmanaged + lcmdisag$wetland)* -2.9640) +
  #  (lcmdisag$managed * -2.6166) +
  #  (lcmdisag$grass * -1.4095) +
  #  (lcmdisag$lai * 0.6032)


  tempmap <-  28.4834+ #intercept
    ((lcmdisag$unmanaged200T)* -2.4011) +
    (lcmdisag$managed50 * -1.4469) +
    (lcmdisag$grass100 * -0.9781) +
    (lcmdisag$lai100 * -0.8995) +
    1.850823

  tempdif <-   #intercept
    ((lcmdisag$unmanaged200T)* -2.4011) +
    (lcmdisag$managed50 * -1.4469) +
    (lcmdisag$grass100 * -0.9781) +
    (lcmdisag$lai100 * -0.8995)

  tempmap[is.na(lcm )] <- NA
  tempmap[lcm  == 1 | lcm  == 2] <- NA


  tempdif[is.na(lcm )] <- NA
  tempdif[lcm  == 1 | lcm  == 2] <- NA

 # convert to non-negative
  tempdif <- 0-tempdif

  # output
  tempdif
}

