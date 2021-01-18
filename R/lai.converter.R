#' Conversion of partial LAI to full LAI
#'
#' This function does a simple conversion to estimate full LAI from a map of LAI above a certain point
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl)
#' @param lai Leaf area index - raster grid with cell sizes in metre units
#' @param vegheight Vegetation height in metres
#' @param instrumentheight Height in metres of instrument used to measure LAI
#' @param underlai Arbitrary LAI to assign to areas below instrument for turf and shrub locations
#' @return One raster of full LAI
#' @export

lai.converter <- function(lcm, lai, vegheight, instrumentheight, underlai){


  # This model uses the old land cover map classes so needs correction using reclassify
lcm<- raster::reclassify(lcm, cbind(ncs2020::looktbl$gaw.2019.code, ncs2020::looktbl$old.code))


# if there is no vegheight data, assume 30 m
vegheight[is.na(vegheight)] <- 30
# also assume a max tree height of 60m - cut off anything taller than that
vegheight[vegheight > 60] <- 60

# ok so fill vegheight with the right data
underint <- vegheight
underint[,]<- 0

# extract the data for forest only and convert
# get veg height
underint[lcm == 9] <- vegheight[lcm==9]

#underint <-  raster::mask(underint, lcm==9, updatevalue = vegheight,maskvalue=0)
# convert to the proportion that 2.5 is
underint <- instrumentheight/underint
underint[underint > 1] <- 0
# so now we know how much % is understorey, can calculate the total
underint <- ((underint*0.9555)+-0.0138) # this is the proportion of the leaf density/ therefore by inference also the proportion of LAI
# logic checks
underint[underint < 0] <- 0
underint[underint >1] <- 1
# make it artificially so that cannot be more than 50% of lai - we have some errors in the hieght dataset
underint[underint >0.5] <- 0.5
#this gets the 100% lai and then multiply by underint to get the underint lai
underint <- (lai/ (1-underint))*underint
# remove non-forest again
underint[lcm != 9] <- 0

# ok clean up lai so that it does not include urban or water
lai[is.na(lcm)]<-NA
lai[lcm == 1]<- 0
lai[lcm == 2]<- 0
lai[lcm == 3]<- 0
lai[lcm == 4]<- 0

# for grass, make it 2 minimum
lai[lcm == 10 & lai < underlai] <- underlai
lai[lcm == 8 & lai < underlai] <- underlai

# add the canopy < 2m lai
lai <- lai + underint

  # output
 lai
}

