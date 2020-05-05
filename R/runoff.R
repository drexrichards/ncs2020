#' Modelling of runoff absorbtion by vegetation
#'
#' This function models runoff reduction by vegetation
#' @param lcm Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl)
#' @param rainfallin Rainfall in mm
#' @return Two rasters in a list, the first is proportion of runoff absorbed. Second is runoff in mm
#' @export
runoff.model <- function(lcm, rainfallin){
  # make a new raster of curve numbers
  cnras <- raster::reclassify(lcm, data.frame(ncs2020::looktbl$gaw.2019.code, ncs2020::looktbl$cn))

  # estimate runoff for a given rainfall event
  # lets assume a 110 mm hourly rainfall event
  # this is a big storm in Winston Chow's Singapore flooding paper
  # 15 inches = 381 mm. so 1 is 25.4
  # 50 mm is 1.96 inches

  rain <- 0.03937008 * rainfallin

  # see if it matches first
  if(rain %in% runoffcurve$Rainfall){
    nrunoff <- runoffcurve[which(runoffcurve$Rainfall == rain),]

  } else {

    # first find the inch intervals we are between
    runoffcurve[c(max(which(runoffcurve$Rainfall < rain)),
                  min(which(runoffcurve$Rainfall > rain))),]

    cf <- runoffcurve$Rainfall[c(max(which(runoffcurve$Rainfall < rain)),
                                 min(which(runoffcurve$Rainfall > rain)))]
    cf<- c(max(cf)-min(cf) ,
           (max(cf)-min(cf)) - (max(cf)-rain))
    cf <- cf[2]/cf[1]

    # convert all cn data
    nrunoff <- runoffcurve[c(max(which(runoffcurve$Rainfall < rain)),
                             min(which(runoffcurve$Rainfall > rain))),][1,] +
      (runoffcurve[c(max(which(runoffcurve$Rainfall < rain)),
                     min(which(runoffcurve$Rainfall > rain))),][2,] -
         runoffcurve[c(max(which(runoffcurve$Rainfall < rain)),
                       min(which(runoffcurve$Rainfall > rain))),][1,]) * cf
    # plot to check it looks nice
    #plot(as.numeric(nrunoff[2:length(nrunoff)]),
    #     as.numeric(substr(names(runoffcurve)[2:length(nrunoff)],2,3))
    #    ,type="l" )
  }

  # take out and add a new row for the CN
  nrunoff <- nrunoff[2:length(nrunoff)]
  nrunoff[2,] <-  as.numeric(substr(names(nrunoff), 2,3))

  # make lookuptbl
  looktbl2 <- ncs2020::looktbl
  looktbl2$runoffj <- NA

  # we don't have date for cn less than 40. convert to this as min for now
  looktbl2$cn[looktbl2$cn < 40] <- 40

  # run for each row in the lookuptable
  for(i in 1:length(looktbl2[,1])){
    # ignore if CN is NA
    if (is.na(looktbl2$cn[i])){

    } else

      # find correct cn insert values
      if (looktbl2$cn[i] %in% nrunoff[2,]){
        looktbl2$runoffj[i] <-  nrunoff[1 ,which(nrunoff[2,] == looktbl2$cn[i] )]
      } else

        # or find correct cn and interpolate between
      {
        cf <- nrunoff[2,][c(max(which(nrunoff[2,] < looktbl2$cn[i])),
                            min(which(nrunoff[2,] > looktbl2$cn[i])))]
        cf<- c(max(cf)-min(cf) ,
               (max(cf)-min(cf)) - (max(cf)- looktbl2$cn[i]))
        cf <- cf[2]/cf[1]

        # convert all cn data
        looktbl2$runoffj[i] <-     nrunoff[1,][c(max(which(nrunoff[2,] < looktbl2$cn[i])),
                                                min(which(nrunoff[2,] > looktbl2$cn[i])))][2]* cf
      }


  }

  # tidying
  looktbl2$runoffj <- unlist(looktbl2$runoffj )

  # convert from inches! to mm
  looktbl2$runoffj <- looktbl2$runoffj * 25.4


  # map runoff using a lookup table
  runrasmm <- raster::reclassify(cnras, data.frame(looktbl2$cn, looktbl2$runoffj))

  # convert runras into % of rainfall for comparability
  runras <- (rainfallin-runrasmm)/rainfallin

  # return outputs
list(runras, runrasmm)
}



