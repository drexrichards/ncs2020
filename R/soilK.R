#' Calculation of soil K factor based on soil data
#'
#' This function estimates a soil K factor for a given soil condition, taking into account the particle size distribution of the soil.
#' @param Dg Geometric mean of the particle size distribution
#' @return K values
#' @export

soilK.model <- function(Dg){

  # NEW METHOD from Renard et al 1997, uses only Dg
  K = 7.594 *
    ((0.0034 + 0.0405) * (exp(
      (-1/2 *
         (((log(Dg) + 1.659)/0.7101))^2)
    )))

  # requires conversion to SI units
  K<-K*0.1317
K
}

