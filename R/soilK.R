#' Calculation of soil K factor based on soil data
#'
#' This function estimates a soil K factor for a given soil condition, taking into account the organic matter content, clay content, and particle size distribution of the soil.
#' @param OM Percentage organic matter
#' @param Cm percentage clay by mass
#' @param Dg Geometric mean of the particle size distribution
#' @return K values
#' @export

soilK.model <- function(OM, C, Dg){
  # use Torri et al 1997
  # K = 0.0293(0.65-Dg + 0.24Dg^2) *
  #(exp(-0.0021 * OM/C * -0.0037 * (OM/C)^2 - 4.02 *C + 1.72C^2)
  # OM is percentage organic matter
  # Cm is percentage clay by mass
  # Dg is geometric mean of the particle size distribution
  # Dg = sum(fi * ln(sqrt(di * di-1)))

  K <- 0.0293 * (0.65 - Dg + (0.24 * (Dg^2))) *
    exp(-0.0021 * (OM/Cm) -0.00037 * (OM/Cm)^2 * - 4.02 + (1.72*Cm^2))
  # Ks are well within the range mentioned in  Wischmeier W, Smith D. 1978

K
}

