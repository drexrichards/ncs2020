% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reclasssoilK.R
\name{reclass.soilK}
\alias{reclass.soilK}
\title{Calculation of soil K factor based on soil data}
\usage{
reclass.soilK(lcm)
}
\arguments{
\item{lcm}{Land/ water cover map, including vegetation categories. Categories must follow Gaw et al. 2019, described in data(looktbl). Must be a raster with map units in metres.}
}
\value{
Map of K values
}
\description{
This function uses Singapore-specific K values and assigns them to land cover categories
}
