#' Calculation of Metabolic Rate
#'
#' The function is used to calculate and plot background
#' respiration, absolute and mass-specific metabolic rates.
#'
#' @usage
#' calculate.MR(slope.data, density = 1000,
#'              plot.BR = TRUE,
#'              plot.MR.abs = TRUE,
#'              plot.MR.mass = TRUE)
#'
#' @param slope.data  a data frame obtained by using the function
#' \code{\link{extract.slope}}
#' @param density  numeric: the density of an animal body (kg/m^3)
#' @param plot.BR  logical: if TRUE, the graph of background
#' respiration rate is plotted
#' @param plot.MR.abs  logical: if TRUE, the graph of absolute
#' metabolic rate is plotted
#' @param plot.MR.mass  logical: if TRUE, the graph of mass-specific
#' metabolic rate is plotted
#'
#' @return The function returns a data frame with calculated
#' background respiration, absolute and mass-specific metabolic
#' rates. The data frame might is used in the function
#' \code{\link{export.MR}}.
#'
#' @importFrom lattice xyplot
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first three lines of the code:
#' setwd(path.package("FishResp", quiet = FALSE))
#' load("data/SMR.slope.RData")
#' load("data/AMR.slope.RData")
#'
#' SMR <- calculate.MR(SMR.slope,
#'                     density = 1000,
#'                     plot.BR = TRUE,
#'                     plot.MR.abs = TRUE,
#'                     plot.MR.mass = TRUE)
#'
#' AMR <- calculate.MR(AMR.slope,
#'                     density = 1000,
#'                     plot.BR = TRUE,
#'                     plot.MR.abs = TRUE,
#'                     plot.MR.mass = TRUE)
#'
#' @export

calculate.MR  <- function(slope.data, density = 1000, plot.BR = TRUE,
                          plot.MR.abs = TRUE, plot.MR.mass = TRUE){
                          V = slope.data$Volume - (slope.data$Weight/density/1000)
                          BW = slope.data$Weight/1000
                          # is 1080 the average density of fish body?

  slope.data$MR.abs.with.BR = -(slope.data$Slope.with.BR*V)

  slope.data$BR = (slope.data$Slope.with.BR - slope.data$Slope)/slope.data$Slope.with.BR*100
  slope.data$MR.abs = -(slope.data$Slope*V)
  slope.data$MR.mass = -(slope.data$Slope*V/BW)

  a <- xyplot(BR~Temp|Ind, data=slope.data, as.table = T,
              xlab = "Temperature (C)", ylab = "Background respiration (%)",
              main = "Percentage rate of background respiration")
  b <- xyplot(MR.abs~Temp|Ind, data=slope.data, as.table = T,
              xlab = "Temperature (C)", ylab = "Absolute MR (mgO2/h)",
              main = "Absolute metabolic rate")
  d <- xyplot(MR.mass~Temp|Ind, data=slope.data, as.table = T,
              xlab = "Temperature (C)", ylab = "Mass-specific MR (mgO2/kg/h)",
              main = "Mass-specific metabolic rate")

  if (plot.BR == TRUE){
    par(mfrow = c(2, 1), ask = T)
    print(a)
  }

  if (plot.MR.abs == TRUE){
    par(mfrow = c(2, 1), ask = T)
    print(b)
  }

  if (plot.MR.mass == TRUE){
    par(mfrow = c(2, 1), ask = T)
    print(d)
  }

  MR.data <- slope.data
  return(MR.data)
}
