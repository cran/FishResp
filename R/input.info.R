#' Input the Information about Individuals and Chambers
#'
#' The function is used to input manually the information required for
#' metabolic rate calculations: ID and wet weight of individuals, volume
#' of chambers. Values of those parameters should be filled in the same
#' order in a vector format replacing default NA values in the template.
#'
#' @usage
#' input.info(ID = c(NA, NA, NA, NA, NA, NA, NA, NA),
#'            Weight = c(NA, NA, NA, NA, NA, NA, NA, NA),
#'            Volume = c(NA, NA, NA, NA, NA, NA, NA, NA))
#'
#' @details
#' It is especially important to keep such format of vectors when not
#' the full number of individuals is in a multi-channel respirometry
#' system. E.g.: if you use a 4-channel respirometry system with
#' three fish and only Chamber 1 is empty, but data are still collected
#' from there, do not remove NA values for that chamber to prevent
#' the shift of actual data between the chambers.
#'
#' @param ID  string: ID of fish or another aquatic organism
#' @param Weight  numeric: wet weight of an individual in grams (g)
#' @param Volume  numeric: the volume of chambers in milliliters (mL)
#'
#' @return The function returns a data frame with three columns:
#' "ID", "Weight", "Volume".
#'
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # Four sticklebacks in a 4-channel respirometry system
#' info <- input.info(ID = c("Stickleback_1", "Stickleback_2",
#'                           "Stickleback_3", "Stickleback_4"),
#'                    Weight = c(1.63, 2.31, 3.51, 1.76),
#'                    Volume = c(250, 250, 250, 250))
#'
#' @export

input.info <- function(ID = c(NA, NA, NA, NA, NA, NA, NA, NA),
                 Weight = c(NA, NA, NA, NA, NA, NA, NA, NA),
                 Volume = c(NA, NA, NA, NA, NA, NA, NA, NA)){

  info.data <- as.data.frame(cbind(ID, Weight, Volume))
  info.data$Weight <- as.numeric(as.character(info.data$Weight))
  info.data$Volume <- as.numeric(as.character(info.data$Volume))
  print(info.data)
  return(info.data)
}
