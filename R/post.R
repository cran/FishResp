#' @name post
#' @aliases post
#' @docType data
#' @title Post Raw Data
#' @description A dataset containing raw data of a background test conducted before metabolic rate measurements (post-test), obtained by using the function \code{\link{import.test}}.
#' @usage post
#' @format A data frame with 4800 rows and 7 variables:
#' \describe{
#'   \item{Chamber.No}{The number of a chamber}
#'   \item{Test}{A constant string "test"}
#'   \item{Time}{Ordinal number of seconds in each period of measurements (1-600)}
#'   \item{Init.O2}{Initial level of dissolved oxygen (mgO2/L)}
#'   \item{Temp}{Temperature at each second (C)}
#'   \item{O2}{Actual level of dissolved oxygen at each second (mgO2/L)}
#'   \item{delta.O2}{The difference between Actual and Initial O2}
#' }
"post"