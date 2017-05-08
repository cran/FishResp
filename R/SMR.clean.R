#' @name SMR.clean
#' @aliases SMR.clean
#' @docType data
#' @title Standard Metabolic Rate: Corrected Raw Data
#' @description
#' A dataset containing raw data of standard metabolic rate
#' measurements corrected for background respiration using
#' the function \code{\link{correct.meas}}
#' @usage SMR.clean
#' @format A data frame with 115200 rows and 16 variables:
#' \describe{
#'   \item{Date.Time}{Date and Time (mm/dd/yy hh:mm:ss)}
#'   \item{Date}{Time (mm/dd/yy)}
#'   \item{Real.Time}{Time (hh:mm:ss)}
#'   \item{Time}{Ordinal number of seconds in each period of measurements (1-1200)}
#'   \item{Phase}{Phase and period of measurements (e.g. M1, F3)}
#'   \item{Start.Meas}{The first second of a measurement period (hh:mm:ss)}
#'   \item{End.Meas}{The last second of a measurement period (hh:mm:ss)}
#'   \item{Chamber.No}{The number of a chamber}
#'   \item{Ind}{ID of an animal}
#'   \item{Weight}{wet weight of an animal (g)}
#'   \item{Volume}{the volume of a chamber (mL)}
#'   \item{Init.O2}{Initial level of dissolved oxygen (mgO2/L)}
#'   \item{Temp}{Temperature at each second (C)}
#'   \item{O2}{Actual level of dissolved oxygen at each second (mgO2/L)}
#'   \item{BR}{Slope of background respiration (mgO2/L/h)}
#'   \item{O2.correct}{Actual level of dissolved oxygen at each second
#'   corrected by slope of background respiration (mgO2/L)}
#' }
"SMR.clean"
