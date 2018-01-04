#' @name AMR.raw
#' @aliases AMR.raw
#' @docType data
#' @title Active Metabolic Rate: Raw Data
#' @description The dataset containing raw data of active metabolic rate measurements obtained by using the function \code{\link{import.meas}})
#' @usage AMR.raw
#' @format A data frame with 1800 rows and 16 variables:
#' \describe{
#'   \item{Date.Time}{Date and Time (mm/dd/yy hh:mm:ss)}
#'   \item{Phase}{Phase and period of measurements (e.g. M1, F3)}
#'   \item{Temp.1}{Temperature at each second (C)}
#'   \item{Ox.1}{Actual level of dissolved oxygen at each second (mgO2/L)}
#'   \item{Real.Time}{Time (hh:mm:ss)}
#'   \item{Date}{Time (mm/dd/yy)}
#'   \item{Time}{Ordinal number of seconds in each period of measurements (1-600)}
#'   \item{Start.Meas}{The first second of a measurement period (hh:mm:ss)}
#'   \item{End.Meas}{The last second of a measurement period (hh:mm:ss)}
#'   \item{Total.Phases}{The total number of measurement periods (constant value)}
#'   \item{Ox.2}{see Ox.1}
#'   \item{Ox.3}{see Ox.1}
#'   \item{Ox.4}{see Ox.1}
#'   \item{Temp.2}{see Temp.1}
#'   \item{Temp.3}{see Temp.1}
#'   \item{Temp.4}{see Temp.1}
#' }
"AMR.raw"