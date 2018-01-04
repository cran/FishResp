#' @name AMR.slope
#' @aliases AMR.slope
#' @docType data
#' @title Active Metabolic Rate: Extracted Slope(s)
#' @description A dataset containing extracted slopes for further AMR calculations and other attributes of active metabolic rate measurements obtained by using the function \code{\link{extract.slope}}
#' @usage AMR.slope
#' @format A data frame with 12 rows and 11 variables:
#' \describe{
#'   \item{Chamber.No}{The number of a chamber}
#'   \item{Ind}{ID of an animal}
#'   \item{Weight}{wet weight of an animal (g)}
#'   \item{Volume}{the volume of a chamber (mL)}
#'   \item{Date.Time}{Date and Time of a period (mm/dd/yy hh:mm:ss)}
#'   \item{Phase}{Phase and period of measurements (e.g. M1, F3)}
#'   \item{Temp}{Average temperature over a period of measurements (C)}
#'   \item{Slope.with.BR}{Slope of animal oxygen consumption with slope of\cr background respiration (mgO2/L/h)}
#'   \item{Slope}{Slope of animal oxygen consumption without background respiration (mgO2/L/h)}
#'   \item{SE}{Standard error of a slope of animal oxygen consumption without\cr background respiration (mgO2/L/h)}
#'   \item{R2}{r2 of a slope of animal oxygen consumption without background respiration}
#'   \item{DO.unit}{the measure unit of DO concentration}
#' }
"AMR.slope"
