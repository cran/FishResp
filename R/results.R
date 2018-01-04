#' @name results
#' @aliases results
#' @docType data
#' @title Results of Analysis: SMR, AMR and MS
#' @description A final dataset containing information about both standard and active metabolic rates, and metabolic scope obtained by using the function \code{\link{export.MR}}.
#' @usage results
#' @format A data frame with 36 rows and 18 variables:
#' \describe{
#'   \item{Chamber.No}{The number of a chamber}
#'   \item{Ind}{ID of an animal}
#'   \item{Weight}{wet weight of an animal (g)}
#'   \item{Volume}{the volume of a chamber (mL)}
#'   \item{DO.unit}{the measure unit of DO concentration}
#'   \item{SMR_Temp}{Average temperature over a period of SMR measurements (C)}
#'   \item{SMR_R2}{r2 of a slope of animal oxygen consumption without background respiration}
#'   \item{SMR_BR}{Percentage rate of background respiration}
#'   \item{SMR_MR.abs}{Absolute SMR (mgO2/h)}
#'   \item{SMR_MR.mass}{Mass-specific SMR (mgO2/kg/h)}
#'   \item{AMR_Temp}{Average temperature over a period of AMR measurements (C)}
#'   \item{AMR_R2}{r2 of a slope of animal oxygen consumption without background respiration}
#'   \item{AMR_BR}{Percentage rate of background respiration}
#'   \item{AMR_MR.abs}{Absolute AMR (mgO2/h)}
#'   \item{AMR_MR.mass}{Mass-specific AMR (mgO2/kg/h)}
#'   \item{MS.abs}{Absolute metabolic scope: the difference between absolute AMR and SMR (mgO2/h)}
#'   \item{MS.mass}{Mass-specific metabolic scope: the difference between mass-specific AMR and  SMR (mgO2/kg/h)}
#'   \item{MS.fact}{Factorial metabolic scope: the ratio between AMR and SMR}
#' }
"results"
