% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post.R
\docType{data}
\name{post}
\alias{post}
\title{Post Raw Data}
\format{A data frame with 2400 rows and 7 variables:
\describe{
  \item{Chamber.No}{the number of a chamber}
  \item{Test}{a constant string "test"}
  \item{Time}{ordinal number of seconds in each measurement phase (1-600)}
  \item{Init.O2}{initial level of dissolved oxygen (mgO2/L)}
  \item{Temp}{temperature at each second (\eqn{C^{o}})}
  \item{O2}{actual level of dissolved oxygen at each second (mgO2/L)}
  \item{delta.O2}{the difference between actual and initial \eqn{O_{2}}}
}}
\usage{
post
}
\description{
A dataset contains raw data of a background test conducted before metabolic rate measurements (post-test), obtained by using the function \code{\link{import.test}}.
}
\keyword{datasets}
