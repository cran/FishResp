#' Extraction of Slope(s)
#'
#' The function extracts the slopes of the linear regression
#' of corrected O2 concenration over time with defined parameters
#' (see Arguments).
#'
#' @usage
#' extract.slope(clean.data, method = c("min", "max"),
#'               n.slope=1000, r2=0.9, length = 9999,
#'               plot.data = TRUE)
#'
#' @param clean.data  a data frame obtained by using the function
#' \code{\link{correct.meas}}
#' @param method  string: the method of extracting slopes: minimal
#' or maximal values
#' @param n.slope  integer: the number of extracted slopes (only
#' one slope is calculated for each measurement period)
#' @param r2  numeric: minimal coeficient of determination (r2)
#' for extracted slopes. Coeficient of determination is used as
#' a threshold of quality to be determined by the user.
#' @param length  integer: length of a measurement period for slope
#' calculations (in seconds)
#' @param plot.data logical: if TRUE, then a graph with raw data
#' for extracted slopes is plotted
#'
#' @return The function returns a data frame with the information
#' about extracted slopes. The data frame is used in the functions
#' \code{\link{QC.slope}} and \code{\link{calculate.MR}}.
#'
#' @importFrom lattice xyplot
#' @importFrom chron chron times
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first three lines of the code:
#' setwd(path.package("FishResp", quiet = FALSE))
#' load("data/SMR.clean.RData")
#' load("data/AMR.clean.RData")
#'
#' SMR.slope <- extract.slope(SMR.clean,
#'                            method = "min",
#'                            n.slope = 3,
#'                            r2=0.95,
#'                            length = 1200,
#'                            plot.data = TRUE)
#'
#' AMR.slope <- extract.slope(AMR.clean,
#'                            method = "max",
#'                            n.slope = 3,
#'                            r2=0.8,
#'                            length = 300,
#'                            plot.data = TRUE)
#'
#' @export

extract.slope <- function(clean.data, method = c("min", "max"), n.slope=1000, r2=0.9, length = 9999, plot.data = TRUE){
  Chamber.No <- Phase <- R2 <- NULL
  MR.est.all<-data.frame(Chamber.No=factor(), Ind=factor(), Weight=numeric(), Volume=numeric(), Date.Time = chron(), Phase=factor(),
                         Temp=numeric(), Slope.with.BR=numeric(), Slope=numeric(), SE=numeric(), R2=numeric())

  chamber<-levels(clean.data$Chamber.No)
  meas<-levels(clean.data$Phase)

  for(i in 1:length(chamber)){
    chamber.df<-subset(clean.data, Chamber.No==chamber[i])
    for(m in 1:length(meas)){
      m.df<-subset(chamber.df, Phase==meas[m])
      m.df<-subset(m.df, m.df$Time<=length)
      model.with.BR <- lm(O2~Time, data=m.df)
      model<-lm(O2.correct~Time, data=m.df)
      out.df<-data.frame(Chamber.No=head(m.df, 1)$Chamber.No, Ind=head(m.df, 1)$Ind, Weight=head(m.df, 1)$Weight, Volume=head(m.df, 1)$Volume,
                         Date.Time = tail(m.df, 1)$Date.Time, Phase=head(m.df, 1)$Phase, Temp=mean(m.df$Temp), Slope.with.BR = coef(model.with.BR)[2],
                         Slope=coef(model)[2],  SE=summary(model)$coef[4], R2=summary(model)$r.squared)
      row.names(out.df)<-NULL
      MR.est.all<-rbind(MR.est.all, out.df)
    }
  }
  head(MR.est.all)
  rm(chamber)
  rm(meas)
  rm(i)
  rm(chamber.df)
  rm(m)
  rm(m.df)
  rm(model)
  rm(out.df)

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  MR.est<-data.frame(Chamber.No=factor(), Ind=factor(), Weight=numeric(), Volume=numeric(), Phase=factor(), Temp=numeric(),
                     Slope.with.BR=numeric(), Slope=numeric(), SE=numeric(), R2=numeric())

  # I've tweaked the code slightly here to ensure that we're only looking at data which fit a model of linear O2 consumption
  MR.temporal<-subset(MR.est.all, R2>=r2)
  chamber<-levels(MR.temporal$Chamber.No)
  for(i in 1:length(chamber)){
    chamber.df<-subset(MR.temporal, Chamber.No==chamber[i])
    if (method == "min"){
      s<-head(order(chamber.df$Slope, decreasing=T),n.slope)
    }
    else if (method == "max"){
      s<-head(order(chamber.df$Slope, decreasing=F),n.slope)
    }
    out.df<-chamber.df[s,]
    rm(s)
    row.names(out.df)<-NULL
    MR.est<-rbind(MR.est, out.df)
  }

  rm(chamber)
  rm(i)
  rm(chamber.df)
  rm(out.df)
  rm(MR.temporal)

  #==================================================================================================================================================#
  ### Plotting Raw Data -- subsetting those values with the minimal oxygen consumption (i.e. lowest resting metabolic rate)
  #--------------------------------------------------------------------------------------------------------------------------------------------------#

  if (plot.data == T){
    chamber<-levels(MR.est$Chamber.No)
    extracted.data<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                               Chamber.No=factor(), Ind=factor(), Weight=numeric(), Volume=numeric(), Init.O2=numeric(), Temp=numeric(), O2=numeric(), BOD=numeric(), O2.correct=numeric())

    for(i in 1:length(chamber)){
      meas<-as.character(subset(MR.est, Chamber.No==chamber[i])$Phase)
      chamber.df<-subset(clean.data, Chamber.No==chamber[i])
      for (m in 1:length(meas)){
        out.df<-subset(chamber.df, Phase==meas[m])
        row.names(out.df)<-NULL
        extracted.data<-rbind(extracted.data, out.df)
      }
    }

    a<-xyplot(O2.correct~Time|Chamber.No, data=extracted.data,
              xlab = "Time (s)", ylab = "DO (mgO2/L)", as.table = T)
    print(a)

    rm(chamber)
    rm(i)
    rm(meas)
    rm(chamber.df)
    rm(m)
    rm(out.df)
  }

  slope.data <- MR.est
  slope.data$Volume <- as.numeric(as.character(slope.data$Volume))
  slope.data$Weight <- as.numeric(as.character(slope.data$Weight))
  return(slope.data)
}
