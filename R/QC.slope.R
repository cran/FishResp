#' Quality Control of Slope(s)
#'
#' Graphical quality control test of extracted slopes
#' represents a visual comparison of linear regression
#' of corrected O2 concenration over time with current
#' and alternative length of measurements.
#'
#' @usage
#' QC.slope(slope.data, clean.data,
#'          chamber = c("CH1", "CH2", "CH3", "CH4",
#'                      "CH5", "CH6", "CH7", "CH8"),
#'          current = 9999, alter = 9999)
#'
#' @param slope.data  a data frame obtained by using the function
#' \code{\link{extract.slope}}
#' @param clean.data  a data frame obtained by using the function
#' \code{\link{correct.meas}}
#' @param chamber  string: the chamber chosen for the QC test
#' @param current  integer: current length of measurements for
#' slope estimation (in seconds)
#' @param alter  integer: alternative length of measurements for
#' slope estimation (in seconds)
#'
#' @importFrom chron chron times
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # if the data have been already loaded to R,
#' # skip the first five lines of the code:
#' setwd(path.package("FishResp", quiet = FALSE))
#' load("data/SMR.slope.RData")
#' load("data/SMR.clean.RData")
#' load("data/AMR.slope.RData")
#' load("data/AMR.clean.RData")
#'
#' QC.slope(SMR.slope, SMR.clean,
#'          chamber = "CH1", current = 1200, alter = 600)
#' QC.slope(SMR.slope, SMR.clean,
#'          chamber = "CH2", current = 1200, alter = 600)
#' QC.slope(SMR.slope, SMR.clean,
#'          chamber = "CH3", current = 1200, alter = 600)
#' QC.slope(SMR.slope, SMR.clean,
#'          chamber = "CH4", current = 1200, alter = 600)
#'
#' QC.slope(AMR.slope, AMR.clean,
#'          chamber = "CH1", current = 300, alter = 600)
#' QC.slope(AMR.slope, AMR.clean,
#'          chamber = "CH2", current = 300, alter = 600)
#' QC.slope(AMR.slope, AMR.clean,
#'          chamber = "CH3", current = 300, alter = 600)
#' QC.slope(AMR.slope, AMR.clean,
#'          chamber = "CH4", current = 300, alter = 600)
#'
#' @export

QC.slope <- function(slope.data, clean.data,
                     chamber = c("CH1", "CH2", "CH3", "CH4",
                                 "CH5", "CH6", "CH7", "CH8"),
                     current = 9999, alter = 9999){

  #==================================================================================================================================================#
  ### Plotting Raw Data -- subsetting those values with the minimal oxygen consumption (i.e. lowest resting metabolic rate)
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  Chamber.No <- Phase <- m1.df <- m2.df <- Time <- NULL
  chlevels<-levels(slope.data$Chamber.No)
  extracted.data<-data.frame(Date.Time=chron(), Date=chron(), Real.Time=times(), Time=integer(), Phase=factor(), Start.Meas=times(), End.Meas=times(),
                             Chamber.No=factor(), Ind=factor(), Weight=numeric(), Volume=numeric(), Init.O2=numeric(), Temp=numeric(), O2=numeric(), BOD=numeric(), O2.correct=numeric())

  for(i in 1:length(chlevels)){
    meas<-as.character(subset(slope.data, Chamber.No==chlevels[i])$Phase)
    chlevels.df<-subset(clean.data, Chamber.No==chlevels[i])
    for (m in 1:length(meas)){
      out.df<-subset(chlevels.df, Phase==meas[m])
      row.names(out.df)<-NULL
      extracted.data<-rbind(extracted.data, out.df)
    }
  }

  rm(chlevels)
  rm(i)
  rm(meas)
  rm(chlevels.df)
  rm(m)
  rm(out.df)

  a <-length(slope.data$Chamber.No[slope.data$Chamber.No == "CH1"])

  if (a <= 3){
    par(mfrow = c(a, 1))
  }
  else if(a == 4){
    dev.new()
    par(mfrow = c(2, 2))
  }
  else if(a > 4 && a <= 6){
    dev.new()
    par(mfrow = c(3, 2))
  }
  else{
    dev.new()
    par(mfrow = c(3, 2), ask = T)
  }

  rm(a)
  chamber.df<-subset(extracted.data, Chamber.No == chamber)
  meas<-unique(as.character(chamber.df$Phase))


  for(m in 1:length(meas))
  {m.df<-subset(chamber.df, Phase==meas[m])
  m1.df<-subset(m.df, Time<=current)
  m2.df<-subset(m.df, Time<=alter)
  model.1<-lm(O2.correct~Time, data=m1.df)
  model.2<-lm(O2.correct~Time, data=m2.df)
  plot(m.df$O2.correct~m.df$Time, main=meas[m], las=1,
       xlab = "Time (s)", ylab = "DO (mgO2/L)")

  abline(coef(model.1)[1], coef(model.1)[2], col="red", lwd=3)
  abline(coef(model.2)[1], coef(model.2)[2], col="green", lwd=3, lty=2)
  l.1<-paste("now: r^2=",(round(summary(model.1)$r.sq, digits=2)),"; slope= ",(round(coef(model.1)[2], digits=5)),sep="")
  l.2<-paste("alter: r^2=",(round(summary(model.2)$r.sq, digits=2)),"; slope= ",(round(coef(model.2)[2], digits=5)),sep="")
  legend("topright", legend=c(l.1,l.2))
  rm(model.1)
  rm(l.1)
  }
  rm(meas)
  rm(chamber.df)
  rm(m)
  rm(m.df)
  rm(m1.df)
  rm(m2.df)
}

