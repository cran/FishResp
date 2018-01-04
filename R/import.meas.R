#' Import Raw Data of Metabolic Rate Measurements
#'
#' The function is used to import raw data of metabolic rate measurements to R environment.
#'
#' @usage
#' import.meas(file, info.data,
#'             n.chamber = c(1,2,3,4,5,6,7,8),
#'             logger = c("AutoResp", "FishResp"),
#'             date.format = c("DMY", "MDY", "YMD"),
#'             start.measure = "00:00:00",
#'             stop.measure = "23:59:59",
#'             plot.temperature = TRUE,
#'             plot.oxygen = TRUE)
#'
#' @param file  the name of a file  which raw data of metabolic rate measurements are to be read from
#' @param info.data  a data frame obtained by using the function \code{\link{input.info}}
#' @param n.chamber  integer: the number of chambers used in an experiment (including empty ones)
#' @param logger  string: the name of a logger software used for intermittent-flow respirometry
#' \itemize{
#'   \item 'AutoResp' if you use commercial software of 'Loligo Systems'
#'   \item 'FishResp' if you do not use the above-mentioned software, standardize data to the format described below (see Details in the\cr function \code{\link{import.test}})
#' }
#' @param date.format  string: date format (DMY, MDY or YMD)
#' @param start.measure  chron: time when metabolic rate measurements are started
#' @param stop.measure  chron: time when metabolic rate measurements are finished
#' @param plot.temperature  logical: if TRUE then the graph of raw temperature data is plotted
#' @param plot.oxygen  logical: if TRUE then the graph of raw oxygen data is plotted
#'
#' @return The function returns a data frame containing standardized raw data of metabolic rate measurements. The data frame should be used in the function \code{\link{correct.meas}} to correct metabolic rate measurements for background respiration.
#'
#' @importFrom chron chron
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # Import raw data for standard and active metabolic
#' # rate measurements (SMR and AMR, respectively)
#'
#' # if the data have been already loaded to R,
#' # skip the first line of the code:
#' data(info)
#' \dontrun{
#' SMR.path = system.file("extdata/stickleback/SMR_raw.txt.xz", package = "FishResp")
#' SMR.raw <- import.meas(file = SMR.path,
#'                        info.data = info,
#'                        logger = "AutoResp",
#'                        n.chamber = 4,
#'                        date.format = "DMY",
#'                        start.measure = "20:00:00",
#'                        stop.measure = "08:00:00",
#'                        plot.temperature = TRUE,
#'                        plot.oxygen = TRUE)
#' }
#' AMR.path = system.file("extdata/stickleback/AMR_raw.txt.xz", package = "FishResp")
#' AMR.raw <- import.meas(file = AMR.path,
#'                        info.data = info,
#'                        logger = "AutoResp",
#'                        n.chamber = 4,
#'                        date.format = "DMY",
#'                        plot.temperature = TRUE,
#'                        plot.oxygen = TRUE)
#'
#' @export

import.meas <- function(file, info.data,
                        n.chamber = c(1,2,3,4,5,6,7,8),
                        logger = c("AutoResp", "FishResp"),
                        date.format = c("DMY", "MDY", "YMD"),
                        start.measure = "00:00:00",
                        stop.measure = "23:59:59",
                        plot.temperature = TRUE,
                        plot.oxygen = TRUE){

  V1 <- V2 <- V3 <- V4 <- V5 <- V6 <- V7 <- V8 <- V9 <- V10 <- NULL
  V11 <- V12 <- V13 <- V14 <- V15 <- V16 <- V17 <- V18 <- V19 <- NULL
  V20 <- V21 <- V22 <- V23 <- V24 <- V25 <- V26 <- V27 <- V28 <- NULL
  Phase.Type <- Phase <- Start.Meas <- NULL

  ### AutoResp format ###
  if (logger == "AutoResp"){
    MR.data.all<-read.table(file, sep = "\t", skip=38, header=F, strip.white=T)

    if (n.chamber == 1){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1")
    }
    else if (n.chamber == 2){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2")
    }
    else if (n.chamber == 3){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3")
    }
    else if (n.chamber == 4){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4")
    }
    else if (n.chamber == 5){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5")
    }
    else if (n.chamber == 6){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6")
    }
    else if (n.chamber == 7){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22, V24, V25))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7")
    }
    else if (n.chamber == 8){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22, V24, V25, V27, V28))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7", "Temp.8", "Ox.8")
    }
    else{
      print("Please, choose the number of chambers between 1 and 8")
    }
  }

  ### FishResp format ###

  else if (logger == "FishResp"){
    MR.data.all<-read.table(file, sep = "\t", skip=1, header=F, strip.white=T)
    if (n.chamber == 1){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1")
    }
    else if (n.chamber == 2){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2")
    }
    else if (n.chamber == 3){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3")
    }
    else if (n.chamber == 4){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4")
    }
    else if (n.chamber == 5){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5")
    }
    else if (n.chamber == 6){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6")
    }
    else if (n.chamber == 7){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7")
    }
    else if (n.chamber == 8){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7", "Temp.8", "Ox.8")
    }
    else{
      print("Please, choose the number of chambers between 1 and 8")
    }
  }

  else{
    print("Please, choose the format of your data: AutoResp or FishResp")
  }

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # Formatting Date & Time Entries
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # AM PM problem solving
  PM.1 <- grepl("AM", MR.data.all[,1], fixed=TRUE)
  PM.2 <- grepl("am", MR.data.all[,1], fixed=TRUE)
  PM.3 <- grepl("PM", MR.data.all[,1], fixed=TRUE)
  PM.4 <- grepl("pm", MR.data.all[,1], fixed=TRUE)
  dts <- NULL

  if(PM.1 || PM.2 || PM.3 || PM.4 == TRUE){

    if(any(date.format == "DMY")){
      MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%d/%m/%Y/ %I:%M:%S %p")
      dts<-dates(strftime(MR.data.all$Date.Time, "%d/%m/%y"), format="d/m/y")
    }
    else if(any(date.format == "MDY")){
      MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%m/%d/%Y/ %I:%M:%S %p")
      dts<-dates(strftime(MR.data.all$Date.Time, "%m/%d/%y"), format="m/d/y")
    }
    else if(any(date.format == "YMD")){
      MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%Y/%m/%d/ %I:%M:%S %p")
      dts<-dates(strftime(MR.data.all$Date.Time, "%y/%m/%d"), format="y/m/d")
    }
    else{
      print("Please, choose the date format: DMY, MDY or YMD, where D-day, M-month, Y-year")
    }
  }
  else{
    if(any(date.format == "DMY")){
      MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%d/%m/%Y/%H:%M:%S")
      dts<-dates(strftime(MR.data.all$Date.Time, "%d/%m/%y"), format="d/m/y")
    }
    else if(any(date.format == "MDY")){
      MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%m/%d/%Y/%H:%M:%S")
      dts<-dates(strftime(MR.data.all$Date.Time, "%m/%d/%y"), format="m/d/y")
    }
    else if(any(date.format == "YMD")){
      MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%Y/%m/%d/%H:%M:%S")
      dts<-dates(strftime(MR.data.all$Date.Time, "%y/%m/%d"), format="y/m/d")
    }
    else{
      print("Please, choose the time format: DMY, MDY or YMD, where D-day, M-month, Y-year")
    }
  }

  x<-strftime(MR.data.all$Date.Time, "%H:%M:%S")
  MR.data.all$Real.Time<-chron::chron(times=x)
  rm(x)
  tms<-times(strftime(MR.data.all$Date.Time, "%H:%M:%S"))
    x<-chron::chron(dates = dts, times = tms,  format = c(dates = "yy-m-d", times = "h:m:s"))
    dts<-chron::chron(dates = dts,  format = "yy-m-d")
  MR.data.all$Date.Time<-x
  MR.data.all$Date<-dts
  MR.data.all$Real.Time<-tms
  rm(dts)
  rm(tms)
  rm(x)

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # Removing Non-Measurement Data
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # this is some simple code to get rid of all the flushing and acclimation phases
  MR.data.all$Phase.Type<-as.factor(substr(as.vector(MR.data.all$Phase),1,1))
  MR.data.all<-subset(MR.data.all, Phase.Type=="M")
  MR.data.all$Phase.Type<-NULL
  row.names(MR.data.all)<-NULL
  MR.data.all$Phase<-factor(MR.data.all$Phase)

  # here is a quick lesson in how to create an ordered factor
  # note the odering of levels for the factor 'Phase' in the current dataframe
  levels(MR.data.all$Phase)

  # now creating the re-ordered factor
  z <- MR.data.all$Phase
  z <- gsub("[M]","",z)
  z <- as.numeric(z)
  x <- ordered(MR.data.all$Phase, levels=paste(rep("M",length(levels(MR.data.all$Phase))), head(z, n=1):tail(z, n=1), sep=""))
  MR.data.all$Phase<-x
  rm(x)
  # note the new class of 'Phase' and the new ordering
  levels(MR.data.all$Phase)

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # Removing the final measurement Phase (tail error)
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  y <- length(which(MR.data.all$Phase==head(levels(MR.data.all$Phase),1), T))
  z <- length(which(MR.data.all$Phase==tail(levels(MR.data.all$Phase),1), T))
  if (y != z){
    MR.data.all<-subset(MR.data.all, Phase!=tail(levels(MR.data.all$Phase),1))
  }
  MR.data.all$Phase<-factor(MR.data.all$Phase)

  # Identification of time period (M1 error)
  for(i in 1:10){
    a <- paste("M", i, sep = "")
    ifelse(MR.data.all$Phase==a, x <- a, i<-i+1)
  }
  MR.data.all$Time<-rep(1:1:length(subset(MR.data.all, Phase == x)$Ox.1), length(levels(MR.data.all$Phase)))
  rm(x)

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # Restricting Dataset to Mearusements Taken in the Dark
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  x<-levels(MR.data.all$Phase)

  if (n.chamber == 1){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Real.Time=chron(), Date=chron(), Time=integer(),
                        Start.Meas=character(), End.Meas=character())
  }

  else if (n.chamber == 2){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Temp.2=numeric(), Ox.2=numeric(),
                        Real.Time=chron(), Date=chron(), Time=integer(), Start.Meas=character(), End.Meas=character())
  }

  else if (n.chamber == 3){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Temp.2=numeric(), Ox.2=numeric(),
                        Temp.3=numeric(), Ox.3=numeric(), Real.Time=chron(), Date=chron(), Time=integer(),
                        Start.Meas=character(), End.Meas=character())
  }

  else if (n.chamber == 4){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Temp.2=numeric(), Ox.2=numeric(),
                        Temp.3=numeric(), Ox.3=numeric(), Temp.4=numeric(), Ox.4=numeric(), Real.Time=chron(), Date=chron(), Time=integer(),
                        Start.Meas=character(), End.Meas=character())
  }

  else if (n.chamber == 5){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Temp.2=numeric(), Ox.2=numeric(),
                        Temp.3=numeric(), Ox.3=numeric(), Temp.4=numeric(), Ox.4=numeric(), Temp.5=numeric(), Ox.5=numeric(),
                        Real.Time=chron(), Date=chron(), Time=integer(), Start.Meas=character(), End.Meas=character())
  }

  else if (n.chamber == 6){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Temp.2=numeric(), Ox.2=numeric(),
                        Temp.3=numeric(), Ox.3=numeric(), Temp.4=numeric(), Ox.4=numeric(), Temp.5=numeric(), Ox.5=numeric(),
                        Temp.6=numeric(), Ox.6=numeric(),
                        Real.Time=chron(), Date=chron(), Time=integer(), Start.Meas=character(), End.Meas=character())
  }

  else if (n.chamber == 7){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Temp.2=numeric(), Ox.2=numeric(),
                        Temp.3=numeric(), Ox.3=numeric(), Temp.4=numeric(), Ox.4=numeric(), Temp.5=numeric(), Ox.5=numeric(),
                        Temp.6=numeric(), Ox.6=numeric(), Temp.7=numeric(), Ox.7=numeric(),
                        Real.Time=chron(), Date=chron(), Time=integer(), Start.Meas=character(), End.Meas=character())
  }
  else if (n.chamber == 8){
    temp.df<-data.frame(Date.Time=chron(), Phase=factor(), Temp.1=numeric(), Ox.1=numeric(), Temp.2=numeric(), Ox.2=numeric(),
                        Temp.3=numeric(), Ox.3=numeric(), Temp.4=numeric(), Ox.4=numeric(), Temp.5=numeric(), Ox.5=numeric(),
                        Temp.6=numeric(), Ox.6=numeric(), Temp.7=numeric(), Ox.7=numeric(), Temp.8=numeric(), Ox.8=numeric(),
                        Real.Time=chron(), Date=chron(), Time=integer(), Start.Meas=character(), End.Meas=character())
  }
  else{

  }

  for(i in 1:length(x))
  {	x.df<-subset(MR.data.all, Phase==x[i])
  x.start<-rep(as.character(x.df$Real.Time[1]), dim(x.df)[1])
  x.end<-rep(as.character(tail(x.df$Real.Time, 1)), dim(x.df)[1])
  x.df$Start.Meas<-x.start
  x.df$End.Meas<-x.end
  temp.df<-rbind(temp.df, x.df) }

  rm(x)
  rm(i)
  rm(x.df)
  rm(x.start)
  rm(x.end)
  temp.df$Start.Meas<-times(temp.df$Start.Meas)
  temp.df$End.Meas<-times(temp.df$End.Meas)
  temp.df$Total.Phases<-nlevels(temp.df$Phase)


  ### One or two days? (one day error)
  temp.df1<-subset(temp.df, Start.Meas>times(start.measure) & Start.Meas<times(stop.measure))
  if (length(temp.df1$Date) != 0){
    temp.df <- temp.df1
  }
  else
  {
    temp.df2<- subset(temp.df, Start.Meas>times(start.measure) | Start.Meas<times(stop.measure))
    temp.df <- temp.df2
  }

  temp.df$Phase<-factor(temp.df$Phase)

  if (plot.temperature == T){
    if(n.chamber == 1){
      par(mfrow=c(1,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 2){
      par(mfrow=c(2,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 3){
      par(mfrow=c(3,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 4){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 5){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 6){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 7){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.7~temp.df$Date.Time, main="Chamber 7", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 8){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.7~temp.df$Date.Time, main="Chamber 7", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.8~temp.df$Date.Time, main="Chamber 8", xlab = "Date and Time", ylab =  "Temperature (C)", col = "#0082FF", cex=0.8)
    }
    else{
    }
  }

  if (plot.oxygen == T){
    if(n.chamber == 1){
      par(mfrow=c(1,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 2){
      par(mfrow=c(2,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 3){
      par(mfrow=c(3,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 4){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 5){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 6){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 7){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.7~temp.df$Date.Time, main="Chamber 7", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 8){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Ox.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.7~temp.df$Date.Time, main="Chamber 7", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(temp.df$Ox.8~temp.df$Date.Time, main="Chamber 8", xlab = "Date and Time", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
    }
  }

  meas.data <- temp.df
  return(meas.data)
}

