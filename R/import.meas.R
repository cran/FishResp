#' Import Raw Data of Metabolic Rate Measurements
#'
#' The function is used to import raw data of metabolic rate measurements to R environment.
#'
#' @usage
#' import.meas(file, info.data,
#'             n.chamber = c(1,2,3,4,5,6,7,8),
#'             logger = c("AutoResp", "FishResp", "QboxAqua"),
#'             date.format = c("DMY", "MDY", "YMD"),
#'             start.measure = "00:00:00",
#'             stop.measure = "23:59:59",
#'             start.measure.date = NA,
#'             stop.measure.date = NA,
#'             set.date.time = NA,
#'             meas.to.wait = 0,
#'             meas.to.flush = 0,
#'             plot.temperature = TRUE,
#'             plot.oxygen = TRUE)
#'
#' @param file  the name of a file  which raw data of metabolic rate measurements are to be read from
#' @param info.data  a data frame obtained by using the function \code{\link{input.info}}
#' @param n.chamber  integer: the number of chambers used in an experiment (including empty ones)
#' @param logger  string: the name of a logger software used for intermittent-flow respirometry
#' \itemize{
#'   \item 'AutoResp' if you use commercial software by 'Loligo Systems'
#'   \item 'FishResp' if you use free software 'AquaResp' in combination with equipment produced by 'PreSens' or 'Pyroscience', please convert data to the 'FishResp' format using the functions \code{\link{presens.aquaresp}} or \code{\link{pyroscience.aquaresp}}, respectively. \cr If you do not use commercial software or 'AquaResp' for running intermittent-flow respirometry, adjust raw data manually to the 'FishResp' format (see Details below).
#'   \item 'QboxAqua' if you use commercial software by 'Qubit Systems'
#' }
#' @param date.format  string: date format (DMY, MDY or YMD)
#' @param start.measure  chron: time when metabolic rate measurements are started
#' @param stop.measure  chron: time when metabolic rate measurements are finished
#' @param start.measure.date  chron: date when metabolic rate measurements are started
#' @param stop.measure.date  chron: date when metabolic rate measurements are finished
#' @param set.date.time  chron: this parameter is turned off by default and needed to be specified only if raw data were recorded by 'Q-box Aqua' logger software. Specifically, input the date and time when .cmbl file was built in one of the following formats: "dd/mm/yyyy/hh:mm:ss", "mm/dd/yyyy/hh:mm:ss", or "yyyy/mm/dd/hh:mm:ss" (in accourdance to the chosen date.format parameter).
#' @param meas.to.wait  integer: the number of first rows for each measurement phase (M) which should be reassigned to the wait phase (W). The parameter should be used when the wait phase (W) is absent (e.g. in 'Q-box Aqua' logger software) or not long enough to eliminate non-linear change in DO concentration over time from the measurement phase (M) after shutting off water supply from the ambient water source.
#' @param meas.to.flush  integer: the number of last rows for each measurement phase (M) which should be reassigned to the flush phase (F). The parameter should be used to eliminate non-linear change in DO concentration over time from the measurement phase (M) after untimely shutting on water supply from the ambient water source.
#' @param plot.temperature  logical: if TRUE then the graph of raw temperature data is plotted
#' @param plot.oxygen  logical: if TRUE then the graph of raw oxygen data is plotted
#'
#' @details If you use closed respirometry approach, please standardize raw data. The example of "FishResp" format for 4-channel respirometry system is shown here:
#' \tabular{cccccccccc}{
#'   Date&Time \tab Phase \tab Temp.1 \tab Ox.1 \tab Temp.2 \tab Ox.2 \tab Temp.3 \tab Ox.3 \tab Temp.4 \tab Ox.4\cr
#'   19/08/2016/18:47:20 \tab F1 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#'   19/08/2016/18:47:21 \tab F1 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#'   19/08/2016/18:47:22 \tab M1 \tab 24.49 \tab 7.77 \tab 24.56 \tab 7.72 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#'   19/08/2016/18:47:23 \tab M1 \tab 24.49 \tab 7.76 \tab 24.56 \tab 7.72 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#' } where the items are:
#' \itemize{
#' \item Date&Time should be represented in one of the following formats: "dd/mm/yyyy/hh:mm:ss", "mm/dd/yyyy/hh:mm:ss", or "yyyy/mm/dd/hh:mm:ss". Time step-interval is one second: one row of data per second.
#' \item Phase should have at least two levels: M (measurement) and F (flush). The ordinal number of a phase should be attached to the level of a phase: F1, M1, F2, M2 ...
#' \item Temp.1 contains values of water temperature in Celsius (\eqn{C^{o}}) for Chamber 1
#' \item Ox.1 contains values of dissolved oxygen measured in 'mg/L', 'mmol/L' or 'ml/L' for Chamber 1. If other measurement units were used, convert them to 'mg/L', 'mmol/L' or 'ml/L' using the function \code{\link{convert.respirometry}} or \code{\link{convert.rMR}}.
#' \item ...
#' }
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
#'                        start.measure = "22:00:00",
#'                        stop.measure = "06:00:00",
#'                        plot.temperature = TRUE,
#'                        plot.oxygen = TRUE)
#'
#' AMR.path = system.file("extdata/stickleback/AMR_raw.txt.xz", package = "FishResp")
#' AMR.raw <- import.meas(file = AMR.path,
#'                        info.data = info,
#'                        logger = "AutoResp",
#'                        n.chamber = 4,
#'                        date.format = "DMY",
#'                        plot.temperature = TRUE,
#'                        plot.oxygen = TRUE)
#'
#' # an example for importing raw data recorded by 'Q-box Aqua'
#' qbox.path = system.file("extdata/qboxaqua/qboxaqua.csv", package = "FishResp")
#' RMR.raw <- import.meas(file = qbox.path,
#'                         info.data = info,
#'                         logger = "QboxAqua",
#'                         n.chamber = 1,
#'                         date.format = "DMY",
#'                         start.measure = "23:30:00",
#'                         stop.measure = "01:00:00",
#'                         set.date.time = "23/02/2014/23:30:22",
#'                         meas.to.wait = 200,
#'                         plot.temperature = TRUE,
#'                         plot.oxygen = TRUE)
#' }
#' @export

import.meas <- function(file, 
                        info.data,
                        n.chamber = c(1,2,3,4,5,6,7,8),
                        logger = c("AutoResp", "FishResp", "QboxAqua"),
                        date.format = c("DMY", "MDY", "YMD"),
                        start.measure = "00:00:00",
                        stop.measure = "23:59:59",
                        start.measure.date = NA,
                        stop.measure.date = NA,
                        set.date.time = NA,
                        meas.to.wait = 0,
                        meas.to.flush = 0,
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
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(MR.data.all$Ox.1, na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
    }
    else if (n.chamber == 2){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
    }
    else if (n.chamber == 3){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
    }
    else if (n.chamber == 4){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
    }
    else if (n.chamber == 5){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
    }
    else if (n.chamber == 6){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5, MR.data.all$Ox.6), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.6))){MR.data.all$Ox.6[is.na(MR.data.all$Ox.6)] <- avoid.all.NA}
    }
    else if (n.chamber == 7){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22, V24, V25))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5, MR.data.all$Ox.6, MR.data.all$Ox.7), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.6))){MR.data.all$Ox.6[is.na(MR.data.all$Ox.6)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.7))){MR.data.all$Ox.7[is.na(MR.data.all$Ox.7)] <- avoid.all.NA}
    }
    else if (n.chamber == 8){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22, V24, V25, V27, V28))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7", "Temp.8", "Ox.8")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5, MR.data.all$Ox.6, MR.data.all$Ox.7, MR.data.all$Ox.8), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.6))){MR.data.all$Ox.6[is.na(MR.data.all$Ox.6)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.7))){MR.data.all$Ox.7[is.na(MR.data.all$Ox.7)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.8))){MR.data.all$Ox.8[is.na(MR.data.all$Ox.8)] <- avoid.all.NA}
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
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(MR.data.all$Ox.1, na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
    }
    else if (n.chamber == 2){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
    }
    else if (n.chamber == 3){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
    }
    else if (n.chamber == 4){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
    }
    else if (n.chamber == 5){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
    }
    else if (n.chamber == 6){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5, MR.data.all$Ox.6), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.6))){MR.data.all$Ox.6[is.na(MR.data.all$Ox.6)] <- avoid.all.NA}
    }
    else if (n.chamber == 7){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5, MR.data.all$Ox.6, MR.data.all$Ox.7), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.6))){MR.data.all$Ox.6[is.na(MR.data.all$Ox.6)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.7))){MR.data.all$Ox.7[is.na(MR.data.all$Ox.7)] <- avoid.all.NA}
    }
    else if (n.chamber == 8){
      MR.data.all<-subset(MR.data.all, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7", "Temp.8", "Ox.8")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(c(MR.data.all$Ox.1, MR.data.all$Ox.2, MR.data.all$Ox.3, MR.data.all$Ox.4,
                   MR.data.all$Ox.5, MR.data.all$Ox.6, MR.data.all$Ox.7, MR.data.all$Ox.8), na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.2))){MR.data.all$Ox.2[is.na(MR.data.all$Ox.2)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.3))){MR.data.all$Ox.3[is.na(MR.data.all$Ox.3)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.4))){MR.data.all$Ox.4[is.na(MR.data.all$Ox.4)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.5))){MR.data.all$Ox.5[is.na(MR.data.all$Ox.5)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.6))){MR.data.all$Ox.6[is.na(MR.data.all$Ox.6)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.7))){MR.data.all$Ox.7[is.na(MR.data.all$Ox.7)] <- avoid.all.NA}
      if(all(is.na(MR.data.all$Ox.8))){MR.data.all$Ox.8[is.na(MR.data.all$Ox.8)] <- avoid.all.NA}
    }
    else{
      print("Please, choose the number of chambers between 1 and 8")
    }
  }

  ### QboxAqua format ###

  else if (logger == "QboxAqua"){
    MR.data.all<-read.table(file, sep = ",", skip=2, header=F, strip.white=T)
    if (n.chamber == 1){
      MR.data.all<-subset(MR.data.all, select=c(V1, V9, V4, ncol(MR.data.all)))
      names(MR.data.all)<-c("Date.Time", "Phase", "Temp.1", "Ox.1")
      for(i in 3:ncol(MR.data.all)){MR.data.all[,i] = as.numeric(gsub(',','.', MR.data.all[,i]))}
      avoid.all.NA <- max(MR.data.all$Ox.1, na.rm = TRUE)
      if(all(is.na(MR.data.all$Ox.1))){MR.data.all$Ox.1[is.na(MR.data.all$Ox.1)] <- avoid.all.NA}

      ### Indexing measurement phases for QboxAqua
      MR.data.all$Phase[MR.data.all$Phase == "1"] <- "F"
      MR.data.all$Phase[MR.data.all$Phase == "0"] <- "M"
      bdrs <- which(c(FALSE, tail(MR.data.all$Phase,-1) != head(MR.data.all$Phase,-1)))
      bdrs <- paste(bdrs , MR.data.all$Phase[bdrs], sep = "")
      M.start <- grep('M', bdrs, value=TRUE)
      M.start <- as.integer(sub("M$", "", M.start))
      M.end <- grep('F', bdrs, value=TRUE)
      M.end <- as.integer(sub("F$", "", M.end)) - 1
      if(M.end[1] < M.start[1]){M.end <- M.end[-1]}

      for(i in 1:length(M.end)){
        MR.data.all$Phase[M.start[i]:M.end[i]] <- paste(MR.data.all$Phase[M.start[i]:M.end[i]], i, sep = "")
      }
    }
    else{
      print("If 'Qubit Systems' starts producing multi-chamber systems for aquatic respirometry, please contact us via email: fishresp@gmail.com")
    }
  }

  else{
    print("Please, choose the format of your data: AutoResp, FishResp or QboxAqua")
  }

  rm(avoid.all.NA)

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # Formatting Date & Time Entries
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # AM PM problem solving
  PM.1 <- grepl("AM", MR.data.all[,1], fixed=TRUE)
  PM.2 <- grepl("am", MR.data.all[,1], fixed=TRUE)
  PM.3 <- grepl("PM", MR.data.all[,1], fixed=TRUE)
  PM.4 <- grepl("pm", MR.data.all[,1], fixed=TRUE)
  dts <- NULL

  if(any(PM.1) || any(PM.2) || any(PM.3) || any(PM.4) == TRUE){

    if(any(date.format == "DMY")){
      if(logger == "QboxAqua"){
        crdt<-strptime(as.character(set.date.time), "%d/%m/%Y/ %I:%M:%S %p")
        crdt <- rep(crdt, length(MR.data.all$Date.Time))
        crdt$sec <- crdt$sec + MR.data.all$Date.Time
          if(any(crdt$sec == 60)){
            s<-which(crdt$sec == 60)
            crdt$sec[s] = 0
            crdt$min[s] = crdt$min[s]+1
          }else{
            }
        MR.data.all$Date.Time <- crdt
      }
      else{
        MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%d/%m/%Y/ %I:%M:%S %p")
      }
      dts<-dates(strftime(MR.data.all$Date.Time, "%d/%m/%y"), format="d/m/y")
    }
    else if(any(date.format == "MDY")){
      if(logger == "QboxAqua"){
        crdt<-strptime(as.character(set.date.time), "%m/%d/%Y/ %I:%M:%S %p")
        crdt <- rep(crdt, length(MR.data.all$Date.Time))
        crdt$sec <- crdt$sec + MR.data.all$Date.Time
          if(any(crdt$sec == 60)){
            s<-which(crdt$sec == 60)
            crdt$sec[s] = 0
            crdt$min[s] = crdt$min[s]+1
          }else{
            }
        MR.data.all$Date.Time <- crdt
      }
      else{
        MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%m/%d/%Y/ %I:%M:%S %p")
      }
      dts<-dates(strftime(MR.data.all$Date.Time, "%m/%d/%y"), format="m/d/y")
    }
    else if(any(date.format == "YMD")){
      if(logger == "QboxAqua"){
        crdt<-strptime(as.character(set.date.time), "%Y/%m/%d/ %I:%M:%S %p")
        crdt <- rep(crdt, length(MR.data.all$Date.Time))
        crdt$sec <- crdt$sec + MR.data.all$Date.Time
          if(any(crdt$sec == 60)){
            s<-which(crdt$sec == 60)
            crdt$sec[s] = 0
            crdt$min[s] = crdt$min[s]+1
          }else{
            }
        MR.data.all$Date.Time <- crdt
      }
      else{
        MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%Y/%m/%d/ %I:%M:%S %p")
      }
      dts<-dates(strftime(MR.data.all$Date.Time, "%y/%m/%d"), format="y/m/d")
    }
    else{
      print("Please, choose the date format: DMY, MDY or YMD, where D-day, M-month, Y-year")
    }
  }
  else{
    if(any(date.format == "DMY")){
      if(logger == "QboxAqua"){
        crdt<-strptime(as.character(set.date.time), "%d/%m/%Y/%H:%M:%S")
        crdt <- rep(crdt, length(MR.data.all$Date.Time))
        crdt$sec <- crdt$sec + MR.data.all$Date.Time
          if(any(crdt$sec == 60)){
            s<-which(crdt$sec == 60)
            crdt$sec[s] = 0
            crdt$min[s] = crdt$min[s]+1
          }else{
            }
        MR.data.all$Date.Time <- crdt
      }
      else{
        MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%d/%m/%Y/%H:%M:%S")
      }
      dts<-dates(strftime(MR.data.all$Date.Time, "%d/%m/%y"), format="d/m/y")
    }
    else if(any(date.format == "MDY")){
      if(logger == "QboxAqua"){
        crdt<-strptime(as.character(set.date.time), "%m/%d/%Y/%H:%M:%S")
        crdt <- rep(crdt, length(MR.data.all$Date.Time))
        crdt$sec <- crdt$sec + MR.data.all$Date.Time
          if(any(crdt$sec == 60)){
            s<-which(crdt$sec == 60)
            crdt$sec[s] = 0
            crdt$min[s] = crdt$min[s]+1
          }else{
            }
        MR.data.all$Date.Time <- crdt
      }
      else{
        MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%m/%d/%Y/%H:%M:%S")
      }
      dts<-dates(strftime(MR.data.all$Date.Time, "%m/%d/%y"), format="m/d/y")
    }
    else if(any(date.format == "YMD")){
      if(logger == "QboxAqua"){
        crdt<-strptime(as.character(set.date.time), "%Y/%m/%d/%H:%M:%S")
        crdt <- rep(crdt, length(MR.data.all$Date.Time))
        crdt$sec <- crdt$sec + MR.data.all$Date.Time
          if(any(crdt$sec == 60)){
            s<-which(crdt$sec == 60)
            crdt$sec[s] = 0
            crdt$min[s] = crdt$min[s]+1
          }else{
            }
        MR.data.all$Date.Time <- crdt
      }
      else{
        MR.data.all$Date.Time<-strptime(as.character(MR.data.all$Date.Time), "%Y/%m/%d/%H:%M:%S")
      }
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
  # note the ordering of levels for the factor 'Phase' in the current dataframe
  # RUN: levels(MR.data.all$Phase)

  # now creating the re-ordered factor
  z <- MR.data.all$Phase
  z <- gsub("[M]","",z)
  z <- as.numeric(z)
  x <- ordered(MR.data.all$Phase, levels=paste(rep("M",length(levels(MR.data.all$Phase))), head(z, n=1):tail(z, n=1), sep=""))
  MR.data.all$Phase<-x
  rm(x)
  # note the new class of 'Phase' and the new ordering
  # RUN: levels(MR.data.all$Phase)

  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  # Removing the final measurement Phase (tail error)
  #--------------------------------------------------------------------------------------------------------------------------------------------------#
  y <- length(which(MR.data.all$Phase==head(levels(MR.data.all$Phase),1), T))
  y <- y - 10 #some buffer in case of unexpected lags
  z <- length(which(MR.data.all$Phase==tail(levels(MR.data.all$Phase),1), T))
  if (z < y){
    MR.data.all<-subset(MR.data.all, Phase!=tail(levels(MR.data.all$Phase),1))
  }
  MR.data.all$Phase<-factor(MR.data.all$Phase)
  row.names(MR.data.all) <- 1:nrow(MR.data.all)

  # Measurement phase seconds (M) converted to waiting (W) or flushing (F)
  if(meas.to.wait != 0){
    idx <- unlist(tapply(1:nrow(MR.data.all), MR.data.all$Phase, tail, -(meas.to.wait)), use.names=FALSE)
    MR.data.all <- MR.data.all[idx, ]
  }else{
    }

  if(meas.to.flush != 0){
    idx <- unlist(tapply(1:nrow(MR.data.all), MR.data.all$Phase, head, -(meas.to.flush)), use.names=FALSE)
    MR.data.all <- MR.data.all[idx, ]
  }else{
    }
    
  row.names(MR.data.all) <- 1:nrow(MR.data.all)
  
  #----------------------------------------------#
  # Append time index for each measurement phase #
  #----------------------------------------------#
  i = 1
  time.vector = NULL

  for(i in as.numeric(gsub("[M]","",levels(MR.data.all$Phase)))){
    a <- paste("M", i, sep = "")
    time.vector <- append(time.vector, rep(1:1:length(subset(MR.data.all, Phase == a)$Ox.1)))
    i <- i + 1
  }

  MR.data.all$Time<-time.vector
  rm(i)
  rm(time.vector)

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

  for(i in 1:length(x)){	
    x.df<-subset(MR.data.all, Phase==x[i])
    x.start<-rep(as.character(x.df$Real.Time[1]), dim(x.df)[1])
    x.end<-rep(as.character(tail(x.df$Real.Time, 1)), dim(x.df)[1])
    x.df$Start.Meas<-x.start
    x.df$End.Meas<-x.end
    temp.df<-rbind(temp.df, x.df)
    }

  rm(x)
  rm(i)
  rm(x.df)
  rm(x.start)
  rm(x.end)

  temp.df$Start.Meas<-times(temp.df$Start.Meas)
  temp.df$End.Meas<-times(temp.df$End.Meas)
  temp.df$Total.Phases<-nlevels(temp.df$Phase) ### Why?! CHECK!!!
  
  #-------------------------------------------------------------------#
  # Filtering data based on start.measure and stop.measure thresholds #
  #-------------------------------------------------------------------#
  

  ### START ###
  if (is.na(start.measure.date) == TRUE){
    start.date.time <- temp.df$Date.Time[na.omit(which(temp.df$Real.Time >= times(start.measure)))][1]
  }
  else{
    if(date.format == "DMY"){
      start.date.v.0 <- strptime(as.character(start.measure.date), "%d/%m/%Y")
      start.date.v.1 <- dates(strftime(start.date.v.0, "%d/%m/%y"), format="d/m/y")
    }
    if(date.format == "MDY"){
      start.date.v.0 <- strptime(as.character(start.measure.date), "%m/%d/%Y")
      start.date.v.1 <- dates(strftime(start.date.v.0, "%m/%d/%y"), format="m/d/y")
    }
    if(date.format == "YMD"){
      start.date.v.0 <- strptime(as.character(start.measure.date), "%Y/%m/%d")
      start.date.v.1 <- dates(strftime(start.date.v.0, "%y/%m/%d"), format="y/m/d")
    }
    start.date.v.2 <- chron(dates. = start.date.v.1,  format = "yy-m-d")
    start.date.time <- chron(start.date.v.2, times(start.measure), format = c(dates = "yy-m-d ", times = "h:m:s"))
  }

  ### STOP ###
  if (is.na(stop.measure.date) == TRUE){
    stop.date.time <- rev(temp.df$Date.Time[na.omit(which(temp.df$Real.Time <= times(stop.measure)))])[1]
  }
  else{
    if(date.format == "DMY"){
      stop.date.v.0 <- strptime(as.character(stop.measure.date), "%d/%m/%Y")
      stop.date.v.1 <- dates(strftime(stop.date.v.0, "%d/%m/%y"), format="d/m/y")
    }
    if(date.format == "MDY"){
      stop.date.v.0 <- strptime(as.character(stop.measure.date), "%m/%d/%Y")
      stop.date.v.1 <- dates(strftime(stop.date.v.0, "%m/%d/%y"), format="m/d/y")
    }
    if(date.format == "YMD"){
      stop.date.v.0 <- strptime(as.character(stop.measure.date), "%Y/%m/%d")
      stop.date.v.1 <- dates(strftime(stop.date.v.0, "%y/%m/%d"), format="y/m/d")
    }
    stop.date.v.2 <- chron(dates. = stop.date.v.1,  format = "yy-m-d")
    stop.date.time <- chron(stop.date.v.2, times(stop.measure), format = c(dates = "yy-m-d ", times = "h:m:s"))
  }

  temp.df <- subset(temp.df, (temp.df$Date.Time>start.date.time & temp.df$Date.Time<stop.date.time))
  temp.df$Phase<-factor(temp.df$Phase)

  if (plot.temperature == T){
    if(n.chamber == 1){
      par(mfrow=c(1,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 2){
      par(mfrow=c(2,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 3){
      par(mfrow=c(3,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 4){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 5){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 6){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 7){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.7~temp.df$Date.Time, main="Chamber 7", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else if(n.chamber == 8){
      par(mfrow=c(4,1), ask = T)
      plot(temp.df$Temp.1~temp.df$Date.Time, main="Chamber 1", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.2~temp.df$Date.Time, main="Chamber 2", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.3~temp.df$Date.Time, main="Chamber 3", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.4~temp.df$Date.Time, main="Chamber 4", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.5~temp.df$Date.Time, main="Chamber 5", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.6~temp.df$Date.Time, main="Chamber 6", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.7~temp.df$Date.Time, main="Chamber 7", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(temp.df$Temp.8~temp.df$Date.Time, main="Chamber 8", xlab = "Date and Time", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
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
