# P5_Compound.R
# About: This program will open the exceedance file for the selected 
#       variable and calculate a 'wave' of when the exceedance occurs.
#       Most recent run of day historical (B) took 30 hr, 13GB; SSP126 (C & D)
#       took 32h, 13GB; SSP585(E & F) took h, 14GB 
#       Time: 30-35hr for 1 variable - 8 Models
#
# Inputs: EXCEED_DAY & WAVES_DAY
# Outputs: COMP_DAY_comp
#
# T. A. Schillerberg
#               Oct. 2022
#      Updated: Jan. 2024

# Mac

# Office Computer
# setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
# fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# HPC
fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ###############################################################
library(tidyverse)

# Part I Variables To Change ##############################################
mNum <- as.numeric('model_num') # Bash script
# mNum <- 1 # Select a model (1-4)
# wavesTime <- c('WAVES_DAY','WAVES_WEEK','WAVES_MONTH')[as.numeric('waves_time')]
wavesTime <- c('WAVES_DAY_','WAVES_WEEK_','WAVES_MONTH_')[1]
wavesTime2 <- c('WAVES_DAY_','WAVES_WEEK_FD_','WAVES_WEEK_D_',
               'WAVES_MONTH_FD_','WAVES_MONTH_D_')[1]
mFile <- c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_',
           '_day_EC-Earth3_historical_r1i1p1f1_gr_',
           '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
           '_day_INM-CM4-8_historical_r1i1p1f1_gr1_',
           '_day_INM-CM5-0_historical_r1i1p1f1_gr1_',
           '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
           '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_',
           '_day_NorESM2-MM_historical_r1i1p1f1_gn_') [mNum]
# mFile <- c('_day_CMCC-ESM2_ssp126_r1i1p1f1_gn_',
#            '_day_EC-Earth3_ssp126_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp126_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_ssp126_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_ssp126_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp126_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp126_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_ssp126_r1i1p1f1_gn_')[mNum]
# mFile <- c('_day_CMCC-ESM2_ssp585_r1i1p1f1_gn_',
#            '_day_EC-Earth3_ssp585_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_ssp585_r1i1p1f1_gn_')[mNum]
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')[1]
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/') [mNum]
var <- c('tasmax', 'tasmin','pr','mrsos')
startyr <- 1980
endyr <- 2010

print(paste0('Model: ',loc2))
print(paste0('Temporal Resolution: ', wavesTime))
print('Rscript: P5_Compound.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))
# Functions ####################################################################
daily_Comp_Event <- function (seriesA, seriesB, delta = 0, tau = 0, 
                              repNa = TRUE, ending = TRUE){
  # About: This function will calculate daily compound events using data that 
  #        contains waves of data. IE heatwave, drought period.
  
  # seriesA : A numeric vector containing values of 1(non wave var), 0, 
  #           or < 0 values
  # seriesB : A numeric vector containing values of 1(non wave var), 0, 
  #           or < 0 value
  # delta   : Non-negative integer for defining the tolerance window
  # tau     : Non-negative integer to specify the time lag
  # repNa   : Should any NA's in the time series be replaced with 0. If FALSE 
  #           and the series contains NA's an error will be given.
  
  # 1 Test the variables -------------------------------------------------------
  if (length(which(seriesA > 1)) > 0) {
    print("|--------------    ERROR     --------------|")
    print("|       Time seriesA contins values        |")
    print("|              larger than 1               |")
    print("|------------------------------------------|")
    return()
  }
  if (length(which(seriesB > 1)) > 0) {
    print("|--------------    ERROR     --------------|")
    print("|       Time seriesA contins values        |")
    print("|              larger than 1               |")
    print("|------------------------------------------|")
    return()
  }
  if (tau < 0 || delta < 0) {
    print("|-------------     ERROR      -------------|")
    print("|    The offset (tau) or delta T (dela)    |")
    print("|              is negative.                |")
    print("|------------------------------------------|")
    return()
  }
  if (length(seriesA) != length(seriesB)) {
    print("|-------------     ERROR      -------------|")
    print("|    lengths of series A and B differ      |")
    print("|------------------------------------------|")
    return()
  }
  if (any(is.na(seriesA)) && repNA == FALSE) {
    print("|-------------     ERROR      -------------|")
    print("|         Time seriesA contains NAs        |")
    print("|------------------------------------------|")
    return()
  }
  if (any(is.na(seriesB)) && repNA == FALSE) {
    print("|-------------     ERROR      -------------|")
    print("|         Time seriesB contains NAs        |")
    print("|------------------------------------------|")
    return()
  }
  
  # 2 Variables ----------------------------------------------------------------
  # Replace NAs with 0
  if (any(is.na(seriesA))){
    seriesA[is.na(seriesA)] <- 0
  }
  if (any(is.na(seriesB))){
    seriesB[is.na(seriesB)] <- 0
  }
  
  lenSer <- length(seriesA)
  modI <- 1
  
  # Return Variable
  returnDat <- matrix(0, nrow = 1, ncol = (6 + lenSer))
  
  bindDat = matrix(0, nrow = 3, ncol = lenSer)
  bindDat[1, ] = seriesA
  bindDat[2, ] = seriesB
  
  # 3 Calculations -------------------------------------------------------------
  # . 3.1 How many events in the series? ----
  returnDat[1] <- sum(bindDat[1, ], na.rm = TRUE)
  returnDat[3] <- sum(bindDat[2, ], na.rm = TRUE)
  
  # . 3.2 What is the average length of the series ----
  dat <- bindDat[1,]
  dat[dat == 0] <- NA
  returnDat[2] <- 1/mean(dat, na.rm = TRUE)
  dat <- bindDat[2,]
  dat[dat == 0] <- NA
  returnDat[4] <- 1/mean(dat, na.rm = TRUE)
  
  # . 3.3 Calculating the compound events ----
  for (i in 1:lenSer){ 
    if (is.na(bindDat[1, i])) { next }
    if (modI == i){ 
      if (bindDat[2, i] > 0 ){
        # Only changes for sequential events. 
        # Requirements for the start (seriesA event end) and end (seriesB)
        start <- i - tau - delta
        end <- i - tau
        if (start < 1 && end < 1) { next } 
        if (start < 1 && end >= 1) { start <- 1 }
        if (start > lenSer) { next }
        if (end > lenSer) { end <- lenSer }
        m <- which(bindDat[1, start:end] > 0)
        if (length(m) >= 1){
          if (delta == 0){   # Simultanious Events
            day = 0;
            for (j in i:lenSer){
              if (bindDat[1,j] != 0 && bindDat[2,j] != 0) {
                day <- day + 1
                bindDat[3, (i:j)] <- 1/day
                modI <- j + 1
              } else {
                # bindDat[3, (i:j-1)] <- 1/day
                # modI <- j + 1
                break
              }
            }
          } # End Simultanious Events
          if (delta != 0 && ending == TRUE){   # Sequential Events
            len <- start:end
            daySt <- NA; dayEd <- NA; modJ <- FALSE
            for (j in len[m[length(m)]]:1){
              if (bindDat[2,j] == 0 && bindDat[1,j] > 0 | j == 1){
                daySt <- j 
              } else { break }
            }
            if (is.na(daySt)) { next }
            for (j in i:lenSer){
              if (bindDat[2,j] == 0 && bindDat[1,j] == 0 && modJ == TRUE) { break } 
              if (bindDat[2,j] > 0 && bindDat[1,j] == 0 | j == lenSer){
                dayEd <- j # Want to end on the last day of eventB
                modI <- j + 1    # this may need help 
                modJ <- TRUE
                # break
              } else {
                dayEd <- NA
                break
              }
            }
            if (bindDat[1,i] > 0 & bindDat[2,i] > 0) { next } # remove  Simultanious events
            if (is.na(dayEd)) { next }
            bindDat[3, daySt:dayEd] <- 1/length(daySt:dayEd)
          } # End Sequential Events Calc from sA ending
          if (delta != 0 && ending == FALSE){   # Sequential Events
            len <- start:end
            daySt <- NA; dayEd <- NA; modJ <- FALSE
            for (j in len[m[length(m)]]:1){
              if (len[m[length(m)]] == bindDat[1,i]) { next }
              if ( bindDat[1,j] > 0 | j == 1){
                daySt <- j 
              } else { break }
            }
            if (is.na(daySt)) { next }
            for (j in i:lenSer){
              if (bindDat[2,j] > 0 | j == lenSer){
                dayEd <- j # Want to end on the last day of eventB
                modI <- j + 1  # this may need help 
                # break
              } else { break }
            }
            if (is.na(dayEd)) { next }
            bindDat[3, daySt:dayEd] <- 1/length(daySt:dayEd)
          }  # End Sequential Events from sB start
        } else { 
          # modI <- i + 1
          modI <- i + round(1 / bindDat[2, i], digits = 0)
        }
      } else if ( modI > i) {
        next
      } else { 
        modI <- i + 1 
      }
    } else if ( modI > i) {
      next
    } else {
      modI <- i + 1
    }
  } # End For loop
  # . 3.4 How many events in the compound event ----
  returnDat[5] <- sum(bindDat[3,], na.rm = TRUE)
  
  # . 3.5 What is the average length of the compound Event ----
  dat <- bindDat[3,]
  dat[dat == 0] <- NA
  returnDat[6] <- 1/mean(dat, na.rm = TRUE)
  
  # Return Variable ------------------------------------------------------------
  returnDat[7:length(returnDat)] <- bindDat[3,]
  colnames(returnDat) <- c('N_SeriesA', 'Length_SeriesA', 
                           'N_SeriesB', 'Length_SeriesB',
                           'N_SeriesCE', 'Length_SeriesCE',
                           1:lenSer)
  return(returnDat)
}

lineCC <- function(dat, days, tau = 0, ending){
  # This function will take the data given (combined binary time series), split 
  #      into two series and then send the data into the CoinCalc::CC.eca.ts 
  #      function using the days(delT) specified by the user. The data will then
  #      be taken out of the function results list and put into a data frame 
  #      before being returned to the call function.
  
  # dat : matrix containing the combined binary time series
  # days : delta how much of a delay allowed for sequential events

  # Variables ------------------------------------------------------------------
  # print(dat[1])
  # dat <- dat[2:length(dat)]
  dat <- dat[1:length(dat)]
  seriesA <- dat[1:(length(dat)/2)]
  seriesB <- dat[(length(dat)/2 + 1): length(dat)]
  # need to make sure its a matrix
  days <- days
  
  # Calculations ---------------------------------------------------------------
  # Check to make sure they are the same dimentions
  if (length(seriesA) == length(seriesB)){
    #  Correlation
    result <- daily_Comp_Event(seriesA = seriesA, seriesB = seriesB, 
                                delta = days, tau = tau, ending = ending)
  } else {
    result <- c(vector('ERROR',length = 6 + length(seriesA)))
  }
  return(result)
}

# Part II Opening files & Formatting ###########################################
A <- Sys.time()
print(paste0('Starting to open the required file at: ',A))
# Tmax
series1 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime, var[1],
                           mFile, startyr,'-',endyr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
lonlat <- series1[,1:2]
ID <- rowid_to_column(series1); ID <- ID[,1]; ID <- as.matrix(ID) # For testing
series1 <- series1[,3:ncol(series1)]
series1[series1 > 0] <- 1     # Needs to be converted to binary (0,1)
series1 <- as.matrix(series1)

# Pr
if (wavesTime == 'WAVES_DAY_'){
  series3 <- read_csv(paste0(fileloc1, loc1, loc2, 'EXCEED_DAY_', var[3],
                           mFile, startyr,'-',endyr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
} else {
  series3 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime, var[3],
                             mFile, startyr,'-',endyr,'.csv'),
                      col_names = TRUE, cols(.default = col_double()))
}
series3 <- series3[,3:ncol(series3)]
series3[series3 > 0] <- 1     # Needs to be converted to binary (0,1)
series3 <- as.matrix(series3)

# mrsos
if (wavesTime == 'WAVES_DAY_'){
  series4 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime, var[4],
                             mFile, startyr,'-',endyr,'.csv'),
                      col_names = TRUE, cols(.default = col_double()))
} else {
  series4 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime2, var[4],
                             mFile, startyr,'-',endyr,'.csv'),
                      col_names = TRUE, cols(.default = col_double()))
}
series4 <- series4[,3:ncol(series4)]
series4[series4 > 0.0357] <- 0 # 28 days exceeded 0.0357
series4[series4 > 0] <- 1     # Needs to be converted to binary (0,1)
series4 <- as.matrix(series4)

if (wavesTime == 'WAVES_DAY_'){
  a <-strsplit(wavesTime,'_') %>% unlist()
  a <- a[2]
  seqTime <- 7
} else if (wavesTime == 'WAVES_WEEK_'){
  a <-strsplit(wavesTime2,'_') %>% unlist()
  a <- paste0(a[2],'_',a[3])
  seqTime <- 1
} else if (wavesTime == 'WAVES_MONTH_'){
  a <-strsplit(wavesTime2,'_') %>% unlist()
  a <- paste0(a[2],'_',a[3])
  seqTime <- 1
}else {
  print( 'Variable wavesTime not reconized.')
}
colSerA <- colnames(series1)

# Part III Heatwave & Drought ##################################################
# . 3.1 Sim. Heatwave & Drought ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Heatwave & Drought at: ',B))

if (dim(series1)[1] == dim(series4)[1] | dim(series1)[2] == dim(series4)[2]){
  seriesC <- cbind(series1, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = 0) %>%
    t() 
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SIM14',
                                mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}
B <- Sys.time()
print(paste0('Finished calculated the simultanious Heatwave & Drought at: ',B))

# . 3.2 Seq. Heatwave & Drought ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Heatwave & Drought at: ',B))

if (dim(series1)[1] == dim(series4)[1] | dim(series1)[2] == dim(series4)[2]){
  seriesC <- cbind(series1, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = TRUE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ14_EndsA',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}
if (dim(series1)[1] == dim(series4)[1] | dim(series1)[2] == dim(series4)[2]){
  seriesC <- cbind(series1, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = FALSE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ14',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the sequential Heatwave & Drought at: ',B))

# . 3.3 Seq. Drought & Heatwave ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Drought & Heatwave at: ',B))

if (dim(series4)[1] == dim(series1)[1] | dim(series4)[2] == dim(series1)[2]){
  seriesC <- cbind(series4, series1)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = TRUE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ41_EndsA',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}
if (dim(series4)[1] == dim(series1)[1] | dim(series4)[2] == dim(series1)[2]){
  seriesC <- cbind(series4, series1)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = FALSE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ41',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the sequential Drought & Heatwave at: ',B))

# Part IV Extreme Precip. & Heatwave ###########################################
# . 4.1 Sim. Extreme Precip. & Heatwave ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Heatwave & Ex. Precip. at: ',B))

if (dim(series1)[1] == dim(series3)[1] | dim(series1)[2] == dim(series3)[2]){
  seriesC <- cbind(series1, series3)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = 0) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SIM13',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the simultanious Heatwave & Ex. Precip. at: ',B))

# . 4.2 Seq. Heatwave -> Extreme Precip. ---------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Heatwave & Ex. Precip. at: ',B))

if (dim(series1)[1] == dim(series3)[1] | dim(series1)[2] == dim(series3)[2]){
  seriesC <- cbind(series1, series3)
  # seriesC <- cbind(ID, series1, series3)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = TRUE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ13_EndsA',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}
if (dim(series1)[1] == dim(series3)[1] | dim(series1)[2] == dim(series3)[2]){
  seriesC <- cbind(series1, series3)
  # seriesC <- cbind(ID, series1, series3)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = FALSE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ13',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the sequential Heatwave & Ex. Precip. at: ',B))

# . 4.3 Seq. Extreme Precip -> Heatwave ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Ex. Precip. & Heatwave at: ',B))

if (dim(series3)[1] == dim(series1)[1] | dim(series3)[2] == dim(series1)[2]){
  seriesC <- cbind(series3, series1)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = TRUE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ31_EndsA',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}
if (dim(series3)[1] == dim(series1)[1] | dim(series3)[2] == dim(series1)[2]){
  seriesC <- cbind(series3, series1)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = FALSE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ31',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the simultanious Ex. Precip. & Heatwave at: ',B))
# Part V Extreme Precip. & Drought #############################################
# . 5.1 Seq. Extreme Precip. -> Drought ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Ex. Precip. &  Drought at: ',B))

if (dim(series3)[1] == dim(series4)[1] | dim(series3)[2] == dim(series4)[2]){
  seriesC <- cbind(series3, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC,
                   days = seqTime, ending = TRUE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ34_EndsA',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}
if (dim(series3)[1] == dim(series4)[1] | dim(series3)[2] == dim(series4)[2]){
  seriesC <- cbind(series3, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC,
                   days = seqTime, ending = FALSE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ34',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the sequential Ex. Precip. &  Drought at: ',B))

# . 5.2 Seq. Drought -> Extreme Precip. ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Drought & Ex. Precip. at: ',B))

if (dim(series4)[1] == dim(series3)[1] | dim(series4)[2] == dim(series3)[2]){
  seriesC <- cbind(series4, series3)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, 
                   days = seqTime, ending = TRUE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ43_EndsA',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}
if (dim(series3)[1] == dim(series4)[1] | dim(series3)[2] == dim(series4)[2]){
  seriesC <- cbind(series3, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC,
                   days = seqTime, ending = FALSE) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon', 'lat','N_SeriesA', 'Length_SeriesA',
                         'N_SeriesB', 'Length_SeriesB',
                         'N_SeriesCE', 'Length_SeriesCE',
                         colSerA)
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ34',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finsihed calculating the sequential Drought & Ex. Precip. at: ',B))

# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating the compound events.',
             'End time: ',B, 'Total time elapsed: ', B-A))
print("-----------------------------------------------------------------------")