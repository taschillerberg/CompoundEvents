# P5_Month_Waves.R
# About: This program will open files for the selected variable and calculate 
#        the monthly values.
#
# Inputs: WAVES DAY(tasmax), EXCEED (pr), ORG DAY (mrsos)
# Outputs: WAVES_MONTHLY
#
# T. A. Schillerberg
#               NOV. 2022
#      Updated: May. 2023

# Mac

# Office Computer
# setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
# fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# HPC
fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)

# Part I Variables To Change ###################################################
# var <- c('tasmax', 'tasmin', 'pr', 'mrsos')[as.numeric('model_var')] # Bash script
# mNum <- as.numeric('model_num') # Bash script
var <- c('tasmax', 'tasmin','pr','mrsos') [1]
mNum <- 8 # Select a model (1-8)
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
a <-strsplit(loc2,'/') %>% unlist()
startyr <- 1980
endyr <- 2010
if (mNum == 2| mNum == 6 | mNum == 7) {
  mLeap <- TRUE
} else { mLeap <- FALSE}

if (startyr == 1980) {
  if (mNum == 8) { 
    month <- 23748:24119 
  } else { month <- 1560:1931 }
} else if (startyr == 2040){
  if (mNum == 8) { 
    month <- 24468:24839 
  } else { month <- 2280:2651 }
} else {
  if (mNum == 8) { 
    month <- 24828:25199 
  } else { month <- 2640:3011 }
}

print(' ')
print(paste0('Model: ',loc2))
print(paste0('Variable: ', var))
print('Rscript: P3_Month_Waves.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))

# Part II Functions ############################################################
month_wave <- function(dat, years, leap){
  # About: This function will take a daily data set and convert it into a monthly 
  #        time series. The number of occurrences will be added for each month, 
  #        then the value will be divided by the number of days in the month. If
  #        there is no occurrence the value will remain 0.
  #
  # dat : array of binary variables
  # years : c(start year, end year)
  # leap : TRUE or FALSE
  
  # Variables ------------------------------------------------------------------
  datW <- array(0,dim = (12* (years[2] - years[1] + 1)))
  monthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  dayTot <- 1;     jYr <- 0
  
  # Calculations ---------------------------------------------------------------
  for (i in years[1]:years[2]){
    if (i %% 4 == 0 & i != 2100 & leap == TRUE){
      leapYr <- 1
    } else { leapYr <- 0}
    # dayYr <- sum(monthDays) + leap # Number of days in the year
    # ---- January
    m  <- which(dat[dayTot:(dayTot+monthDays[1]-1)] == 1)
    n <- length(m)/monthDays[1]
    datW[(jYr*12)+1] <- n
    dayTot <- dayTot + monthDays[1] # February start day
    # ---- February
    m  <- which(dat[dayTot:(dayTot+monthDays[2]-1+leapYr)] == 1)
    n <- length(m)/monthDays[2]
    datW[(jYr*12)+2] <- n
    dayTot <- dayTot + monthDays[2] + leapYr # March start day
    # ---- March
    m  <- which(dat[dayTot:(dayTot+monthDays[3]-1)] == 1)
    n <- length(m)/monthDays[3]
    datW[(jYr*12)+3] <- n
    dayTot <- dayTot + monthDays[3] # April start day
    # ---- April
    m  <- which(dat[dayTot:(dayTot+monthDays[4]-1)] == 1)
    n <- length(m)/monthDays[4]
    datW[(jYr*12)+4] <- n
    dayTot <- dayTot + monthDays[4] # May start day
    # ---- May
    m  <- which(dat[dayTot:(dayTot+monthDays[5]-1)] == 1)
    n <- length(m)/monthDays[5]
    datW[(jYr*12)+5] <- n
    dayTot <- dayTot + monthDays[5] # June start day
    # ---- June
    m  <- which(dat[dayTot:(dayTot+monthDays[6]-1)] == 1)
    n <- length(m)/monthDays[6]
    datW[(jYr*12)+6] <- n
    dayTot <- dayTot + monthDays[6] # July start day
    # ---- July
    m  <- which(dat[dayTot:(dayTot+monthDays[7]-1)] == 1)
    n <- length(m)/monthDays[7]
    datW[(jYr*12)+7] <- n
    dayTot <- dayTot + monthDays[7] # August start day
    # ---- August
    m  <- which(dat[dayTot:(dayTot+monthDays[8]-1)] == 1)
    n <- length(m)/monthDays[8]
    datW[(jYr*12)+8] <- n
    dayTot <- dayTot + monthDays[8] # September start day
    # ---- September
    m  <- which(dat[dayTot:(dayTot+monthDays[9]-1)] == 1)
    n <- length(m)/monthDays[9]
    datW[(jYr*12)+9] <- n
    dayTot <- dayTot + monthDays[9] # October start day
    # ---- October
    m  <- which(dat[dayTot:(dayTot+monthDays[10]-1)] == 1)
    n <- length(m)/monthDays[10]
    datW[(jYr*12)+10] <- n
    dayTot <- dayTot + monthDays[10] # November start day
    # ---- November
    m  <- which(dat[dayTot:(dayTot+monthDays[11]-1)] == 1)
    n <- length(m)/monthDays[11]
    datW[(jYr*12)+11] <- n
    dayTot <- dayTot + monthDays[11] # December start day
    # ---- December
    if (i == endyr){
      m  <- which(dat[dayTot:length(dat)] == 1)
    } else {
      m  <- which(dat[dayTot:(dayTot+monthDays[12]-1)] == 1)
    }
    n <- length(m)/monthDays[12]
    datW[(jYr*12)+12] <- n
    dayTot <- dayTot + monthDays[12] # January start day
    # ---
    jYr <- jYr + 1 
  }
  return(datW)
  
}
month_avg <- function(dat, years, leap){
  # About: This function will take a daily dataset and convert it into a monthly 
  #        time series. The number of occurances will be added for each month, 
  #        then the value will be divided by the number of days in the month. If
  #        there is no occurance the value will remain 0.
  #
  # dat : array of daily variables
  # years : c(start year, end year)
  # leap : TRUE or FALSE
  
  # Variables ------------------------------------------------------------------
  datM <- array(0,dim = (12* (years[2] - years[1] + 1)))
  monthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  dayTot <- 1;     jYr <- 0
  
  # Calculations ---------------------------------------------------------------
  for (i in startyr:endyr){
    if (i %% 4 == 0 & i != 2100 & leap == TRUE){
      leapYr <- 1
    } else { leapYr <- 0}
    # ---- January
    m  <- mean(dat[dayTot:(dayTot+monthDays[1]-1)], na.rm = TRUE)
    datM[(jYr*12)+1] <- m
    dayTot <- dayTot + monthDays[1] # February start day
    # ---- February
    m  <- mean(dat[dayTot:(dayTot+monthDays[2]-1)], na.rm = TRUE)
    datM[(jYr*12)+2] <- m
    dayTot <- dayTot + monthDays[2] # March start day
    # ---- March
    m  <- mean(dat[dayTot:(dayTot+monthDays[3]-1)], na.rm = TRUE)
    datM[(jYr*12)+3] <- m
    dayTot <- dayTot + monthDays[3] # April start day
    # ---- April
    m  <- mean(dat[dayTot:(dayTot+monthDays[4]-1)], na.rm = TRUE)
    datM[(jYr*12)+4] <- m
    dayTot <- dayTot + monthDays[4] # May start day
    # ---- May
    m  <- mean(dat[dayTot:(dayTot+monthDays[5]-1)], na.rm = TRUE)
    datM[(jYr*12)+5] <- m
    dayTot <- dayTot + monthDays[5] # June start day
    # ---- June
    m  <- mean(dat[dayTot:(dayTot+monthDays[6]-1)], na.rm = TRUE)
    datM[(jYr*12)+6] <- m
    dayTot <- dayTot + monthDays[6] # July start day
    # ---- July
    m  <- mean(dat[dayTot:(dayTot+monthDays[7]-1)], na.rm = TRUE)
    datM[(jYr*12)+7] <- m
    dayTot <- dayTot + monthDays[7] # August start day
    # ---- August
    m  <- mean(dat[dayTot:(dayTot+monthDays[8]-1)], na.rm = TRUE)
    datM[(jYr*12)+8] <- m
    dayTot <- dayTot + monthDays[8] # September start day
    # ---- September
    m  <- mean(dat[dayTot:(dayTot+monthDays[9]-1)], na.rm = TRUE)
    datM[(jYr*12)+9] <- m
    dayTot <- dayTot + monthDays[9] # October start day
    # ---- October
    m  <- mean(dat[dayTot:(dayTot+monthDays[10]-1)], na.rm = TRUE)
    datM[(jYr*12)+10] <- m
    dayTot <- dayTot + monthDays[10] # November start day
    # ---- November
    m  <- mean(dat[dayTot:(dayTot+monthDays[11]-1)], na.rm = TRUE)
    datM[(jYr*12)+11] <- m
    dayTot <- dayTot + monthDays[11] # December start day
    # ---- December
    if (i == endyr){
      m  <- mean(dat[dayTot:length(dat)], na.rm = TRUE)
    } else {
      m  <- mean(dat[dayTot:(dayTot+monthDays[12]-1)], na.rm = TRUE)
    }
    datM[(jYr*12)+12] <- m
    dayTot <- dayTot + monthDays[12] # January start day
    # ---
    jYr <- jYr + 1 
  }
  return(datM)
}
threshold_exceed <- function(X, opp){
  # This function will calculate the exceed of the time series (X) and  
  #     operator (opp) given. The first variable of the time series is the
  #     threshold to be used for the time series.The default threshold is 
  #     greater than or equal to.
  #
  # X   vector of Threshold [1] and time series values
  # opp 1: >= 2: <= 3 Two thresholds both <= <=
  
  # Variables ------------------------------------------------------------------
  threshold <- X[1]
  X <- X[-1]
  datE <- array(0, dim=length(X))
  n <- NULL
  # Calculate the exceedance of the given time series --------------------------
  if (opp == 1){
    m <- which(X >= threshold)
  } else if (opp == 2){
    m <- which(X <= threshold)
  } else if (opp == 3){
    # threshold == higher threshold
    threshold2 <- X[1]
    X <- X[-1]
    datE <- array(0, dim=length(X))
    m <- which(X <= threshold)
    n <- which(X <= threshold2)
  } else {
    m <- which(X >= threshold)
  }
  datE[m] <- 1
  datE[n] <- 2
  
  return(datE)
}

# Part III Heatwaves ###########################################################
# . 3.1 Opening the Files ------------------------------------------------------
if (var == 'tasmax'| var == 'tasmin'){
  A <- Sys.time()
  print(paste0('Starting to open temperature files at: ',A))
  datWaves <- read_csv(paste0(fileloc1, loc1, loc2, 'WAVES_DAY_', var,
                              mFile, startyr,'-',endyr,'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
  lonlat <- datWaves[,1:2]
  datWaves <- datWaves[,3:ncol(datWaves)]
  # . . 3.1.2 Formatting ##
  datWaves[datWaves > 0] <- 1
  # . 3.2 Formatting into monthly values ---------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the monthly Temperature Wave at: ',B))
  datMonth <- apply(datWaves, MARGIN = 1, FUN = month_wave, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datMonth <- cbind(lonlat, datMonth)
  colnames(datMonth) <- c('lon', 'lat', month)
  write.csv(datMonth, file=paste0(fileloc1, loc1, loc2, 'WAVES_MONTH_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}

# Part IV Extreme Precipitation ################################################
# . 4.1 Opening the Files ------------------------------------------------------
if (var == 'pr'){
  A <- Sys.time()
  print(paste0('Starting to open precipitation file at: ',A))
  datWaves <- read_csv(paste0(fileloc1, loc1, loc2, 'EXCEED_DAY_', var,
                              mFile, startyr,'-',endyr,'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
  lonlat <- datWaves[,1:2]
  datWaves <- datWaves[,3:ncol(datWaves)]
  # . . 4.1.2 Formatting ##
  datWaves[datWaves > 0] <- 1
  # . 4.2 Formatting into monthly values #########################################
  B <- Sys.time()
  print(paste0('Starting to calculate the Temperature Wave at: ',B))
  datMonth <- apply(datWaves, MARGIN = 1, FUN = month_wave, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datMonth <- cbind(lonlat, datMonth)
  colnames(datMonth) <- c('lon', 'lat', month)
  write.csv(datMonth, file=paste0(fileloc1, loc1, loc2, 'WAVES_MONTH_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}

# Part V Drought ###############################################################
# . 5.1 Flash Drought Opening Files --------------------------------------------
# This section will be to analyze the occurrence of flash drought (FD) at the 
# monthly scale.
if (var == 'mrsos'){
  A <- Sys.time()
  print(paste0('Starting to open WAVES FD soil moisture file at: ',A))
  datWaves <- read_csv(paste0(fileloc1, loc1, loc2, 'WAVES_DAY_', var,
                              mFile, startyr,'-',endyr,'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
  lonlat <- datWaves[,1:2]
  datWaves <- datWaves[,3:ncol(datWaves)]
  # . . 5.1.2 Formatting ##
  datWaves[datWaves > 0] <- 1
  # . 5.2 Formatting into monthly values ---------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the monthly Flash Drought Wave at: ',B))
  datMonth <- apply(datWaves, MARGIN = 1, FUN = month_wave, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datMonth <- cbind(lonlat, datMonth)
  colnames(datMonth) <- c('lon', 'lat', month)
  write.csv(datMonth, file=paste0(fileloc1, loc1, loc2, 'WAVES_MONTH_FD_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  print(paste0('Finished calculating monthly flash drought: ',B))
}

# . 5.3 Drought Opening Files --------------------------------------------------
if (var == 'mrsos'){
  A <- Sys.time()
  print(paste0('Starting to open org mrsos file at: ',A))
  datOrg <- read_csv(paste0(fileloc1, loc1, loc2, 'ORG_DAY_', var,
                            mFile, startyr,'-',endyr,'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
  lonlat <- datOrg[,1:2]
  datOrg <- datOrg[,3:ncol(datOrg)]
  # . 5.4 Convert to monthly Drought -------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the Monthly average at: ',B))
  datMonth <- apply(datOrg, MARGIN = 1, FUN = month_avg, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datMonth <- cbind(lonlat, datMonth)
  colnames(datMonth) <- c('lon', 'lat', month)
  write.csv(datMonth, file=paste0(fileloc1, loc1, loc2, 'ORG_MONTH_', var,
                                mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  # . 5.5 Monthly Thresholds ---------------------------------------------------
  if (loc1 == 'CMIP6_historical/'){
    # . . 5.5.1 Calculating the thresholds ## 
    perc <- apply(datMonth[,3:ncol(datMonth)], 
                  MARGIN = 1, quantile, c(0.40, 0.20, 0.10), 
                  na.rm = TRUE) %>%  t()
    Tmean <- apply(datMonth[,3:ncol(datMonth)], 
                   MARGIN = 1, mean, na.rm = TRUE) %>% tibble()
    Tmedian <- apply(datMonth[,3:ncol(datMonth)], MARGIN = 1, median, na.rm = TRUE) %>% tibble()
    Tmax <- apply(datMonth[,3:ncol(datMonth)], MARGIN = 1, max, na.rm = TRUE) %>% tibble()
    Tmin <- apply(datMonth[,3:ncol(datMonth)], MARGIN = 1, min, na.rm = TRUE) %>% tibble()
    TSd <- apply(datMonth[,3:ncol(datMonth)], MARGIN = 1, sd, na.rm = TRUE) %>% tibble()
    # . . 5.5.2 Formating the data ##
    datThresh <- cbind(lonlat, perc, Tmean, Tmedian, Tmax, Tmin, TSd)
    colnames(datThresh) <- c('lon', 'lat', 'P40', 'P20', 'P10', 
                             'PMean', 'PMedian', 'PMax', 'PMin', 'PSd')
    datThresh$PMean[is.nan(datThresh$PMean)] <- NA
    datThresh$PMax[datThresh$PMax == -Inf] <- NA
    datThresh$PMin[datThresh$PMin == Inf] <- NA
    # . . 5.5.3 Writting the Thresholds ##
    write.csv(datThresh, file=paste0(fileloc1,'CMIP6_historical/',loc2,
                                     'THRESHOLD_MONTH_',var, 
                                     mFile,startyr,'-',endyr,'.csv'),
              row.names = FALSE)
    rm(perc, Tmean, Tmedian, Tmax, Tmin, TSd)
  } else {
    # . . 5.5.4 Opening the thresholds ##
    if (mNum == 1 | mNum == 6 | mNum == 7 | mNum == 8){
      x <- '_historical_r1i1p1f1_gn'
    } else if (mNum == 2){
      x <- '_historical_r1i1p1f1_gr'
    } else if (mNum == 3){
      x <- '_esm-hist_r1i1p1f1_gr1'
    } else if (mNum == 4 | mNum == 5){
      x <- '_historical_r1i1p1f1_gr1'
    } else { print(paste0('Model Number ', mNum, ' not reconized.'))}
    datThresh <- read_csv(paste0(fileloc1, 'CMIP6_historical/', loc2, 
                                 'THRESHOLD_MONTH_', var, '_day_', a, x, '_',
                                 1980, '-', 2010, '.csv'),
                          col_names = TRUE, cols(.default = col_double()))
  }
  B <- Sys.time()
  print(paste0('Finished calculating or opening the Monthly values at: ', B))
  # . 5.4 Monthly Drought ------------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the monthly drought exceedance at: ', B))
  lonlat <- datThresh[,1:2]
  days <- colnames(datThresh[,3:ncol(datThresh)])
  datThresh <- cbind(datThresh$P20, datMonth[,3:ncol(datMonth)])
  exceed <- apply(X = datThresh, MARGIN=1, FUN=threshold_exceed, opp=2) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat', month)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'WAVES_MONTH_D_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}

# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating all the monthly values for ',
             var,'. End time: ',B, ' Total time elapsed: ', B-A))
