# P4_Weekly_Waves.R
# About: This program will open the exceedance file for the selected variable 
#        and calculate a 'wave' of when the exceedance occurs.
#
# Inputs: WAVES DAY(tasmax), EXCEED (pr), ORG DAY (mrsos)
# Outputs: EXCEED_WEEK
#
# T. A. Schillerberg
#               Jan. 2023
#      Updated: May. 2023

# Mac

# Office Computer
setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# HPC
# fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)

# Part I Variables To Change ###################################################
# var <- c('tasmax', 'tasmin','pr', 'mrsos')[as.numeric('model_var')] # Bash script
# mNum <- as.numeric('model_num') # Bash script
var <- c('tasmax', 'tasmin','pr','mrsos') [4]
mNum <- 1 # Select a model (1-4)
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
    week <- 102908:104519 
  } else { week <- 6760:8371 }
} else if (startyr == 2040){
  if (mNum == 8) { 
    week <- 106028:107639
  } else { week <- 9880:11491 }
} else {
  if (mNum == 8) { 
    week <- 107588:109199 
  } else { week <- 11440:13051 }
}

print('')
print(paste0('Model: ',loc2))
print(paste0('Variable: ', var))
print('Rscript: P3_Week_Waves.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))

# Part II Functions ############################################################
week_wave <- function(dat, years, leap){
  # About: This function will take a daily data set and convert it into a monthly 
  #        time series. The number of occurrences will be added for each month, 
  #        then the value will be divided by the number of days in the month. If
  #        there is no occurrence the value will remain 0.
  #
  # dat : array of binary variables
  # years : c(start year, end year)
  # leap : TRUE or FALSE
  
  # Variables ------------------------------------------------------------------
  datW <- array(0,dim = (52 * (years[2] - years[1] + 1)))
  dayTot <- 1;     jYr <- 0
  
  # Calculations ---------------------------------------------------------------
  for (i in years[1]:years[2]){
    if (i %% 4 == 0 & i != 2100 & leap == TRUE){
      leapYr <- 1
    } else { leapYr <- 0}
    j <- 1
    repeat {
      if (j == 52){
        m <- which(dat[dayTot:(dayTot + 6 + 1 + leapYr)] == 1)
        n <- length(m) / (7 + 1 + leapYr)
        datW[(jYr*52)+j] <- n
        dayTot <- dayTot + 7 + 1 + leap # Next year start day
        jYr <- jYr + 1
        break
      } else {
        m <- which(dat[dayTot:(dayTot + 6 )] == 1)
        n <- length(m) / (7)
        datW[(jYr*52)+j] <- n
        dayTot <- dayTot + 7 # Next week start day
        j <- j + 1
      }
    }
  }
  return(datW)
}
week_avg <- function(dat, years, leap){
  # About: This function will take a daily dataset and convert it into a weekly 
  #        time series. The number of occurances will be added for each week, 
  #        then the value will be divided by the number of days in the week. If
  #        there is no occurance the value will remain 0.
  #
  # dat : array of daily variables
  # years : c(start year, end year)
  # leap : TRUE or FALSE
  
  # Variables ------------------------------------------------------------------
  datW <- array(0,dim = (52 * (years[2] - years[1] + 1)))
  dayTot <- 1;     jYr <- 0
  
  # Calculations ---------------------------------------------------------------
  for (i in years[1]:years[2]){
    if (i %% 4 == 0 & i != 2100 & leap == TRUE){
      leapYr <- 1
    } else { leapYr <- 0}
    j <- 1
    repeat {
      if (j == 52){
        m <- mean(dat[dayTot:(dayTot + 6 + 1 + leapYr)], na.rm = TRUE)
        datW[(jYr*52)+j] <- m
        dayTot <- dayTot + 7 + 1 + leap # Next year start day
        jYr <- jYr + 1
        break
      } else {
        m <- mean(dat[dayTot:(dayTot + 6 )], na.rm = TRUE)
        datW[(jYr*52)+j] <- m
        dayTot <- dayTot + 7 # Next week start day
        j <- j + 1
      }
    }
  }
  return(datW)
}
threshold_exceed <- function(X, opp){
  # This function will calculate the exceedance of the time series (X) and  
  #     opperator (opp) given. The first variable of the time series is the
  #     threshold to be used for the time series.THe default threshold is 
  #     greater than or equal to.
  #
  # X   vector of Threshold [1] and timeseries values
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
  # . 3.2 Formatting into monthly values #########################################
  B <- Sys.time()
  print(paste0('Starting to calculate the weekly Temperature Wave at: ',B))
  datWeek <- apply(datWaves, MARGIN = 1, FUN = week_wave, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datWeek <- cbind(lonlat, datWeek)
  colnames(datWeek) <- c('lon','lat',week)
  write.csv(datWeek, file=paste0(fileloc1, loc1, loc2, 'WAVES_WEEK_', var,
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
  print(paste0('Starting to calculate the weekly at: ',B))
  datWeek <- apply(datWaves, MARGIN = 1, FUN = week_wave, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datWeek <- cbind(lonlat, datWeek)
  colnames(datWeek) <- c('lon','lat',week)
  write.csv(datWeek, file=paste0(fileloc1, loc1, loc2, 'WAVES_WEEK_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}
# Part V Drought ###############################################################
# . 5.1 Flash Drought Opening Files --------------------------------------------
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
  # . 5.2 Formatting into weekly values ----------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the week Flash Drought Wave at: ',B))
  datWeek <- apply(datWaves, MARGIN = 1, FUN = week_wave, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datWeek <- cbind(lonlat, datWeek)
  colnames(datWeek) <- c('lon', 'lat', week)
  write.csv(datWeek, file=paste0(fileloc1, loc1, loc2, 'WAVES_WEEK_FD_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  print(paste0('Finished calculating weekly flash drought: ',B))
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
  # . 5.4 Convert to weekly Drought --------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the Weekly average at: ',B))
  datWeek <- apply(datOrg, MARGIN = 1, FUN = week_avg, 
                    years = c(startyr, endyr), leap = mLeap) %>%
    t()
  datWeek <- cbind(lonlat, datWeek)
  colnames(datWeek) <- c('lon', 'lat', week)
  write.csv(datWeek, file=paste0(fileloc1, loc1, loc2, 'ORG_WEEK_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  # . 5.5 Weekly Thresholds ---------------------------------------------------
  if (loc1 == 'CMIP6_historical/'){
    # . . 5.5.1 Calculating the thresholds ## 
    perc <- apply(datWeek[,3:ncol(datWeek)], 
                  MARGIN = 1, quantile, c(0.40, 0.20, 0.10), 
                  na.rm = TRUE) %>%  t()
    Tmean <- apply(datWeek[,3:ncol(datWeek)], 
                   MARGIN = 1, mean, na.rm = TRUE) %>% tibble()
    Tmedian <- apply(datWeek[,3:ncol(datWeek)], MARGIN = 1, median, na.rm = TRUE) %>% tibble()
    Tmax <- apply(datWeek[,3:ncol(datWeek)], MARGIN = 1, max, na.rm = TRUE) %>% tibble()
    Tmin <- apply(datWeek[,3:ncol(datWeek)], MARGIN = 1, min, na.rm = TRUE) %>% tibble()
    TSd <- apply(datWeek[,3:ncol(datWeek)], MARGIN = 1, sd, na.rm = TRUE) %>% tibble()
    # . . 5.5.2 Formating the data ##
    datThresh <- cbind(lonlat, perc, Tmean, Tmedian, Tmax, Tmin, TSd)
    colnames(datThresh) <- c('lon', 'lat', 'P40', 'P20', 'P10', 
                             'PMean', 'PMedian', 'PMax', 'PMin', 'PSd')
    datThresh$PMean[is.nan(datThresh$PMean)] <- NA
    datThresh$PMax[datThresh$PMax == -Inf] <- NA
    datThresh$PMin[datThresh$PMin == Inf] <- NA
    # . . 5.5.3 Writting the Thresholds ##
    write.csv(datThresh, file=paste0(fileloc1,'CMIP6_historical/',loc2,
                                     'THRESHOLD_WEEK_',var, 
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
                                 'THRESHOLD_WEEK_', var, '_day_', a, x, '_',
                                 1980, '-', 2010, '.csv'),
                          col_names = TRUE, cols(.default = col_double()))
  }
  B <- Sys.time()
  print(paste0('Finished calculating or opening the weekly threshold values at: ', B))
  # . 5.4 Weekly Drought -------------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the weekly drought exceedance at: ', B))
  lonlat <- datThresh[,1:2]
  days <- colnames(datThresh[,3:ncol(datThresh)])
  datThresh <- cbind(datThresh$P20, datWeek[,3:ncol(datWeek)])
  exceed <- apply(X = datThresh, MARGIN=1, FUN=threshold_exceed, opp=2) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat', week)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'WAVES_WEEK_D_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}

# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating all the weekly values for ',
             var,'. End time: ',B, 'Total time elapsed: ', B-A))