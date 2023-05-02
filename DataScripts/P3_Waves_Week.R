# P4_Weekly_Waves.R
# About: This program will open the exceedance file for the selected 
#       variable and calculate a 'wave' of when the exceedance occurs.
#
# Inputs: WAVES DAY(tasmax), EXCEED (pr), ORG DAY (mrsos)
# Outputs: EXCEED_WEEK
#
# T. A. Schillerberg
#               Jan. 2023
#      Updated: Jan. 2023

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
# var <- c('tasmax', 'tasmin', 'mrsos')[as.numeric('model_var')] # Bash script
# mNum <- as.numeric('model_num') # Bash script
var <- c('tasmax', 'tasmin','mrsos') [3]
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
startyr <- 1980
endyr <- 2010
core <- 23
week <- 6783:8400 # Historical
# week <- 9914:11531 # 2040-2070
# week <- 11479:13096 # 2070-2100

print(paste0('Model: ',loc2))
print(paste0('Variable: ', var))
print('Rscript: P4_Week_Waves.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))

# Part II Functions ############################################################
week_calc <- function(ID, dat){
  # About: This function will take a daily dataset and convert it into a weekly 
  #        time series. The number of occurances will be added for each week, 
  #        then the value will be divided by the number of days in the week. If
  #        there is no occurance the value will remain 0.
  #
  # ID : row location
  # dat, X : array of binary variables
  # dat, month : sequence of months
  
  # Variables ------------------------------------------------------------------
  X <- dat[["X"]][ID,]
  datW <- array(0,dim = dat[["weeks"]])
  dat <- unlist(c(0,X))
  i <- 1;     w <- 1
  
  # Calculations ---------------------------------------------------------------
  repeat{
    if ((i + 6) > length(dat)){
      m <- which(dat[i:length(dat)] == 1)
      n <- length(m) / (length(dat) - i)
      datW[w] <- n
      break
    }
    m <- which(dat[i:(i+6)] == 1)
    n <- length(m) / 7
    datW[w] <- n
    i <- i+7; w <- w + 1
  }
  
  return(datW)
}
week_avg <- function(ID, dat){
  # About: This function will take a daily dataset and convert it into a weekly 
  #        time series. The number of occurances will be added for each week, 
  #        then the value will be divided by the number of days in the week. If
  #        there is no occurance the value will remain 0.
  #
  # ID : row location
  # dat, X : array of binary variables
  # dat, weeks : the number of weeks in the time period
  
  # Variables ------------------------------------------------------------------
  X <- dat[["X"]][ID,]
  datW <- array(0,dim = dat[["weeks"]])
  dat <- unlist(c(0,X))
  i <- 1;     w <- 1 
  
  # Calculations ---------------------------------------------------------------
  repeat{
    if ((i + 6) > length(dat)){
      m <- mean(dat[i:length(dat)], na.rm = TRUE)
      datW[w] <- m
      break
    }
    m <- mean(dat[i:(i+6)], na.rm = TRUE)
    datW[w] <- m
    i <- i+7; w <- w + 1
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
  print(paste0('Starting to calculate the Temperature Wave at: ',B))
  ID <- 1:nrow(datWaves)
  dat <- list(
    'X' = datWaves,
    'weeks'=length(week)
  )
  datWeek <- parallel::mclapply(X=ID, FUN=,week_calc, dat=dat, mc.cores=core) %>%
    unlist() %>%
    matrix(ncol = length(week), byrow = TRUE)
  datWeek <- cbind(lonlat, datWeek)
  colnames(datWeek) <- c('lon','lat',week)
  write.csv(datWeek, file=paste0(fileloc1, loc1, loc2, 'EXCEED_WEEK_', var,
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
  ID <- 1:nrow(datWaves)
  dat <- list(
    'X' = datWaves,
    'weeks'=length(week)
  )
  datWeek <- parallel::mclapply(X=ID, FUN=week_calc, dat=dat, mc.cores=core) %>%
    unlist() %>%
    matrix(ncol = length(week), byrow = TRUE)
  datWeek <- cbind(lonlat, datWeek)
  colnames(datWeek) <- c('lon','lat',week)
  write.csv(datWeek, file=paste0(fileloc1, loc1, loc2, 'EXCEED_WEEK_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}
# Part V Drought ###############################################################
# . 5.1 Opening the Files ------------------------------------------------------
if (var == 'mrsos'){
  A <- Sys.time()
  print(paste0('Starting to open drought file at: ',A))
  datOrg <- read_csv(paste0(fileloc1, loc1, loc2, 'ORG_DAY_', var,
                            mFile, startyr,'-',endyr,'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
  lonlat <- datOrg[,1:2]
  datOrg <- datOrg[,3:ncol(datOrg)]
  # . 5.2 Converting to monthly --------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the Weekly values at: ',B))
  ID <- 1:nrow(datOrg)
  dat <- list(
    'X' = datOrg,
    'weeks'=length(week)
  )
  datVar <- parallel::mclapply(X=ID, FUN=week_avg, dat=dat, mc.cores=core) %>%
    unlist() %>%
    matrix(ncol = length(week), byrow = TRUE)
  datVar <- cbind(lonlat, datVar)
  colnames(datVar) <- c('lon','lat',week)
  write.csv(datVar, file=paste0(fileloc1, loc1, loc2, 'ORG_WEEK_', var,
                                mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  
  # . 5.3 Weekly Thresholds -----------------------------------------------------
  if (loc1 == 'CMIP6_historical/'){
    # . . 5.3.1 Calculating the thresholds ## 
    perc <- apply(datVar[,3:ncol(datVar)], 
                  MARGIN = 1, quantile, c(0.40, 0.20, 0.10), 
                  na.rm = TRUE) %>%  t()
    Tmean <- apply(datVar[,3:ncol(datVar)], 
                   MARGIN = 1, mean, na.rm = TRUE) %>% tibble()
    Tmedian <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, median, na.rm = TRUE) %>% tibble()
    Tmax <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, max, na.rm = TRUE) %>% tibble()
    Tmin <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, min, na.rm = TRUE) %>% tibble()
    TSd <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, sd, na.rm = TRUE) %>% tibble()
    # . . 5.3.2 Formating the data ##
    datThresh <- cbind(datVar[,1:2], perc, Tmean, Tmedian, Tmax, Tmin, TSd)
    colnames(datThresh) <- c('lon', 'lat', 'P40', 'P20', 'P10', 
                             'PMean', 'PMedian', 'PMax', 'PMin', 'PSd')
    datThresh$PMean[is.nan(datThresh$PMean)] <- NA
    datThresh$PMax[datThresh$PMax == -Inf] <- NA
    datThresh$PMin[datThresh$PMin == Inf] <- NA
    # . . 5.3.3 Writting the Thresholds ##
    write.csv(datThresh, file=paste0(fileloc1,'CMIP6_historical/',loc2,
                                     'THRESHOLD_WEEK',var, 
                                     mFile,startyr,'-',endyr,'.csv'),
              row.names = FALSE)
    rm(perc, Tmean, Tmedian, Tmax, Tmin, TSd)
  } else {
    # . . 5.3.4 Opening the thresholds ##
    if (mNum == 1){
      x <- 'gr'
    } else if (mNum == 2){
      x <- 'gr1'
    } else { x <- 'gn'}
    datThresh <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2,
                                 'THRESHOLD_WEEK',var,'_day_',a,
                                 '_historical_r1i1p1f1_',x,'_',1980,'-',2010,'.csv'),
                          col_names = TRUE, cols(.default = col_double()))
  }
  B <- Sys.time()
  print(paste0('Finished calculating or opening the Weekly values at: ', B))
  
  # . 5.4 Weekly Drought ------------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the exceedance at: ', B))
  lonlat <- datVar[,1:2]
  days <- colnames(datVar[,3:ncol(datVar)])
  datVar <- cbind(datThresh$P10, datVar[,3:ncol(datVar)])
  exceed <- apply(X = datVar, MARGIN=1, FUN=threshold_exceed, opp=2) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat',week)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'EXCEED_WEEK_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}


# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating all the weekly values for ',
             var,'. End time: ',B, 'Total time elapsed: ', B-A))