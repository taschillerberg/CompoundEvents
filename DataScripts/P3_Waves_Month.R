# P5_Month_Waves.R
# About: This program will open files for the selected variable and calculate 
#        the monthly values.
#
# Inputs: WAVES DAY(tasmax), EXCEED (pr), ORG DAY (mrsos)
# Outputs: EXCEED_MONTHLY
#
# T. A. Schillerberg
#               NOV. 2022
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
month <- 1560:1931 # Historical
# month <- 2280:2651 # 2040-2070
# month <- 2640:3011 # 2070-2100

print(paste0('Model: ',loc2))
print(paste0('Variable: ', var))
print('Rscript: P5_Month_Waves.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))

# Part II Functions ############################################################
month_calc <- function(ID, dat){
  # About: This function will take a daily dataset and convert it into a monthly 
  #        time series. The number of occurances will be added for each month, 
  #        then the value will be divided by the number of days in the month. If
  #        there is no occurance the value will remain 0.
  #
  # ID : row location
  # dat, X : array of binary variables
  # dat, startyr : start year
  # dat, endyr : end year
  
  # Variables ------------------------------------------------------------------
  X <- dat[["X"]][ID,]
  startyr <- dat[['startyr']]
  endyr <- dat[['endyr']]
  dat <- unlist(c(0,X))  
  datM <- array(0,dim = (12* (endyr - startyr + 1)))
  monthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  dayTot <- 1;     jYr <- 0

  # Calculations ---------------------------------------------------------------
  for (i in startyr:endyr){
    if (i %% 4 == 0 & i != 2100){
      leap <- 1
    } else { leap <- 0}
    # dayYr <- sum(monthDays) + leap # Number of days in the year
    # ---- January
    m  <- which(dat[dayTot:(dayTot+monthDays[1]-1)] == 1)
    n <- length(m)/monthDays[1]
    datM[(jYr*12)+1] <- n
    dayTot <- dayTot + monthDays[1] # February start day
    # ---- February
    m  <- which(dat[dayTot:(dayTot+monthDays[2]-1)] == 1)
    n <- length(m)/monthDays[2]
    datM[(jYr*12)+2] <- n
    dayTot <- dayTot + monthDays[2] # March start day
    # ---- March
    m  <- which(dat[dayTot:(dayTot+monthDays[3]-1)] == 1)
    n <- length(m)/monthDays[3]
    datM[(jYr*12)+3] <- n
    dayTot <- dayTot + monthDays[3] # April start day
    # ---- April
    m  <- which(dat[dayTot:(dayTot+monthDays[4]-1)] == 1)
    n <- length(m)/monthDays[4]
    datM[(jYr*12)+4] <- n
    dayTot <- dayTot + monthDays[4] # May start day
    # ---- May
    m  <- which(dat[dayTot:(dayTot+monthDays[5]-1)] == 1)
    n <- length(m)/monthDays[5]
    datM[(jYr*12)+5] <- n
    dayTot <- dayTot + monthDays[5] # June start day
    # ---- June
    m  <- which(dat[dayTot:(dayTot+monthDays[6]-1)] == 1)
    n <- length(m)/monthDays[6]
    datM[(jYr*12)+6] <- n
    dayTot <- dayTot + monthDays[6] # July start day
    # ---- July
    m  <- which(dat[dayTot:(dayTot+monthDays[7]-1)] == 1)
    n <- length(m)/monthDays[7]
    datM[(jYr*12)+7] <- n
    dayTot <- dayTot + monthDays[7] # August start day
    # ---- August
    m  <- which(dat[dayTot:(dayTot+monthDays[8]-1)] == 1)
    n <- length(m)/monthDays[8]
    datM[(jYr*12)+8] <- n
    dayTot <- dayTot + monthDays[8] # September start day
    # ---- September
    m  <- which(dat[dayTot:(dayTot+monthDays[9]-1)] == 1)
    n <- length(m)/monthDays[9]
    datM[(jYr*12)+9] <- n
    dayTot <- dayTot + monthDays[9] # October start day
    # ---- October
    m  <- which(dat[dayTot:(dayTot+monthDays[10]-1)] == 1)
    n <- length(m)/monthDays[10]
    datM[(jYr*12)+10] <- n
    dayTot <- dayTot + monthDays[10] # November start day
    # ---- November
    m  <- which(dat[dayTot:(dayTot+monthDays[11]-1)] == 1)
    n <- length(m)/monthDays[11]
    datM[(jYr*12)+11] <- n
    dayTot <- dayTot + monthDays[11] # December start day
    # ---- December
    if (i == endyr){
      m  <- which(dat[dayTot:length(dat)] == 1)
    } else {
      m  <- which(dat[dayTot:(dayTot+monthDays[12]-1)] == 1)
    }
    n <- length(m)/monthDays[12]
    datM[(jYr*12)+12] <- n
    dayTot <- dayTot + monthDays[12] # January start day
    # ---
    jYr <- jYr + 1 
  }
  return(datM)
  
}
month_avg <- function(ID, dat){
  # About: This function will take a daily dataset and convert it into a monthly 
  #        time series. The number of occurances will be added for each month, 
  #        then the value will be divided by the number of days in the month. If
  #        there is no occurance the value will remain 0.
  #
  # ID : row location
  # dat, X : array of binary variables
  # dat, startyr : start year
  # dat, endyr : end year
  
  # Variables ------------------------------------------------------------------
  X <- dat[["X"]][ID,]
  startyr <- dat[['startyr']]
  endyr <- dat[['endyr']]
  dat <- unlist(c(0,X))  
  datM <- array(0,dim = (12* (endyr - startyr + 1)))
  monthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  dayTot <- 1;     jYr <- 0
  
  # Calculations ---------------------------------------------------------------
  for (i in startyr:endyr){
    if (i %% 4 == 0 & i != 2100){
      leap <- 1
    } else { leap <- 0}
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
    'startyr'=startyr,
    'endyr'=endyr
  )
  datMonth <- parallel::mclapply(X=ID, FUN=,month_calc, dat=dat, mc.cores=core) %>%
    unlist() %>%
    matrix(ncol = length(month), byrow = TRUE)
  datMonth <- cbind(lonlat, datMonth)
  colnames(datMonth) <- c('lon','lat',month)
  write.csv(datMonth, file=paste0(fileloc1, loc1, loc2, 'EXCEED_MONTH_', var,
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
  ID <- 1:nrow(datWaves)
  dat <- list(
    'X' = datWaves,
    'startyr'=startyr,
    'endyr'=endyr
  )
  datMonth <- parallel::mclapply(X=ID, FUN=month_calc, dat=dat, mc.cores=core) %>%
    unlist() %>%
    matrix(ncol = length(month), byrow = TRUE)
  datMonth <- cbind(lonlat, datMonth)
  colnames(datMonth) <- c('lon','lat',month)
  write.csv(datMonth, file=paste0(fileloc1, loc1, loc2, 'EXCEED_MONTH_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}

# Part V Drought ###############################################################
# . 5.1 Opening the Files ------------------------------------------------------
if (var == 'mrsos'){
  A <- Sys.time()
  print(paste0('Starting to open precipitation file at: ',A))
  datOrg <- read_csv(paste0(fileloc1, loc1, loc2, 'ORG_DAY_', var,
                            mFile, startyr,'-',endyr,'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
  lonlat <- datOrg[,1:2]
  datOrg <- datOrg[,3:ncol(datOrg)]
  # . 5.2 Converting to monthly --------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the Monthly values at: ',B))
  ID <- 1:nrow(datOrg)
  dat <- list(
    'X' = datOrg,
    'startyr'=startyr,
    'endyr'=endyr
  )
  datVar <- parallel::mclapply(X=ID, FUN=,month_avg, dat=dat, mc.cores=core) %>%
    unlist() %>%
    matrix(ncol = length(month), byrow = TRUE)
  datVar <- cbind(lonlat, datVar)
  colnames(datVar) <- c('lon','lat',month)
  write.csv(datVar, file=paste0(fileloc1, loc1, loc2, 'ORG_MONTH_', var,
                                mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  
  # . 5.3 Monthly Thresholds -----------------------------------------------------
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
                                     'THRESHOLD_MONTH',var, 
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
    a <-strsplit(loc2,'/') %>% unlist() 
    datThresh <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2,
                                 'THRESHOLD_MONTH',var,'_day_',a,
                                 '_historical_r1i1p1f1_',x,'_',1980,'-',2010,'.csv'),
                          col_names = TRUE, cols(.default = col_double()))
  }
  B <- Sys.time()
  print(paste0('Finished calculating or opening the Monthly values at: ', B))
  
  # . 5.4 Monthly Drought ------------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the exceedance at: ', B))
  lonlat <- datVar[,1:2]
  days <- colnames(datVar[,3:ncol(datVar)])
  datVar <- cbind(datThresh$P20, datVar[,3:ncol(datVar)])
  exceed <- apply(X = datVar, MARGIN=1, FUN=threshold_exceed, opp=2) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat',month)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'EXCEED_MONTH_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}

# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating all the monthly values for ',
             var,'. End time: ',B, 'Total time elapsed: ', B-A))
