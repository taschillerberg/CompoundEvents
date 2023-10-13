# P3_Waves.R
# About: This program will open the exceed file for the selected 
#        variable and calculate a 'wave' of when the exceed occurs.
#        Time: 2hr for 1 variable - 8 Models
#     
# Inputs:  EXCEED_DAY
# Outputs: WAVES_DAY
#
# T. A. Schillerberg
#               Oct. 2022
#      Updated: Oct. 2023

# Mac
# setwd("~/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Code2")
# fileloc1 <- '~/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Data/'

# Office Computer
# setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
# fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# HPC
fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)

# Part I Variables To Change ###################################################
var <- c('tasmax', 'tasmin', 'mrsos')[as.numeric('model_var')] # Bash script
mNum <- as.numeric('model_num') # Bash script
# var <- c('tasmax', 'tasmin','mrsos') [3]
# mNum <- 1 # Select a model (1-8)
mFileH <- c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_',
           '_day_EC-Earth3_historical_r1i1p1f1_gr_',
           '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
           '_day_INM-CM4-8_historical_r1i1p1f1_gr1_',
           '_day_INM-CM5-0_historical_r1i1p1f1_gr1_',
           '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
           '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_',
           '_day_NorESM2-MM_historical_r1i1p1f1_gn_') [mNum]
mFile <- c('_day_CMCC-ESM2_ssp126_r1i1p1f1_gn_',
           '_day_EC-Earth3_ssp126_r1i1p1f1_gr_',
           '_day_GFDL-ESM4_ssp126_r1i1p1f1_gr1_',
           '_day_INM-CM4-8_ssp126_r1i1p1f1_gr1_',
           '_day_INM-CM5-0_ssp126_r1i1p1f1_gr1_',
           '_day_MPI-ESM1-2-HR_ssp126_r1i1p1f1_gn_',
           '_day_MRI-ESM2-0_ssp126_r1i1p1f1_gn_',
           '_day_NorESM2-MM_ssp126_r1i1p1f1_gn_')[mNum]
# mFile <- c('_day_CMCC-ESM2_ssp585_r1i1p1f1_gn_',
#            '_day_EC-Earth3_ssp585_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_ssp585_r1i1p1f1_gn_')[mNum]
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/') [mNum]
startyr <- 2010
endyr <- 2040
scenario <- 2 #1:3

print(paste0('Model: ',loc2))
print(paste0('Variable: ', var))
print('Rscript: P3_Waves.R')
print(paste0('Scenario: ', scenario, ' For the time period: ', startyr, '-', endyr))

# Part II Functions ############################################################
day_wave <- function(dat){
  # About: This function will calculate the wave period of exceedance and return 
  #        a time series of the same length composed of 0s and values <1 that 
  #        can be used to determine the length of the wave.
  #
  # dat: array of binary variables
  
  # Variables ------------------------------------------------------------------
  datW <- array(0,dim = length(dat))
  st1 <- c(0,1,1,1)
  
  # Calculations ---------------------------------------------------------------
  m <- which(dat == st1[1])
  if (length(m) != 0){
    # Removing m at the end where a 3 day wave would not be possible
    if(m[length(m)] == length(dat)){m <- m[-c(length(m))]}
    if(m[length(m)] == (length(dat)-1)) {m <- m[-c(length(m))]}
    if(m[length(m)] == (length(dat)-2)){m <- m[-c(length(m))]}
    # Finding where a wave exists
    n <- m[sapply(m, function(i) all(dat[i:(i+(length(st1)-1))] == st1))]
    # Finding the length of the heatwave
    i <- 1
    repeat{
      if(i == (length(n)+1) | is.na(n[i])) {  break  }
      if(i == length(n)| is.na(n[i+1])){
        m <- which(dat[(n[i]+1):(length(dat)-1)] == 0)
      } else {
        m <- which(dat[(n[i]+1):n[i+1]] == 0)
      }
      if (length(m) != 0){
        o <- sum(dat[(n[i]+1):(n[i] + m[1] -1)])
        datW[(n[i]):(n[i]+m[1]-2)] <- 1/o
      }
      i <- i + 1
    }
  }
  return(datW)
}
day_wave_flash <- function(dat){
  # About: This function will calculate the occurrence of flash droughts
  #
  # dat, X : array of tri-nary (0,1,2) variables
  
  # Variables ------------------------------------------------------------------
  datW <- array(0,dim = length(dat))
  st0<- c(2,1,2); st1 <- c(0,1); st2 <- c(0,2)
  
  # Calculations ---------------------------------------------------------------
  # correct any cases of 2,1,2 
  # --- shouldn't have to worry about since first occurrence of 0
  # m <- which(dat == st0[1])
  # if (length(m) != 0){
  #   # Removing m at the end where the sequence would not be possible
  #   if(m[length(m)] == length(dat))  {m <- m[-c(length(m))]}
  #   if(m[length(m)] == (length(dat)-1))  {m <- m[-c(length(m))] }
  #   # Finding where the sequence exists
  #   n <- m[sapply(m, function(i) all(dat[i:(i+(length(st0)-1))] == st0))]
  #   dat[n + 1] <- 2
  # } 
  
  # Find the occurrences of 0,1 aka potential starts of flash drought
  m <- which(dat == st1[1])
  if (length(m) != 0){
    # Removing m at the end where a wave would not be possible
    if(m[length(m)] == length(dat)){m <- m[-c(length(m))]}
    # Finding where a wave exists
    n <- m[sapply(m, function(i) all(dat[i:(i+(length(st1)-1))] == st1))]
    # Test the potential lengths of flash drought. If not longer than 14 days
    # convert to 0,2 to indicate the start of the flash drought
    i <- 1
    repeat{
      if (i == length(n) | is.na(n[i])){  break  }
      m <- which(dat[n[i]:(n[i]+13)] == 2)
      if (length(m) == 0) {
        i <- i + 1
      } else {
        # o <- dat[n[i] + m[1] - 1] # This is where the first 2 is located
        o <- m[1] - 2
        dat[n[i]+m[1]-2] <- 0
        dat[n[i]+m[1]-3] <- o
        # datW[(n[i]+m[length(m)]-i-1):(n[i]+m[1]-2)] <- 1
        i <- i + 1
      }
    }
  }
  
  # Find the occurrences of 0,2
  m <- which(dat == st2[1])
  if (length(m) != 0){
    # Removing m at the end where a wave would not be possible
    if(m[length(m)] == length(dat)){m <- m[-c(length(m))]}
    # Finding where a wave exists
    n <- m[sapply(m, function(i) all(dat[i:(i+(length(st2)-1))] == st2))]
    # Find the length of the flash drought by first finding the first occurrence 
    # of 0 after the first 2
    i <- 1
    repeat{
      if(i == (length(n)+1) | is.na(n[i])) {  break  }
      if(i == length(n) | is.na(n[i+1])){
        m <- which(dat[(n[i]+1):(length(dat)-1)] == 0)
      } else {
        m <- which(dat[(n[i]+1):n[i+1]] == 0)
      }
      if (length(m) != 0){
        o <- length(dat[(n[i]+1):(n[i] + m[1] -1)])
        p <- dat[n[i]-1]
        if (length(p) == 0) {p <- 0}
        datW[(n[i]+1):(n[i]+m[1]-1)] <- 1/(o + p)
      }
      i <- i +1
    }
  }
  
  # datW <- array(data=datW, dim = length(dat))
  return(datW)
}

# Part III Opening Files #######################################################
A <- Sys.time()
print(paste0('Starting to open the exceed file at: ',A))
if (var == 'tasmax'| var == 'tasmin'){
  if(scenario == 1){ mFile <- mFileH }
  loc1 <- loc1[scenario]
  datExceed <- read_csv(paste0(fileloc1, loc1, loc2, 'EXCEED_DAY_', var, mFile,
                               startyr,'-',endyr,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
  lonlat <- datExceed[,1:2]
  days <- colnames(datExceed[,3:ncol(datExceed)])
} else if (var == 'mrsos' & startyr == 2010) {
  datH <- read_csv(paste0(fileloc1, loc1[1], loc2, 'EXCEED_DAY_', var, mFileH,
                          '1980','-','2010','.csv'),
                   col_names = TRUE, cols(.default = col_double()))
  lonlat <- datH[,1:2]
  daysH <- colnames(datH[,3:ncol(datH)])
  dat1040 <- read_csv(paste0(fileloc1, loc1[scenario], loc2, 'EXCEED_DAY_', var, mFile,
                          startyr,'-',endyr,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
  days1040 <- colnames(dat1040[,3:ncol(dat1040)])
  dat4070 <- read_csv(paste0(fileloc1, loc1[scenario], loc2, 'EXCEED_DAY_', var, mFile,
                             '2040','-','2070','.csv'),
                      col_names = TRUE, cols(.default = col_double()))
  days4070 <- colnames(dat4070[,3:ncol(dat4070)])
  dat7000 <- read_csv(paste0(fileloc1, loc1[scenario], loc2, 'EXCEED_DAY_', var, mFile,
                             '2070','-','2100','.csv'),
                      col_names = TRUE, cols(.default = col_double()))
  days7000 <- colnames(dat7000[,3:ncol(dat7000)])
  
  days <- c('lon','lat', daysH, days1040, days4070, days7000)
  days <- unique(days)
  datExceed <- cbind(datH, dat1040, dat4070, dat7000)
  datExceed <- datExceed %>%
    select(all_of(days))
} else {
  var <- NA
}


# Part IV Temperature Waves ####################################################
if (var == 'tasmax'| var == 'tasmin'){
  B <- Sys.time()
  print(paste0('Starting to calculate the Temperature Wave at: ',B))
  datWaves <- apply(datExceed[,3:ncol(datExceed)], 
                    MARGIN = 1, FUN = day_wave) %>%
    t()
  datWaves <- cbind(lonlat, datWaves)
  colnames(datWaves) <- c('lon','lat',days)
  write.csv(datWaves, file=paste0(fileloc1, loc1, loc2, 'WAVES_DAY_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}

# Part V Soil Moisture Waves ###################################################
if (var == 'mrsos'& startyr == 2010){
  B <- Sys.time()
  print(paste0('Starting to calculate the Flash Drought Wave at: ',B))
  datWaves <- apply(datExceed[,3:ncol(datExceed)], 
                    MARGIN = 1, FUN = day_wave_flash) %>%
    t()
  datWaves <- cbind(lonlat, datWaves)
  colnames(datWaves) <- c(days)
  dat <- datWaves %>%
    select(all_of(daysH))
  dat <- cbind(lonlat, dat)
  write.csv(dat, file=paste0(fileloc1, loc1[1], loc2, 'WAVES_DAY_', var,
                                  mFileH, '1980','-','2010','.csv'), 
            row.names = FALSE)
  dat <- datWaves %>%
    select(all_of(days1040))
  dat <- cbind(lonlat, dat)
  write.csv(dat, file=paste0(fileloc1, loc1[scenario], loc2, 'WAVES_DAY_', var,
                                  mFile, startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  dat <- datWaves %>%
    select(all_of(days4070))
  dat <- cbind(lonlat, dat)
  write.csv(dat, file=paste0(fileloc1, loc1[scenario], loc2, 'WAVES_DAY_', var,
                             mFile, '2040','-','2070','.csv'), 
            row.names = FALSE)
  dat <- datWaves %>%
    select(all_of(days7000))
  dat <- cbind(lonlat, dat)
  write.csv(dat, file=paste0(fileloc1, loc1[scenario], loc2, 'WAVES_DAY_', var,
                             mFile, '2070','-','2100','.csv'), 
            row.names = FALSE)
}

# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating the WAVE requested for ',
             var,'. End time: ',B, ' Total time elapsed: ', B-A))
print("-----------------------------------------------------------------------")
