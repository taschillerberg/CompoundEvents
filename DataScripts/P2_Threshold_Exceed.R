# P2_Threshold_Exceed.R
# About: This program will calculate threshold values for climate events, 
#        concerning temperature, precipitation, and soil moisture. The overall 
#        structure is open historical data, calculate the thresholds, and the 
#        exceedance with respect to the climate variable.
#        Time: 2.5hr for 1 variable - 8 Models
# 
# Inputs: CMIP6 Historical, SSP126, SSP585
# Outputs: THRESHOLD & EXCEED, ORG (mrsos only)
#
# T. A. Schillerberg
#               Oct. 2022
#      Updated: Sep. 2023

# Office Computer
# fileloc1 <- 'C:/Research/Data/'

# HPC
fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(ncdf4)
library(tidyverse)
library(zoo)

# Part I Variables To Change ###################################################
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')[as.numeric('model_var')] # bash script
mNum <- as.numeric('model_num') # bash script
# var <- c('tasmax', 'tasmin', 'pr', 'mrsos')[4]
# mNum <- 1 # Select a model (1-8)
# mFile <- c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_',
#            '_day_EC-Earth3_historical_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_historical_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_historical_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_historical_r1i1p1f1_gn_')[mNum]
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
mFile <- list(c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_','_day_CMCC-ESM2_ssp126_r1i1p1f1_gn_'),
              c('_day_EC-Earth3_historical_r1i1p1f1_gr_','_day_EC-Earth3_ssp126_r1i1p1f1_gr_'),
              c('_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_','_day_GFDL-ESM4_ssp126_r1i1p1f1_gr1_'),
              c('_day_INM-CM4-8_historical_r1i1p1f1_gr1_','_day_INM-CM4-8_ssp126_r1i1p1f1_gr1_'),
              c('_day_INM-CM5-0_historical_r1i1p1f1_gr1_','_day_INM-CM5-0_ssp126_r1i1p1f1_gr1_'),
              c('_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_','_day_MPI-ESM1-2-HR_ssp126_r1i1p1f1_gn_'),
              c('_day_MRI-ESM2-0_historical_r1i1p1f1_gn_','_day_MRI-ESM2-0_ssp126_r1i1p1f1_gn_'),
              c('_day_NorESM2-MM_historical_r1i1p1f1_gn_','_day_NorESM2-MM_ssp126_r1i1p1f1_gn_'))[mNum]
# mFile <- list(c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_','_day_CMCC-ESM2_ssp585_r1i1p1f1_gn_'),
#               c('_day_EC-Earth3_historical_r1i1p1f1_gr_','_day_EC-Earth3_ssp585_r1i1p1f1_gr_'),
#               c('_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_','_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_'),
#               c('_day_INM-CM4-8_historical_r1i1p1f1_gr1_','_day_INM-CM4-8_ssp585_r1i1p1f1_gr1_'),
#               c('_day_INM-CM5-0_historical_r1i1p1f1_gr1_','_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_'),
#               c('_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_','_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_'),
#               c('_day_MRI-ESM2-0_historical_r1i1p1f1_gn_','_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_'),
#               c('_day_NorESM2-MM_historical_r1i1p1f1_gn_','_day_NorESM2-MM_ssp585_r1i1p1f1_gn_'))[mNum]
# loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')[1]
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/')
# loc1 <- c('CMIP6_historical/','CMIP6_SSP585/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')[mNum]
a <-strsplit(loc2,'/') %>% unlist() 
startyr <- 2010
endyr <- 2040

print(paste0('Model ',loc2))
print(paste0('Var ', var))
print('Rscript: P2_Threshold_Exceed.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))

# Part II Functions ############################################################
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
pr_thresholds <- function(X){
  # This function will calculate the extreme percentile (lower and upper) 
  #     for the supplied time series. Which should be precipitation. Only 
  #     the wet days (daily precipitation greater than 1mm) will be 
  #     considered. All other values will be treated as NA and removed from
  #     the calculation.
  #
  # X   vector of timeseries values
  
  X[X < 1] <- NA
  perc <- quantile(X, c(0.01,0.025,0.05,0.95, 0.975, 0.99, 0.999), na.rm = TRUE)
  pmean <- median(X, na.rm = TRUE)
  pmedian <- median(X, na.rm = TRUE)
  pmax <- max(X, na.rm = TRUE)
  pmin <- min(X, na.rm = TRUE)
  pSd <- sd(X, na.rm = TRUE)
  threshold <- c(perc, pmean, pmedian, pmax, pmin, pSd)
  
  return (threshold)
}
mrsos_thresholds <- function(X){
  # This function will calculate the percentiles for the supplied time
  #     series. Which should be soil moisture. Only days with soil moisture
  #     greater than 0.0001 will be considered. All other values will be 
  #     treated as NA and removed from the calculation.
  #
  # X   vector of timeseries values
  
  X[X < 0.0001] <- NA
  perc <- quantile(X, c(0.1,0.2,0.4), na.rm = TRUE)
  pmean <- median(X, na.rm = TRUE)
  pmedian <- median(X, na.rm = TRUE)
  pmax <- max(X, na.rm = TRUE)
  pmin <- min(X, na.rm = TRUE)
  pSd <- sd(X, na.rm = TRUE)
  threshold <- c(perc, pmean, pmedian, pmax, pmin, pSd)
  
  return (threshold)
}
# Part III - Opening the Files #################################################
# . 3.1 Opening the NC Files ---------------------------------------------------
A <- Sys.time()
print(paste0('Starting to open files at: ',A))
if (mNum == 1 & var != 'mrsos'){
  # . . 3.1.a Model 1 CMCC no mrsos -----
  if (startyr == 1980){
    # # Open the first file 1975-1999
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '1975','0101-','1999','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Testing via raster plotting
    # dat <- raster::raster(t(varNC[,,1]), 
    #                       xmn=min(lonNC), xmx=max(lonNC), ymn=min(latNC), 
    #                       ymx=max(latNC)) %>% 
    #   raster::flip(direction = 2)
    # spp::plot(dat)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Testing via ggplot
    # ggplot(data=datVar, aes(x=Var1, y=Var2, fill=Var.3)) +
    #   geom_tile()
        # # Open the second file 2000-2014
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2000','0101-','2014','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(47450.5,58764.5, by=1)))]
  } else if (startyr == 2010){
    # # Open the first file 2000-2014
    datNC <- ncdf4::nc_open(paste0(fileloc1, loc1[1], loc2, 'regrid360x180_', var,
                                   mFile[[1]][1],'2000','0101-','2014','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2015-2039
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                   mFile[[1]][2],'2015','0101-','2039','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the third file 2040-2064
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                   mFile[[1]][2],'2040','0101-','2064','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(58400.5,69714.5, by=1)))]
  } else if (startyr == 2040){
    # # Open the first file 2040-2064
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2040','0101-','2064','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2065-2089
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2065','0101-','2089','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(69350.5,80664.5, by=1)))]
  } else if (startyr == 2070){
    # # Open the first file 2065-2089
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2065','0101-','2089','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2090-2100
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2090','0101-','2100','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(80300.5,91614.5, by=1)))]
  } else {
    print('Start year not reconized')
  }
} else if (mNum == 1 | mNum == 4 | mNum == 5 | mNum == 7){
  # . . 3.1.b Model 1 CMCC mrsos & Model 4 INM, 5 INM, 7 MRI -----
  if (startyr == 1980){
    # #  Open the first file 1950-1999
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '1950','0101-','1999','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2000-2014
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2000','0101-','2014','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    if (mNum == 7){
      datVar <- datVar[,c('lon','lat',as.character(seq(47481.5,58803.5, by=1)))]
    } else {
      datVar <- datVar[,c('lon','lat',as.character(seq(47450.5,58764.5, by=1)))]
    }
  } else if (startyr == 2010){
    # # Open the first file 2000-2014
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[1],loc2,'regrid360x180_',var,
                                   mFile[[1]][1], '2000','0101-','2014','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2015-2064
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                   mFile[[1]][2],'2015','0101-','2064','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    if (mNum == 7){
      datVar <- datVar[,c('lon','lat',as.character(seq(58439.5,69761.5, by=1)))]
    } else {
      datVar <- datVar[,c('lon','lat',as.character(seq(58400.5,69714.5, by=1)))]
    }
  } else if (startyr == 2040 | 2070){
    # # Open the first file 2015-2064
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2015','0101-','2064','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2065-2100
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2065','0101-','2100','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    if (startyr == 2040) {
      if (mNum == 7){
        datVar <- datVar[,c('lon','lat',as.character(seq(69396.5,80718.5, by=1)))]
      } else {
        datVar <- datVar[,c('lon','lat',as.character(seq(69350.5,80664.5, by=1)))]
      }
    } else if (startyr == 2070){
      if (mNum == 7){
        datVar <- datVar[,c('lon','lat',as.character(seq(80354.5,91675.5, by=1)))]
      } else {
        datVar <- datVar[,c('lon','lat',as.character(seq(80300.5,91614.5, by=1)))]
      }
    } else { print('Start year not reconized') }
  } else { print('Start year not reconized') } 
} else if (mNum == 2){
  # . . 3.1.c Model 2 EC-Earth -----
  for (i in startyr:endyr){
    if (startyr == 2010){
      if (i <= 2014){
        datNC <- ncdf4::nc_open(paste0(fileloc1, loc1[1],loc2,'regrid360x180_',
                                       var,mFile[[1]][1],i,'0101-',i,'1231.nc'))
      } else {
        datNC <- ncdf4::nc_open(paste0(fileloc1, loc1[2],loc2,'regrid360x180_',
                                       var,mFile[[1]][2],i,'0101-',i,'1231.nc'))
      }
    } else {
      datNC <- ncdf4::nc_open(paste0(fileloc1, loc1,loc2,'regrid360x180_',
                                     var,mFile,i,'0101-',i,'1231.nc'))
    }
    
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    if (i == startyr){
      latNC <- ncdf4::ncvar_get(datNC, 'lat')
      lonNC <- ncdf4::ncvar_get(datNC, 'lon')
      datVar <- expand.grid(lonNC,latNC) %>%
        tibble()
      time <- tNC
    } else {
      time <- c(time, tNC)
    }
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Make into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    if (i == endyr){
      colnames(datVar) <- c('lon','lat',time)
    } 
  } # End year loop for Model EC-Earth3
} else if (mNum == 3){
  # . . 3.1.d Model 3 GFDL -----
  if (startyr == 1980){
    # #  Open the first file 1970-1989
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '1970','0101-','1989','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 1990-2009
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '1990','0101-','2009','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the third file 2010-2014
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2010','0101-','2014','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(47450.5,58764.5, by=1)))]
  } else if (startyr == 2010){
    # # Open the first file 2010-2014
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[1],loc2,'regrid360x180_',var,
                                   mFile[[1]][1], '2010','0101-','2014','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2035-2054
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                   mFile[[1]][2],'2015','0101-','2034','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the third file 2035-2054
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                   mFile[[1]][2],'2035','0101-','2054','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(58400.5,69714.5, by=1)))]
  } else if (startyr == 2040){
    # # Open the first file 2035-2054
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2035','0101-','2054','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2055-2074
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2055','0101-','2074','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(69350.5,80664.5, by=1)))]
  } else if (startyr == 2070){  
    # # Open the first file 2055-2074
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2055','0101-','2074','1231.nc'))
    lonNC <- ncdf4::ncvar_get(datNC, 'lon')
    latNC <- ncdf4::ncvar_get(datNC, 'lat')
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- tNC
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the second file 2075-2094
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2075','0101-','2094','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    # # Open the third file 2095-2100
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,mFile,
                                   '2095','0101-','2100','1231.nc'))
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    time <- c(time, tNC)
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    colnames(datVar) <- c('lon','lat',time)
    datVar <- datVar[,c('lon','lat',as.character(seq(80300.5,91614.5, by=1)))]
  } else { print('Start year not reconized') } 
} else if (mNum == 6){
  # . . 3.1.e Model 6 MPI -----
  for (i in seq(startyr, endyr, by = 5)){
    if (startyr == 2010){
      if (i == 2010){
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[1],loc2,'regrid360x180_',var,
                                       mFile[[1]][1],i,'0101-',(i+4),'1231.nc'))
      } else {
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                       mFile[[1]][2],i,'0101-',(i+4),'1231.nc'))
      }
    } else {
      if (i == 2100){
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,
                                       mFile,i,'0101-',i,'1231.nc'))
      } else {
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,
                                       mFile,i,'0101-',(i+4),'1231.nc'))
      }
    }
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    if (i == startyr){
      latNC <- ncdf4::ncvar_get(datNC, 'lat')
      lonNC <- ncdf4::ncvar_get(datNC, 'lon')
      datVar <- expand.grid(lonNC,latNC) %>%
        tibble()
      time <- tNC
    } else {
      time <- c(time, tNC)
    }
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    if (i == endyr){
      colnames(datVar) <- c('lon','lat',time)
      if (startyr == 1980){
        datVar <- datVar[,c('lon','lat',as.character(seq(47481.5,58803.5, by=1)))]
      } else if (startyr == 2010){
        datVar <- datVar[,c('lon','lat',as.character(seq(58439.5,69761.5, by=1)))]
      } else if (startyr == 2040){
        datVar <- datVar[,c('lon','lat',as.character(seq(69396.5,80718.5, by=1)))]
      } else if (startyr == 2070){
        datVar <- datVar[,c('lon','lat',as.character(seq(80354.5,91675.5, by=1)))]
      } else (print('Start year not reconized'))
    } 
  } # For Loop 
} else if (mNum == 8){
  # . . 3.1.f Model 8 Nor -----
  for (i in seq(startyr, endyr, by = 10)){
    if (startyr == 1980){
      if ( i == 2010){
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,
                                       mFile,i,'0101-',(i+4),'1231.nc'))
      }
      else {
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,
                                       mFile,i,'0101-',(i+9),'1231.nc'))
      }
    } else if (startyr == 2010) {
      if ( i == 2010){
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[1],loc2,'regrid360x180_',var,
                                       mFile[[1]][1],i,'0101-',(i+4),'1231.nc'))
      } else if (i == 2020) {
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                       mFile[[1]][2],(i-5),'0101-',i,'1231.nc'))
      } else {
        datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[2],loc2,'regrid360x180_',var,
                                       mFile[[1]][2],(i-9),'0101-',i,'1231.nc'))
      }
    } else {
      datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2,'regrid360x180_',var,
                                     mFile,(i-9),'0101-',i,'1231.nc'))
    }
    tNC <- ncdf4::ncvar_get(datNC, 'time')
    if (i == startyr){
      latNC <- ncdf4::ncvar_get(datNC, 'lat')
      lonNC <- ncdf4::ncvar_get(datNC, 'lon')
      datVar <- expand.grid(lonNC,latNC) %>%
        tibble()
      time <- tNC
    } else {
      time <- c(time, tNC)
    }
    varNC <- ncdf4::ncvar_get(datNC, var)
    fillvalue <- ncdf4::ncatt_get(datNC, var, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    ncdf4::nc_close(datNC)
    # # Making into long format
    j <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,j], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      if (var == 'tasmax' | var == 'tasmin'){ dat <- dat - 273.15} # Conversion to C
      if (var == 'pr'){ dat <- dat * 86400 } # Conversion to mm/day
      datVar <- cbind(datVar,dat)
      if(j == dim(varNC)[3]){break}
      j <- j + 1
    }
    if (i == endyr){
      colnames(datVar) <- c('lon','lat',time)
      if (startyr == 1980){
        datVar <- datVar[,c('lon','lat',as.character(seq(722335.5,733649.5, by=1)))]
      } else if (startyr == 2010){
        datVar <- datVar[,c('lon','lat',as.character(seq(733285.5,744599.5, by=1)))]
      } else if (startyr == 2040){
          datVar <- datVar[,c('lon','lat',as.character(seq(744235.5,755549.5, by=1)))]
      } else if (startyr == 2070){
        datVar <- datVar[,c('lon','lat',as.character(seq(755185.5,766499.5, by=1)))]
      } else (print('Start year not reconized'))
    } 
  } # end for loop
} else {
  print(paste0('Model Number ', mNum, ' not reconized.'))
}
B <- Sys.time()
print(paste0('Finished opening the nc files at: ',B, 
             ' and starting to modify the lon and opening mask to remove water.'))

# . 3.2 Opening the mask & modifying lon ---------------------------------------
mask <- read_csv(paste0(fileloc1,'CMIP6_historical/','MASK_FULL.csv'),
                 col_names = TRUE, cols(.default = col_double()))
datVar <- cbind(mask$lon2, datVar$lat,
                datVar[,3:dim(datVar)[2]] * mask$FullMask) 
datVar <- na.omit(datVar)

B <- Sys.time()
print(paste0('Finished Part I at: ',B,' time elapsed: ', B-A))

if (startyr == 2010){ 
  loc1 <- loc1[2]
  mFile <- mFile[[1]][2] 
  }

# Part IV - TMAX ###############################################################
# . 4.1 Thresholds -------------------------------------------------------------
B <- Sys.time()
if (var == 'tasmax'){
  write.csv(datVar, file = paste0(fileloc1,loc1,loc2,'ORG_DAY_',
                                  var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  print(paste0('Starting to calculate or opening the Tmax thresholds at: ',B))
  if (loc1 == 'CMIP6_historical/'){
    # . . 4.1.1 Calculating the thresholds ##
    perc <- apply(datVar[,3:ncol(datVar)], 
                  MARGIN = 1, quantile, c(0.95, 0.975, 0.99), 
                  na.rm = TRUE) %>%  t()
    Tmean <- apply(datVar[,3:ncol(datVar)], 
                   MARGIN = 1, mean, na.rm = TRUE) %>% tibble()
    Tmedian <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, median, na.rm = TRUE) %>% tibble()
    Tmax <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, max, na.rm = TRUE) %>% tibble()
    Tmin <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, min, na.rm = TRUE) %>% tibble()
    TSd <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, sd, na.rm = TRUE) %>% tibble()
    # . . 4.1.2 Formating the data ##
    datThresh <- cbind(datVar[,1:2], perc, Tmean, Tmedian, Tmax, Tmin, TSd)
    colnames(datThresh) <- c('lon', 'lat', 'P95', 'P975', 'P99', 
                              'PMean', 'PMedian', 'PMax', 'PMin', 'PSd')
    datThresh$PMean[is.nan(datThresh$PMean)] <- NA
    datThresh$PMax[datThresh$PMax == -Inf] <- NA
    datThresh$PMin[datThresh$PMin == Inf] <- NA
    # . . 4.1.3 Writing the Thresholds ##
    write.csv(datThresh, file=paste0(fileloc1, 'CMIP6_historical/', loc2,
                                     'THRESHOLD_', var, mFile, startyr, '-', 
                                     endyr, '.csv'),
              row.names = FALSE)
    rm(perc, Tmean, Tmedian, Tmax, Tmin, TSd)
  } else {
    # . . 4.1.4 Opening the thresholds ##
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
                                 'THRESHOLD_', var, '_day_', a, x, '_', 
                                 1980,'-',2010,'.csv'),
                           col_names = TRUE, cols(.default = col_double()))
  }

  B <- Sys.time()
  print(paste0('Finished calculating or opening the thresholds at: ', B))
  # . 4.2 Temperature Exceed ---------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the exceedance at: ', B))
  lonlat <- datVar[,1:2]
  days <- colnames(datVar[ ,3:ncol(datVar)])
  datVar <- cbind(datThresh$P95, datVar[ , 3:ncol(datVar)])
  exceed <- apply(X = datVar, MARGIN = 1, FUN = threshold_exceed, opp = 1) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat',days)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'EXCEED_DAY_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}

# Part V - TMIN ################################################################
# . 5.1 Thresholds -------------------------------------------------------------
B <- Sys.time()
if (var == 'tasmin'){
  write.csv(datVar, file = paste0(fileloc1,loc1,loc2,'ORG_DAY_',
                                  var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  print(paste0('Starting to calculate or opening the Tmin thresholds at: ',B))
  if (loc1 == 'CMIP6_historical/'){
    # . . 5.1.1 Calculating the thresholds ##
    perc <- apply(datVar[,3:ncol(datVar)], 
                  MARGIN = 1, quantile, c(0.05, 0.225, 0.01), 
                  na.rm = TRUE) %>%  t()
    Tmean <- apply(datVar[,3:ncol(datVar)], 
                   MARGIN = 1, mean, na.rm = TRUE) %>% tibble()
    Tmedian <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, median, na.rm = TRUE) %>% tibble()
    Tmax <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, max, na.rm = TRUE) %>% tibble()
    Tmin <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, min, na.rm = TRUE) %>% tibble()
    TSd <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, sd, na.rm = TRUE) %>% tibble()
    # . . 5.1.2 Formating the data ##
    datThresh <- cbind(datVar[,1:2], perc, Tmean, Tmedian, Tmax, Tmin, TSd)
    colnames(datThresh) <- c('lon', 'lat', 'P05', 'P225', 'P01', 
                             'PMean', 'PMedian', 'PMax', 'PMin', 'PSd')
    datThresh$PMean[is.nan(datThresh$PMean)] <- NA
    datThresh$PMax[datThresh$PMax == -Inf] <- NA
    datThresh$PMin[datThresh$PMin == Inf] <- NA
    # . . 5.1.3 Writing the Thresholds ##
    write.csv(datThresh, file=paste0(fileloc1, 'CMIP6_historical/', loc2,
                                     'THRESHOLD_', var, mFile, startyr, '-', 
                                     endyr, '.csv'),
              row.names = FALSE)
    rm(perc, Tmean, Tmedian, Tmax, Tmin, TSd)
  } else {
    # . . 5.1.4 Opening the thresholds ##
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
                                 'THRESHOLD_', var, '_day_', a, x, '_', 
                                 1980,'-',2010,'.csv'),
                          col_names = TRUE, cols(.default = col_double()))
  }
  B <- Sys.time()
  print(paste0('Finished calculating or opening the thresholds at: ', B))
  # . 5.2 Temperature Exceed ---------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the exceedance at: ', B))
  lonlat <- datVar[,1:2]
  days <- colnames(datVar[,3:ncol(datVar)])
  datVar <- cbind(datThresh$P05, datVar[,3:ncol(datVar)])
  exceed <- apply(X = datVar, MARGIN=1, FUN=threshold_exceed, opp=2) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat',days)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'EXCEED_DAY_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}

# Part VI - Precipitation ######################################################
# . 6.1 Thresholds -------------------------------------------------------------
B <- Sys.time()
if (var == 'pr'){
  write.csv(datVar, file = paste0(fileloc1,loc1,loc2,'ORG_DAY_',
                                  var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  print(paste0('Starting to calculate or opening the Pr thresholds at: ',B))
  if (loc1 == 'CMIP6_historical/'){
    # . . 6.1.1 Calculating the thresholds ##
    datThresh <- apply(datVar[,3:ncol(datVar)], 
                       MARGIN = 1, FUN = pr_thresholds) %>%
      t() %>%
      as_tibble()
    # . . 6.1.2 Formatting the data ##
    datThresh <- cbind(datVar[,1:2], datThresh)
    colnames(datThresh) <- c('lon','lat','P01','P025','P05','P95','P975',
                             'P99','P999','PMean','PMedian','PMax',
                             'PMin','PSd')
    datThresh$PMean[is.nan(datThresh$PMean)] <- NA
    datThresh$PMax[datThresh$PMax == -Inf] <- NA
    datThresh$PMin[datThresh$PMin == Inf] <- NA
    # . . 6.1.3 Writing the Thresholds ##
    
    write.csv(datThresh, file=paste0(fileloc1, 'CMIP6_historical/', loc2,
                                     'THRESHOLD_', var, mFile, startyr, '-', 
                                     endyr, '.csv'),
              row.names = FALSE)
  } else {
    # . . 5.1.4 Opening the thresholds ##
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
                                 'THRESHOLD_', var, '_day_', a, x, '_', 
                                 1980,'-',2010,'.csv'),
                          col_names = TRUE, cols(.default = col_double()))
  }

  B <- Sys.time()
  print(paste0('Finished calculating or opening the thresholds at: ', B))
  
  # . 6.2 Precipitation Exceed -------------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the exceedance at: ', B))
  lonlat <- datVar[,1:2]
  days <- colnames(datVar[,3:ncol(datVar)])
  datVar <- cbind(datThresh$P99, datVar[,3:ncol(datVar)])
  exceed <- apply(X = datVar, MARGIN=1,
                  FUN=threshold_exceed, opp=1) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat',days)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'EXCEED_DAY_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}

# Part VII - Soil Moisture #####################################################
# . 7.1 Thresholds -------------------------------------------------------------
B <- Sys.time()
if (var == 'mrsos'){
  write.csv(datVar, file = paste0(fileloc1,loc1,loc2,'ORG_DAY_',
                                  var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  lonlat <- datVar[,1:2]
  # . . 7.1.1 Calculating the moving average ##
  datVar <- apply(datVar[,3:ncol(datVar)], MARGIN = 1, FUN = zoo::rollmean,
                  k = 5, align = 'center', fill = NA) %>%
    t() %>%
    as_tibble()
  datVar <- cbind(NA, NA, datVar)
  
  print(paste0('Starting to calculate or opening the Mrsos thresholds at: ',B))
  if (loc1 == 'CMIP6_historical/'){
    # . . 7.1.2 Calculating the thresholds ##
    datThresh <- apply(datVar, MARGIN = 1, FUN = mrsos_thresholds) %>%
      t() %>%
      as_tibble()
    # . . 7.1.3 Formating the data ##
    datThresh <- cbind(lonlat, datThresh)
    colnames(datThresh) <- c('lon', 'lat', 'P10','P20','P40', 
                             'PMean', 'PMedian', 'PMax', 'PMin', 'PSd')
    datThresh$PMean[is.nan(datThresh$PMean)] <- NA
    datThresh$PMax[datThresh$PMax == -Inf] <- NA
    datThresh$PMin[datThresh$PMin == Inf] <- NA
    # . . 7.1.4 Writing the Thresholds ##
    write.csv(datThresh, file=paste0(fileloc1, 'CMIP6_historical/', loc2,
                                     'THRESHOLD_', var, mFile, startyr, '-', 
                                     endyr, '.csv'),
              row.names = FALSE)
  } else {
    # . . 7.1.5 Opening the thresholds ##
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
                                 'THRESHOLD_', var, '_day_', a, x, '_', 
                                 1980,'-',2010,'.csv'),
                          col_names = TRUE, cols(.default = col_double()))
  }
  B <- Sys.time()
  print(paste0('Finished calculating or opening the thresholds at: ', B))
  
  # . 7.2 SoilMoisture Exceed Day ----------------------------------------------
  B <- Sys.time()
  print(paste0('Starting to calculate the exceedance at: ', B))
  days <- colnames(datVar[ ,3:ncol(datVar)])
  datVar <- cbind(datThresh$P40, datThresh$P10, datVar[,3:ncol(datVar)])
  exceed <- apply(X = datVar, MARGIN=1,
                  FUN=threshold_exceed, opp=3) %>%
    t()
  exceed <- cbind(lonlat, exceed)
  colnames(exceed) <- c('lon','lat',days)
  write.csv(exceed, file=
              paste0(fileloc1,loc1,loc2,'EXCEED_DAY_',var,mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
  
  B <- Sys.time()
  print(paste0('Finished calculating the exceedance at: ', B))
}
# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating all thresholds and exceedance for ',
             var,'. End time: ',B, ' Total time elapsed: ', B-A))
print("-----------------------------------------------------------------------")