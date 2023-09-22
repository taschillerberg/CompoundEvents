# P4_Occurrences.R
# About: This program will open the wave and/or exceed file for the selected 
#        variable and calculate the 'wave' occurance for the time 
#        period, yearly, and monthly.
#        Time: -hr for 1 variable - 8 Models
#     
# Inputs:  EXCEED_DAY, WAVES_DAY
# Outputs: OCC, OCCYr, OCCMo
#
# T. A. Schillerberg
#               Aug. 2023
#      Updated: Sep. 2023

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
# var <- c('tasmax', 'tasmin', 'pr', 'mrsos')[as.numeric('model_var')] # Bash script
var <- c('tasmax', 'tasmin','pr', 'mrsos') [4]
mFile <- c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_',
           '_day_EC-Earth3_historical_r1i1p1f1_gr_',
           '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
           '_day_INM-CM4-8_historical_r1i1p1f1_gr1_',
           '_day_INM-CM5-0_historical_r1i1p1f1_gr1_',
           '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
           '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_',
           '_day_NorESM2-MM_historical_r1i1p1f1_gn_')
# mFile <- c('_day_CMCC-ESM2_ssp126_r1i1p1f1_gn_',
#            '_day_EC-Earth3_ssp126_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp126_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_ssp126_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_ssp126_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp126_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp126_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_ssp126_r1i1p1f1_gn_')
# mFile <- c('_day_CMCC-ESM2_ssp585_r1i1p1f1_gn_',
#            '_day_EC-Earth3_ssp585_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_ssp585_r1i1p1f1_gn_')
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')[1]
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')

startYr <- 1980
endYr <- 2010
leapM <- c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)

if (startYr == 1980){ 
  timePeriod <- '8010'
  a <- 'Hist'
} else if (startYr == 2010){ 
  timePeriod <- '1040'
  a <- strsplit(loc1,'/') %>%
    unlist() %>%
    strsplit('_') %>%
    unlist()
  a <- a[2]
} else if (startYr == 2040){ 
  timePeriod <- '4070'
  a <- strsplit(loc1,'/') %>%
    unlist() %>%
    strsplit('_') %>%
    unlist()
  a <- a[2]
} else if (startYr == 2070){ 
  timePeriod <- '7000'
  a <- strsplit(loc1,'/') %>%
    unlist() %>%
    strsplit('_') %>%
    unlist()
  a <- a[2]
} else { print0("Start Year not reconized")}

print(paste0('Variable: ', var))
print('Rscript: P4_Occurances.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startYr, '-', endYr))

# Part II Functions ############################################################
annual_occ <- function(datX, yrSt, leap){
  # datX  : data array containing values of <= 0
  # yrSt : starting year of the data
  # leap : does the variable dat have leap years in it? TRUE = it does have leap days
  
  # Variables Needed -----------------------------------------------------------
  # datX <- as.matrix(datX)
  datYr <- as_tibble(matrix(0, nrow = 1, ncol = floor(length(datX)/365)))
  dat1 <- datYr
  dat2 <- datYr
  dat3 <- datYr
  start <- 1
  
  # Calculations ---------------------------------------------------------------
  for (i in 1: length(datYr)){
    if ((yrSt + i) %% 4 == 0 & (yrSt + i) != 2100 & leap == TRUE){
      leapYr <- 1
    } else { leapYr <- 0}
    dat1[i] <- sum(datX[start : (start + 364 + leapYr)])
    if (i == 2){
      prev <- dat1[i-1] - floor(dat1[i-1])
      dat2[i] <- 1 - prev
    } else if (i > 2 & i != length(datYr)){
      prev <- dat3[i-1] - floor(dat3[i-1])
      dat2[i] <- 1 - prev
      
    }
    if (i == 1 | i == length(datYr)){
      dat3[i] <- dat1[i]
    } else {
      dat3[i] <- dat1[i]-dat2[i]
    }
    start <- start + 364 + leapYr 
  }  
  
  datYr <- round(dat2) + round(dat3)
  
  return(matrix(datYr))
}
month_occ <- function(datX, yrSt, leap){
  # datX  : data array containing values of <= 0
  # yrSt : starting year of the data
  # leap : does the variable dat have leap years in it? TRUE = it does have leap days
  
  # Variables Needed -----------------------------------------------------------
  # datX <- as.matrix(datX)
  datMo <- as_tibble(matrix(0, nrow = 12, ncol = floor(length(datX)/365)))
  dat1 <- datMo
  dat2 <- datMo
  dat3 <- datMo
  dayTot <- 1
  monthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Calculations ---------------------------------------------------------------
  for (i in 1: length(datMo)){
    if ((yrSt + i) %% 4 == 0 & (yrSt + i) != 2100 & leap == TRUE){
      leapYr <- 1
    } else { leapYr <- 0}
    # ---- January 
    dat1[1,i] <- sum(datX[dayTot:(dayTot + monthDays[1] - 1)])
    if (i == 1){
      dat3[1,i] <- dat1[1,i]
    } else {
      prev <- dat3[12, i-1] - floor(dat3[12, i-1])
      dat2[1,i] <- 1 - prev
      dat3[1,i] <- dat1[1,i] - dat2[1,i]
    }
    dayTot <- dayTot + monthDays[1] # February start day
    # ---- February 
    dat1[2,i] <- sum(datX[dayTot : (dayTot+monthDays[2]-1+leapYr)])
    if (i == 1) {
      prev <- dat1[1,i] - floor(dat1[1,i])
      dat2[2,i] <- 1 - prev
    } else {
      prev <- dat3[1, i] - floor(dat3[1,i])
      dat2[2,i] <- 1 - prev
    }
    dat3[2,i] <- dat1[2,i] - dat2[2,i]
    dayTot <- dayTot + monthDays[2] + leapYr # March start day
    # ---- March 
    dat1[3,i] <- sum(datX[dayTot:(dayTot+monthDays[3]-1)])
    prev <- dat3[2,i] - floor(dat3[2,i])
    dat2[3,i] <- 1 - prev
    dat3[3,i] <- dat1[3,i] - dat2[3,i]
    dayTot <- dayTot + monthDays[3] # April start day
    # ---- April 
    dat1[4,i] <- sum(datX[dayTot:(dayTot+monthDays[4]-1)])
    prev <- dat3[3,i] - floor(dat3[3,i])
    dat2[4,i] <- 1 - prev
    dat3[4,i] <- dat1[4,i] - dat2[4,i]
    dayTot <- dayTot + monthDays[4] # May start day
    # ---- May
    dat1[5,i] <- sum(datX[dayTot:(dayTot+monthDays[5]-1)])
    prev <- dat3[4,i] - floor(dat3[4,i])
    dat2[5,i] <- 1 - prev
    dat3[5,i] <- dat1[5,i] - dat2[5,i]
    dayTot <- dayTot + monthDays[5] # June start day
    # ---- June
    dat1[6,i] <- sum(datX[dayTot:(dayTot+monthDays[6]-1)])
    prev <- dat3[5,i] - floor(dat3[5,i])
    dat2[6,i] <- 1 - prev
    dat3[6,i] <- dat1[6,i] - dat2[6,i]
    dayTot <- dayTot + monthDays[6] # July start day
    # ---- July
    dat1[7,i] <- sum(datX[dayTot:(dayTot+monthDays[7]-1)])
    prev <- dat3[6,i] - floor(dat3[6,i])
    dat2[7,i] <- 1 - prev
    dat3[7,i] <- dat1[7,i] - dat2[7,i]
    dayTot <- dayTot + monthDays[7] # August start day
    # ---- August
    dat1[8,i] <- sum(datX[dayTot:(dayTot+monthDays[8]-1)])
    prev <- dat3[7,i] - floor(dat3[7,i])
    dat2[8,i] <- 1 - prev
    dat3[8,i] <- dat1[8,i] - dat2[8,i]
    dayTot <- dayTot + monthDays[8] # September start day
    # ---- September
    dat1[9,i] <- sum(datX[dayTot:(dayTot+monthDays[9]-1)])
    prev <- dat3[8,i] - floor(dat3[8,i])
    dat2[9,i] <- 1 - prev
    dat3[9,i] <- dat1[9,i] - dat2[9,i]
    dayTot <- dayTot + monthDays[9] # October start day
    # ---- October
    dat1[10,i] <- sum(datX[dayTot:(dayTot+monthDays[10]-1)])
    prev <- dat3[9,i] - floor(dat3[9,i])
    dat2[10,i] <- 1 - prev
    dat3[10,i] <- dat1[10,i] - dat2[10,i]
    dayTot <- dayTot + monthDays[10] # November start day
    # ---- November
    dat1[11,i] <- sum(datX[dayTot:(dayTot+monthDays[11]-1)])
    prev <- dat3[10,i] - floor(dat3[10,i])
    dat2[11,i] <- 1 - prev
    dat3[11,i] <- dat1[11,i] - dat2[11,i]
    dayTot <- dayTot + monthDays[11] # December start day
    # ---- December
    dat1[12,i] <- sum(datX[dayTot:(dayTot+monthDays[12]-1)])
    if (i != dim(datMo)[2]){
      prev <- dat3[11,i] - floor(dat3[11,i])
      dat2[12,i] <- 1-prev
      dat3[12,i] <- dat1[12,i] - dat2[12,i]
    } else {
      dat3[12,i] <- dat1[12,i]
    }
    dayTot <- dayTot + monthDays[12] # January start day
  }
  dat2 <- apply(dat2, MARGIN = 2, FUN = round) %>%
    apply(MARGIN = 1, FUN = mean) %>%
    as_tibble()
  dat3 <- apply(dat3, MARGIN = 2, FUN = round) %>%
    apply(MARGIN = 1, FUN = mean)%>%
    as_tibble()
  datMo <- t(dat2 + dat3) 
  
  return(datMo)
}
am_mean <- function(datX, timeX){
  # This function is to find the average number of events for the years/months given
  # dat   : array - containing multiple years of data
  # time : numeric - number of years/months in the sequence
  # reps : numeric - number of models 
  # Variables Needed -----------------------------------------------------------
  reps <- length(datX)/timeX
  datAvg <- matrix(0, nrow = 1, ncol = timeX)
  datX <- as.numeric(datX)
  datX[is.na(datX)] <- 0
  
  # Calculations ---------------------------------------------------------------
  for (i in 1:timeX){
    sum <- 0
    for (j in 0:(reps-1)){
      sum <- sum + datX[(i + (j * timeX))]
    }
    datAvg[i] <- sum / reps
  }
  return(matrix(datAvg))
}

# Part III Calculating Occurrence ##############################################
# . 3.1 Variables Needed -------------------------------------------------------
datOcc <- tibble(
  'lon' = numeric(length = 12476),
  'lat' = numeric(length = 12476),
  'CMCC-ESM2'  = numeric(length = 12476), 
  'EC-Earth3'  = numeric(length = 12476),
  'GFDL-ESM4' = numeric(length = 12476),
  'INM-CM4-8' = numeric(length = 12476),
  'INM-CM5-0' = numeric(length = 12476),
  'MPI-ESM1-2-HR' = numeric(length = 12476),
  'MRI-ESM2-0' = numeric(length = 12476), 
  'NorESM2-MM'  = numeric(length = 12476),
  'Mu' = numeric(length = 12476)
)

# . 3.2 Calculating for variable -----------------------------------------------
A <- Sys.time()
print(paste0('Starting to calculate the occurance at: ',A))

for (i in 1: length(loc2)){
  print(loc2[i])
  if (var == 'pr'){
    dat <- read_csv(paste0(fileloc1,loc1,loc2[i],'EXCEED_DAY_', var,
                           mFile[i],startYr,'-',endYr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
  } else {
    dat <- read_csv(paste0(fileloc1,loc1,loc2[i],'WAVES_DAY_', var,
                           mFile[i],startYr,'-',endYr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
  }
  # Flash Drought longer than 28 days exceeded 1/28 = 0.0357
  if (var == 'mrsos'){
    lonlat <- dat[,1:2]
    dat <- dat[3:ncol(dat)]
    dat[dat > 0.0357] <- 0
    dat <- cbind(lonlat, dat)
  }
  # Time period number of occurrences
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of total occurrences
  # Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datOcc$lon <- dat$lon
    datOcc$lat <- dat$lat
    datOcc[,2+i] <- as.matrix(Xx)
  } else {
    datOcc[,2+i] <- as.matrix(Xx)
  }
  
  ###  Annual number of occurrences
  print("Annual number of occurances")
  if (i == 1){
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = startYr, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(startYr:endYr), byrow = TRUE)
    datOccYr <- cbind(dat$lon, dat$lat, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist()
    colnames(datOccYr) <- c('lon','lat', paste0(a, "_", startYr:endYr))
  } else {
    names <- colnames(datOccYr)
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = startYr, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(startYr:endYr), byrow = TRUE)
    datOccYr <- cbind(datOccYr, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist()
    colnames(datOccYr) <- c(names, paste0(a, "_", startYr:endYr))
  }
  
  ### Monthly number of occurrences
  print("Monthly occurances")
  if (i == 1){
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = startYr, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = 12, byrow = TRUE)
    datOccMo <- cbind(dat$lon, dat$lat, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist()
    colnames(datOccMo) <- c('lon','lat', paste0(a, "_", 1:12))
  } else {
    names <- colnames(datOccMo)
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = startYr, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1:12), byrow = TRUE)
    datOccMo <- cbind(datOccMo, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist()
    colnames(datOccMo) <- c(names, paste0(a, "_", 1:12))
  }
} 

# . 3.2 Averages ---------------------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the average occurances at: ',B))
### Sequence Mean
datOcc$Mu <- apply(datOcc[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})

### Annual Mean
write.csv(datOccYr,file=paste0(fileloc1, loc1, 'Results/','OCCYr_DAY_', 
                               var,'_', a, '_', timePeriod, 'full.csv'),
          row.names = FALSE)
datOccYr <- cbind(datOccYr[,1], datOccYr[,2],
                  apply(X = datOccYr[,3:ncol(datOccYr)], MARGIN = 1,
                        FUN = am_mean, timeX = length(startYr:endYr)) %>%
                    unlist() %>%
                    matrix(ncol = length(startYr:endYr), byrow = TRUE))
colnames(datOccYr) <- c('lon','lat', paste0(startYr:endYr))

### Monthly Mean
write.csv(datOccYr,file=paste0(fileloc1, loc1, 'Results/','OCCMo_DAY_', 
                               var,'_', a, '_', timePeriod, 'full.csv'),
          row.names = FALSE)
datOccMo <- cbind(datOccMo[,1], datOccMo[,2],
                  apply(X = datOccMo[,3:ncol(datOccMo)], MARGIN = 1,
                        FUN = am_mean, timeX = 12) %>%
                    unlist() %>%
                    matrix(ncol = 12, byrow = TRUE))
colnames(datOccMo) <- c('lon','lat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# . 3.3 Writing the files ------------------------------------------------------
write.csv(datOcc,file=paste0(fileloc1, loc1, 'Results/',
                             'OCC_DAY_', var, '_', a, '_', timePeriod, '.csv'),
          row.names = FALSE)
write.csv(datOccYr,file=paste0(fileloc1, loc1, 'Results/',
                               'OCCYr_DAY_', var,'_', a, '_', timePeriod, '.csv'),
          row.names = FALSE)
write.csv(datOccMo,file=paste0(fileloc1, loc1, 'Results/',
                               'OCCMo_DAY_', var,'_', a, '_', timePeriod, '.csv'),
          row.names = FALSE)
# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating the OCCURANCE requested for ',
             var,'. End time: ',B, ' Total time elapsed: ', B-A))
print("-----------------------------------------------------------------------")
