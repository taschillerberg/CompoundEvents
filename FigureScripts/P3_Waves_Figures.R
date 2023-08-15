# P3_Waves_Figures.R
# About: This script will plot the wave frequencies of the variables.
# 
# Inputs: THRESHOLD & EXCEED
# Outputs: WAVES_Occ##, Frequency plots, database plots
#
# T. A. Schillerberg
#               Oct. 2022
#      Updated: Jan. 2023

# Office Computer
setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# Mac
# setwd("~/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Code2")
# fileloc1 <- "~/OneDrive - Auburn University/Research/FEMAResearch/Data/"

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)

# Part I Variables To Change ###################################################
mFileH <- c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_',
            '_day_EC-Earth3_historical_r1i1p1f1_gr_',
            '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
            '_day_INM-CM4-8_historical_r1i1p1f1_gr1_',
            '_day_INM-CM5-0_historical_r1i1p1f1_gr1_',
            '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
            '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_',
            '_day_NorESM2-MM_historical_r1i1p1f1_gn_')
mFile126 <- c('_day_CMCC-ESM2_ssp126_r1i1p1f1_gn_',
              '_day_EC-Earth3_ssp126_r1i1p1f1_gr_',
              '_day_GFDL-ESM4_ssp126_r1i1p1f1_gr1_',
              '_day_INM-CM4-8_ssp126_r1i1p1f1_gr1_',
              '_day_INM-CM5-0_ssp126_r1i1p1f1_gr1_',
              '_day_MPI-ESM1-2-HR_ssp126_r1i1p1f1_gn_',
              '_day_MRI-ESM2-0_ssp126_r1i1p1f1_gn_',
              '_day_NorESM2-MM_ssp126_r1i1p1f1_gn_')
mFile585 <- c('_day_CMCC-ESM2_ssp585_r1i1p1f1_gn_',
              '_day_EC-Earth3_ssp585_r1i1p1f1_gr_',
              '_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_',
              '_day_INM-CM4-8_ssp585_r1i1p1f1_gr1_',
              '_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_',
              '_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_',
              '_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_',
              '_day_NorESM2-MM_ssp585_r1i1p1f1_gn_')
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')
var <- c('tasmax', 'tasmin', 'pr', 'mrsos') [1]
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation',
          'Flash Drought','Drought') [1]
timeSpan <- c('DAY_', 'WEEK_', 'WEEK_FD_','WEEK_D_', 
              'MONTH_', 'MONTH_FD_','MONTH_D_')[1]
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")

print(paste0('Model ',loc2))
print(paste0('Var ', var))
# Part II Functions ############################################################
annual_occ <- function(dat, yrSt, leap){
  # dat  : data array containing values of <= 0
  # yrSt : starting year of the data
  # leap : does the variable dat have leap years in it? TRUE = it does have leap days
  
  # Variables Needed -----------------------------------------------------------
  # dat <- as.matrix(dat)
  datYr <- as_tibble(matrix(0, nrow = 1, ncol = floor(length(dat)/365)))
  dat1 <- datYr
  dat2 <- datYr
  dat3 <- datYr
  start <- 1
  
  # Calculations ---------------------------------------------------------------
  for (i in 1: length(datYr)){
    if ((yrSt + i) %% 4 == 0 & (yrSt + i) != 2100 & leap == TRUE){
      leapYr <- 1
    } else { leapYr <- 0}
    dat1[i] <- sum(dat[start : (start + 364 + leapYr)])
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
month_occ <- function(dat, yrSt, leap){
  # dat  : data array containing values of <= 0
  # yrSt : starting year of the data
  # leap : does the variable dat have leap years in it? TRUE = it does have leap days
  
  # Variables Needed -----------------------------------------------------------
  # dat <- as.matrix(dat)
  datMo <- as_tibble(matrix(0, nrow = 12, ncol = floor(length(dat)/365)))
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
    dat1[1,i] <- sum(dat[dayTot:(dayTot + monthDays[1] - 1)])
    if (i == 1){
      dat3[1,i] <- dat1[1,i]
    } else {
      prev <- dat3[12, i-1] - floor(dat3[12, i-1])
      dat2[1,i] <- 1 - prev
      dat3[1,i] <- dat1[1,i] - dat2[1,i]
    }
    dayTot <- dayTot + monthDays[1] # February start day
    # ---- February 
    dat1[2,i] <- sum(dat[dayTot : (dayTot+monthDays[2]-1+leapYr)])
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
    dat1[3,i] <- sum(dat[dayTot:(dayTot+monthDays[3]-1)])
    prev <- dat3[2,i] - floor(dat3[2,i])
    dat2[3,i] <- 1 - prev
    dat3[3,i] <- dat1[3,i] - dat2[3,i]
    dayTot <- dayTot + monthDays[3] # April start day
    # ---- April 
    dat1[4,i] <- sum(dat[dayTot:(dayTot+monthDays[4]-1)])
    prev <- dat3[3,i] - floor(dat3[3,i])
    dat2[4,i] <- 1 - prev
    dat3[4,i] <- dat1[4,i] - dat2[4,i]
    dayTot <- dayTot + monthDays[4] # May start day
    # ---- May
    dat1[5,i] <- sum(dat[dayTot:(dayTot+monthDays[5]-1)])
    prev <- dat3[4,i] - floor(dat3[4,i])
    dat2[5,i] <- 1 - prev
    dat3[5,i] <- dat1[5,i] - dat2[5,i]
    dayTot <- dayTot + monthDays[5] # June start day
    # ---- June
    dat1[6,i] <- sum(dat[dayTot:(dayTot+monthDays[6]-1)])
    prev <- dat3[5,i] - floor(dat3[5,i])
    dat2[6,i] <- 1 - prev
    dat3[6,i] <- dat1[6,i] - dat2[6,i]
    dayTot <- dayTot + monthDays[6] # July start day
    # ---- July
    dat1[7,i] <- sum(dat[dayTot:(dayTot+monthDays[7]-1)])
    prev <- dat3[6,i] - floor(dat3[6,i])
    dat2[7,i] <- 1 - prev
    dat3[7,i] <- dat1[7,i] - dat2[7,i]
    dayTot <- dayTot + monthDays[7] # August start day
    # ---- August
    dat1[8,i] <- sum(dat[dayTot:(dayTot+monthDays[8]-1)])
    prev <- dat3[7,i] - floor(dat3[7,i])
    dat2[8,i] <- 1 - prev
    dat3[8,i] <- dat1[8,i] - dat2[8,i]
    dayTot <- dayTot + monthDays[8] # September start day
    # ---- September
    dat1[9,i] <- sum(dat[dayTot:(dayTot+monthDays[9]-1)])
    prev <- dat3[8,i] - floor(dat3[8,i])
    dat2[9,i] <- 1 - prev
    dat3[9,i] <- dat1[9,i] - dat2[9,i]
    dayTot <- dayTot + monthDays[9] # October start day
    # ---- October
    dat1[10,i] <- sum(dat[dayTot:(dayTot+monthDays[10]-1)])
    prev <- dat3[9,i] - floor(dat3[9,i])
    dat2[10,i] <- 1 - prev
    dat3[10,i] <- dat1[10,i] - dat2[10,i]
    dayTot <- dayTot + monthDays[10] # November start day
    # ---- November
    dat1[11,i] <- sum(dat[dayTot:(dayTot+monthDays[11]-1)])
    prev <- dat3[10,i] - floor(dat3[10,i])
    dat2[11,i] <- 1 - prev
    dat3[11,i] <- dat1[11,i] - dat2[11,i]
    dayTot <- dayTot + monthDays[11] # December start day
    # ---- December
    dat1[12,i] <- sum(dat[dayTot:(dayTot+monthDays[12]-1)])
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
    apply(MARGIN = 1, FUN = mean)
  dat3 <- apply(dat3, MARGIN = 2, FUN = round) %>%
    apply(MARGIN = 1, FUN = mean)
  datMo <- dat2 + dat3
   
  return(matrix(datMo))
}
am_mean <- function(dat, years, model){
  # This function is to find the average number of events for the years/months given
  # dat   : array - containing multiple years of data
  # years : numeric - number of years/months in the sequence
  # model : numeric - number of models 
  # Variables Needed -----------------------------------------------------------
  datAvg <- matrix(0, nrow = 1, ncol = years)
  
  # Calculations ---------------------------------------------------------------
  for (i in 1:years){
    sum <- 0
    for (j in 0:(model-1)){
      sum <- sum + dat[(i + j * years)]
    }
    datAvg[i] <- sum / model
  }
  return(matrix(datAvg))
}
get_legend <- function(p, position = NULL){
  # Reference:
  if(is.null(p)) return(NULL)
  if(!is.null(position)){
    p <- p + theme(legend.position = position)
  }
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]]
  else leg <- NULL
  leg
}
as_ggplot <- function(x){
  # Reference: https://github.com/kassambara/ggpubr/blob/master/R/as_ggplot.R 
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::grobTree(x))
}
# Part III Mean Values #########################################################
# . 3.1 Variables Needed -------------------------------------------------------
datOccH <- tibble(
  'lon' = numeric(length = 12476),
  'lat' = numeric(length = 12476),
  'CMCC-ESM2' = numeric(length = 12476),
  'EC-Earth3' = numeric(length = 12476),
  'GFDL-ESM4' = numeric(length = 12476),
  'INM-CM4-8' = numeric(length = 12476),
  'INM-CM5-0' = numeric(length = 12476),
  'MPI-ESM1-2-HR' = numeric(length = 12476),
  'MRI-ESM2-0' = numeric(length = 12476),
  'NorESM2-MM' = numeric(length = 12476),
  'Mu' = numeric(length = 12476),
)
datOcc126_4070 <- datOccH
datOcc126_7000 <- datOccH
datOcc585_4070 <- datOccH
datOcc585_7000 <- datOccH
leapM <- c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)

# . 3.2 Opening & Formatting Waves Files ---------------------------------------
# . . 3.2.1 Historical ---------------------------------------------------------
for (i in 1: length(loc2)){
  print(loc2[i])
  dat <- read_csv(paste0(fileloc1,loc1[1],loc2[i],'WAVES_',timeSpan,var,
                         mFileH[i],1980,'-',2010,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  # Time period number of occurrences
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of total occurrences
  # Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datOccH$lon <- dat$lon
    datOccH$lat <- dat$lat
    datOccH$`CMCC-ESM2` <- as_tibble(Xx)
  } else {
    datOccH[,2+i] <- as_tibble(Xx)
  }
  # Annual number of occurrences
  print("Annual number of occurances")
  if (i == 1){
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 1980, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1980:2010), byrow = TRUE)
    datOccYr <- cbind(dat$lon, dat$lat, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c('lon','lat', paste0(a, "_", 1980:2010))
  } else {
    names <- colnames(datOccYr)
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 1980, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1980:2010), byrow = TRUE)
    datOccYr <- cbind(datOccYr, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c(names, paste0(a, "_", 1980:2010))
  }
  # Monthly number of occurrences 
  print("Monthly occurances")
  if (i == 1){
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 1980, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = 12, byrow = TRUE)
    datOccMo <- cbind(dat$lon, dat$lat, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c('lon','lat', paste0(a, "_", 1:12))
  } else {
    names <- colnames(datOccMo)
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 1980, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1:12), byrow = TRUE)
    datOccMo <- cbind(datOccMo, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c(names, paste0(a, "_", 1:12))
  }
}  
# Sequence Mean
datOccH$Mu <- apply(datOccH[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
  })
# Annual Mean
datOccYrH <- cbind(datOccYr[,1], datOccYr[,2], 
                   apply(datOccYr[,3:ncol(datOccYr)], MARGIN = 1, FUN = am_mean,
                         years = length(1980:2010), model= length(mFileH)
                         ) %>%
                     unlist() %>% 
                     matrix(ncol = length(1980:2010), byrow = TRUE))
colnames(datOccYrH) <- c('lon','lat', paste0(1980:2010))
# Monthly Mean
datOccMoH <- cbind(datOccMo$lon, datOccMo$lat, 
                   apply(datOccMo[,3:ncol(datOccMo)], MARGIN = 1, FUN = am_mean,
                         years = length(1980:2010), model= 12
                         ) %>%
                     unlist() %>% 
                     matrix(ncol = 12, byrow = TRUE))
colnames(datOccYrH) <- c('lon','lat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                         'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(datOccH,file=paste0(fileloc1, loc1[1], 'Results/', 'WAVES_OccHist.csv'),
          row.names = FALSE)
write.csv(datOccYrH,file=paste0(fileloc1, loc1[1], 'Results/', 'WAVES_OccHistYr.csv'),
          row.names = FALSE)
write.csv(datOccMoH,file=paste0(fileloc1, loc1[1], 'Results/', 'WAVES_OccHistMo.csv'),
          row.names = FALSE)

# . . 3.2.2 SSP126 2040-70 -----------------------------------------------------
for (i in 1: length(loc2)){
  print(loc2[i])
  dat <- read_csv(paste0(fileloc1,loc1[2],loc2[i],'WAVES_',timeSpan,var,
                         mFile126[i],2040,'-',2070,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  # Time period number of occurrences
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of total occurrences
  # Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datOcc126_4070$lon <- dat$lon
    datOcc126_4070$lat <- dat$lat
    datOcc126_4070$`CMCC-ESM2` <- as_tibble(Xx)
  } else {
    datOcc126_4070[,2+i] <- as_tibble(Xx)
  }
  # Annual number of occurrences
  print("Annual Occurances")
  if (i == 1){
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2040:2070), byrow = TRUE)
    datOccYr <- cbind(dat$lon, dat$lat, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c('lon','lat', paste0(a, "_", 2040:2070))
  } else {
    names <- colnames(datOccYr)
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2040:2070), byrow = TRUE)
    datOccYr <- cbind(datOccYr, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c(names, paste0(a, "_", 2040:2070))
  }
  # Monthly number of occurrences 
  print("Monthly Occurences")
  if (i == 1){
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = 12, byrow = TRUE)
    datOccMo <- cbind(dat$lon, dat$lat, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c('lon','lat', paste0(a, "_", 1:12))
  } else {
    names <- colnames(datOccMo)
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1:12), byrow = TRUE)
    datOccMo <- cbind(datOccMo, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c(names, paste0(a, "_", 1:12))
  }
}  
# Sequence Mean
datOcc126_4070$Mu <- apply(datOcc126_4070[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})
# Annual Mean
datOcc126_4070Yr <- cbind(datOccYr$lon, datOccYr$lat, 
                   apply(datOccYr[,3:ncol(datOccYr)], MARGIN = 1, FUN = am_mean,
                         years = length(2040:2070), model= length(mFileH)
                   ) %>%
                     unlist() %>% 
                     matrix(ncol = length(2040:2070), byrow = TRUE))
colnames(datOcc126_4070Yr) <- c('lon','lat', paste0(2040:2070))
# Monthly Mean
datOcc126_4070Mo <- cbind(datOccMo$lon, datOccMo$lat, 
                   apply(datOccMo[,3:ncol(datOccMo)], MARGIN = 1, FUN = am_mean,
                         years = length(2040:2070), model= 12
                   ) %>%
                     unlist() %>% 
                     matrix(ncol = 12, byrow = TRUE))
colnames(datOcc126_4070Mo) <- c('lon','lat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                         'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(datOcc126_4070Mo,file=paste0(fileloc1, loc1[2], 'Results/', 'WAVES_Occ126_4070.csv'),
          row.names = FALSE)
write.csv(datOcc126_4070Mo,file=paste0(fileloc1, loc1[2], 'Results/', 'WAVES_Occ126_4070Yr.csv'),
          row.names = FALSE)
write.csv(datOcc126_4070Mo,file=paste0(fileloc1, loc1[2], 'Results/', 'WAVES_Occ126_4070Mo.csv'),
          row.names = FALSE)

# . . 3.2.3 SSP126 2070-2100 ---------------------------------------------------
# Something incorrect with the annual restarted at 9a after replacing with SSP126 2040-70
for (i in 1: length(loc2)){
  print(loc2[i])
  dat <- read_csv(paste0(fileloc1,loc1[2],loc2[i],'WAVES_',timeSpan,var,
                         mFile126[i],2070,'-',2100,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  # Time period number of occurrences
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of total occurrences
  # Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datOcc126_7000$lon <- dat$lon
    datOcc126_7000$lat <- dat$lat
    datOcc126_7000$`CMCC-ESM2` <- as_tibble(Xx)
  } else {
    datOcc126_7000[,2+i] <- as_tibble(Xx)
  }
  # Annual number of occurrences
  print("Annual Occurences")
  if (i == 1){
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2070:2100), byrow = TRUE)
    datOccYr <- cbind(dat$lon, dat$lat, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c('lon','lat', paste0(a, "_", 2070:2100))
  } else {
    names <- colnames(datOccYr)
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2070:2100), byrow = TRUE)
    datOccYr <- cbind(datOccYr, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c(names, paste0(a, "_", 2070:2100))
  }
  # Monthly number of occurrences 
  print("Monthly Occurrences")
  if (i == 1){
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = 12, byrow = TRUE)
    datOccMo <- cbind(dat$lon, dat$lat, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c('lon','lat', paste0(a, "_", 1:12))
  } else {
    names <- colnames(datOccMo)
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1:12), byrow = TRUE)
    datOccMo <- cbind(datOccMo, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c(names, paste0(a, "_", 1:12))
  }
}  
# Sequence Mean
datOcc126_7000$Mu <- apply(datOcc126_7000[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})
# Annual Mean
datOcc126_7000Yr <- cbind(datOccYr$lon, datOccYr$lat, 
                          apply(datOccYr[,3:ncol(datOccYr)], MARGIN = 1, FUN = am_mean,
                                years = length(2070:2100), model= length(mFileH)
                          ) %>%
                            unlist() %>% 
                            matrix(ncol = length(2070:2100), byrow = TRUE))
colnames(datOcc126_7000Yr) <- c('lon','lat', paste0(2070:2100))
# Monthly Mean
datOcc126_7000Mo <- cbind(datOccMo$lon, datOccMo$lat, 
                          apply(datOccMo[,3:ncol(datOccMo)], MARGIN = 1, FUN = am_mean,
                                years = length(2070:2100), model= 12
                          ) %>%
                            unlist() %>% 
                            matrix(ncol = 12, byrow = TRUE))
colnames(datOcc126_7000Mo) <- c('lon','lat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(datOcc126_7000Mo, file=paste0(fileloc1, loc1[2], 'Results/', 'WAVES_Occ126_7000.csv'),
          row.names = FALSE)
write.csv(datOcc126_7000Mo,file=paste0(fileloc1, loc1[2], 'Results/', 'WAVES_Occ126_7000Yr.csv'),
          row.names = FALSE)
write.csv(datOcc126_7000Mo,file=paste0(fileloc1, loc1[2], 'Results/', 'WAVES_Occ126_7000Mo.csv'),
          row.names = FALSE)
# . . 3.2.4 SSP585 2040-70 -----------------------------------------------------
for (i in 1: length(loc2)){
  print(loc2[i])
  dat <- read_csv(paste0(fileloc1,loc1[3],loc2[i],'WAVES_',timeSpan,var,
                         mFile585[i],2040,'-',2070,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  # Time period number of occurrences
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of total occurrences
  # Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datOcc585_4070$lon <- dat$lon
    datOcc585_4070$lat <- dat$lat
    datOcc585_4070$`CMCC-ESM2` <- as_tibble(Xx)
  } else {
    datOcc585_4070[,2+i] <- as_tibble(Xx)
  }
  # Annual number of occurrences
  print("Annual Occurrences")
  if (i == 1){
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2040:2070), byrow = TRUE)
    datOccYr <- cbind(dat$lon, dat$lat, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c('lon','lat', paste0(a, "_", 2040:2070))
  } else {
    names <- colnames(datOccYr)
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2040:2070), byrow = TRUE)
    datOccYr <- cbind(datOccYr, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c(names, paste0(a, "_", 2040:2070))
  }
  # Monthly number of occurrences 
  print("Monthly Occurrences")
  if (i == 1){
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = 12, byrow = TRUE)
    datOccMo <- cbind(dat$lon, dat$lat, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c('lon','lat', paste0(a, "_", 1:12))
  } else {
    names <- colnames(datOccMo)
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2040, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1:12), byrow = TRUE)
    datOccMo <- cbind(datOccMo, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c(names, paste0(a, "_", 1:12))
  }
}  
# Sequence Mean
datOcc585_4070$Mu <- apply(datOcc585_4070[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})
# Annual Mean
datOcc585_4070Yr <- cbind(datOccYr$lon, datOccYr$lat, 
                          apply(datOccYr[,3:ncol(datOccYr)], MARGIN = 1, FUN = am_mean,
                                years = length(2040:2070), model= length(mFileH)
                          ) %>%
                            unlist() %>% 
                            matrix(ncol = length(2040:2070), byrow = TRUE))
colnames(datOcc585_4070Yr) <- c('lon','lat', paste0(2040:2070))
# Monthly Mean
datOcc585_4070Mo <- cbind(datOccMo$lon, datOccMo$lat, 
                          apply(datOccMo[,3:ncol(datOccMo)], MARGIN = 1, FUN = am_mean,
                                years = length(2040:2070), model= 12
                          ) %>%
                            unlist() %>% 
                            matrix(ncol = 12, byrow = TRUE))
colnames(datOcc585_4070Mo) <- c('lon','lat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(datOcc585_4070Mo,file=paste0(fileloc1, loc1, 'Results/', 'WAVES_Occ585_4070.csv'),
          row.names = FALSE)
write.csv(datOcc585_4070Mo,file=paste0(fileloc1, loc1, 'Results/', 'WAVES_Occ585_4070Yr.csv'),
          row.names = FALSE)
write.csv(datOcc585_4070Mo,file=paste0(fileloc1, loc1, 'Results/', 'WAVES_Occ585_4070Mo.csv'),
          row.names = FALSE)

# . . 3.2.5 SSP585 2070-2100 ---------------------------------------------------
for (i in 1: length(loc2)){
  print(loc2[i])
  dat <- read_csv(paste0(fileloc1,loc1[3],loc2[i],'WAVES_',timeSpan,var,
                         mFile585[i],2070,'-',2100,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  # Time period number of occurrences
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of total occurrences
  # Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datOcc585_7000$lon <- dat$lon
    datOcc585_7000$lat <- dat$lat
    datOcc585_7000$`CMCC-ESM2` <- as_tibble(Xx)
  } else {
    datOcc585_7000[,2+i] <- as_tibble(Xx)
  }
  # Annual number of occurrences
  print("Annual Occurrences")
  if (i == 1){
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2070:2100), byrow = TRUE)
    datOccYr <- cbind(dat$lon, dat$lat, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c('lon','lat', paste0(a, "_", 2070:2100))
  } else {
    names <- colnames(datOccYr)
    datYr <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = annual_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(2070:2100), byrow = TRUE)
    datOccYr <- cbind(datOccYr, datYr)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccYr) <- c(names, paste0(a, "_", 2070:2100))
  }
  # Monthly number of occurrences
  print("Monthly Occurrences")
  if (i == 1){
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = 12, byrow = TRUE)
    datOccMo <- cbind(dat$lon, dat$lat, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c('lon','lat', paste0(a, "_", 1:12))
  } else {
    names <- colnames(datOccMo)
    datMo <- apply(X = dat[,3:ncol(dat)], MARGIN = 1, FUN = month_occ,
                   yrSt = 2070, leap = leapM[i]) %>%
      unlist() %>%
      matrix(ncol = length(1:12), byrow = TRUE)
    datOccMo <- cbind(datOccMo, datMo)
    a <- strsplit(loc2[i],'/') %>% unlist() 
    colnames(datOccMo) <- c(names, paste0(a, "_", 1:12))
  }
}  
# Sequence Mean
datOcc585_7000$Mu <- apply(datOcc585_4070[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})
# Annual Mean
datOcc585_7000Yr <- cbind(datOccYr$lon, datOccYr$lat, 
                          apply(datOccYr[,3:ncol(datOccYr)], MARGIN = 1, FUN = am_mean,
                                years = length(2070:2100), model= length(mFileH)
                          ) %>%
                            unlist() %>% 
                            matrix(ncol = length(2070:2100), byrow = TRUE))
colnames(datOcc585_7000Yr) <- c('lon','lat', paste0(2070:2100))
# Monthly Mean
datOcc585_7000Mo <- cbind(datOccMo$lon, datOccMo$lat, 
                          apply(datOccMo[,3:ncol(datOccMo)], MARGIN = 1, FUN = am_mean,
                                years = length(2070:2100), model= 12
                          ) %>%
                            unlist() %>% 
                            matrix(ncol = 12, byrow = TRUE))
colnames(datOcc585_7000Mo) <- c('lon','lat', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(datOcc585_7000Mo,file=paste0(fileloc1, loc1[3], 'Results/', 'WAVES_Occ585_7000.csv'),
          row.names = FALSE)
write.csv(datOcc585_7000Mo,file=paste0(fileloc1, loc1[3], 'Results/', 'WAVES_Occ585_7000Yr.csv'),
          row.names = FALSE)
write.csv(datOcc585_7000Mo,file=paste0(fileloc1, loc1[3], 'Results/', 'WAVES_Occ585_7000Mo.csv'),
          row.names = FALSE)

# . 3.3 Creating a table of values ---------------------------------------------
# . . 3.3.1 Mean Mu ------------------------------------------------------------
# . . 3.3.2 Annual Mu ----------------------------------------------------------
# . . 3.3.3 Month Mu -----------------------------------------------------------
# . 3.4 Plotting the Wave Occurrences Mean -------------------------------------
# . . 3.4.1 Mean Mu ------------------------------------------------------------
# . . 3.4.2 Annual Mu ----------------------------------------------------------
# . . 3.4.3 Month Mu -----------------------------------------------------------










# END ##########################################################################