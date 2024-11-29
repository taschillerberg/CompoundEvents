# P9_ModelEvaluation.R
# About: This script will create plots for the publication.
# 
# Inputs:
# Outputs: Figures and Supplemental Figures
#
# T. A. Schillerberg
#               Jul. 2024
#      Updated: Jul. 2024
#
#
# Computer
setwd("Source File Location") 
fileloc1 <- 'Main project folder' 

# Libraries ####################################################################
library(tidyverse)

# Part 1 Variables ####################################################################
loc1 <- c('CMIP6_historical/','Historical/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/',
          'ERA5/')
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
varT <- c('Heatwave','Coldwave','Extreme Precipitation','Flash Drought')
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')
compT <- c('Sim. Heatwave & Flash Drought','Seq. Heatwave & Flash Drought',
           'Seq. Flash Drought & Heatwave','Sim. Heatwave & Extreme Precip',
           'Seq. Heatwave & Extreme Precipitation',
           'Seq. Extreme Precip & Heatwave',
           'Seq. Extreme Precip & Flash Drought',
           'Seq. Flash Drought & Extreme Precipitation')

locT <- c('Africa','China', 'Europe', 'India ','North America', 
          'Oceania', 'South America')
locTLev <- c('Africa','China', 'Europe', 'India ','North America', 
             'Oceania', 'South America')
lon1 <- c( -17, 94, -10, 73,-125,  94, -82)
lon2 <- c( 50, 124,  50, 90, -66, 153, -34)
lat1 <- c( 30,  45,  56, 36,  55,  10,  13)
lat2 <- c(-35,  20,  36, 10,  23, -35, -35)


options(show.error.locations = TRUE)
# Part 2 Extremes ##############################################################
# . 2.1 Opening Files ----------------------------------------------------------
relV1 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV3 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[3],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV4 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))

obs1 <- read_csv(paste0(fileloc1,'Data/',loc1[2],loc2[9],'OCC_DAY_',var[1],
                        '_Hist_ERA5_8010.csv'), 
                 col_names = TRUE, cols(.default = col_double()))
obs3 <- read_csv(paste0(fileloc1,'Data/',loc1[2],loc2[9],'OCC_DAY_',var[3],
                        '_Hist_ERA5_8010.csv'), 
                 col_names = TRUE, cols(.default = col_double()))
obs4 <- read_csv(paste0(fileloc1,'Data/',loc1[2],loc2[9],'OCC_DAY_',var[4],
                        '_Hist_ERA5_8010.csv'), 
                 col_names = TRUE, cols(.default = col_double()))
# . 2.2 Regions ----------------------------------------------------------------
dat1 <- relV1 %>%
  select(lon, lat, Historical_Mu) %>%
  cbind(obs1$ERA5)
dat3 <- relV3 %>%
  select(lon, lat, Historical_Mu) %>%
  cbind(obs3$ERA5)
dat4 <- relV4 %>%
  select(lon, lat, Historical_Mu) %>%
  cbind(obs4$ERA5)

colnames(dat1) <- c('lon','lat','Historical_Mu','ERA5')
colnames(dat3) <- c('lon','lat','Historical_Mu','ERA5')
colnames(dat4) <- c('lon','lat','Historical_Mu','ERA5')

# Check distribution of errors
hist(dat1$Historical_Mu - dat1$ERA5)
hist(dat3$Historical_Mu - dat3$ERA5)
hist(dat4$Historical_Mu - dat4$ERA5)


datME1 <- tibble(
  'Region' = character(length = 8),
  'RMSE' = numeric(length = 8),
  'MAE' = numeric(length = 8),
  'BIAS' = numeric(length = 8)
)
datME3 <- tibble(
  'Region' = character(length = 8),
  'RMSE' = numeric(length = 8),
  'MAE' = numeric(length = 8),
  'BIAS' = numeric(length = 8)
)
datME4 <- tibble(
  'Region' = character(length = 8),
  'RMSE' = numeric(length = 8),
  'MAE' = numeric(length = 8),
  'BIAS' = numeric(length = 8)
)

# Root Mean Square Error sqrt(mean((actual - predicted)^2))
# Mean Absolute Error mean(abs(actual - predicted))
hist(dat1$ERA5 - dat1$Historical_Mu)

for (i in 1:length(locT)){
  lonA <- lon1[i]
  lonB <- lon2[i]
  latA <- lat1[i]
  latB <- lat2[i]
  
  dat <- dat1
  dat$lon[dat$lon < lonA | dat$lon > lonB] <- NA
  dat$lat[dat$lat > latA | dat$lat < latB] <- NA
  dat <- na.omit(dat)
  datME1$Region[i] <- locT[i]
  datME1$RMSE[i] <- 
    Metrics::rmse(dat$ERA5,dat$Historical_Mu)
  datME1$BIAS[i] <- Metrics::bias(dat$ERA5, dat$Historical_Mu)
  datME1$MAE[i] <- Metrics::mae(dat$ERA5, dat$Historical_Mu)
  
  dat <- dat3
  dat$lon[dat$lon < lonA | dat$lon > lonB] <- NA
  dat$lat[dat$lat > latA | dat$lat < latB] <- NA
  dat <- na.omit(dat)
  datME3$Region[i] <- locT[i]
  datME3$RMSE[i] <- 
    Metrics::rmse(dat$ERA5,dat$Historical_Mu)
  datME3$BIAS[i] <- Metrics::bias(dat$ERA5, dat$Historical_Mu)
  datME3$MAE[i] <- Metrics::mae(dat$ERA5, dat$Historical_Mu)
  
  dat <- dat4
  dat$lon[dat$lon < lonA | dat$lon > lonB] <- NA
  dat$lat[dat$lat > latA | dat$lat < latB] <- NA
  dat <- na.omit(dat)
  datME4$Region[i] <- locT[i]
  datME4$RMSE[i] <- 
    Metrics::rmse(dat$ERA5,dat$Historical_Mu)
  datME4$BIAS[i] <- Metrics::bias(dat$ERA5, dat$Historical_Mu)
  datME4$MAE[i] <- Metrics::mae(dat$ERA5, dat$Historical_Mu)
}

datME1$Region[i+1] <- 'Global'
datME1$RMSE[i+1] <-  sqrt(mean((dat1$ERA5-dat1$Historical_Mu)^2))
Metrics::rmse(dat1$ERA5,dat1$Historical_Mu)
datME1$BIAS[i] <- Metrics::bias(dat1$ERA5, dat1$Historical_Mu)
datME1$MAE[i] <- Metrics::mae(dat1$ERA5, dat1$Historical_Mu)

datME3$Region[i+1] <- 'Global'
datME3$RMSE[i+1] <-  sqrt(mean((dat3$ERA5-dat3$Historical_Mu)^2))
Metrics::rmse(dat3$ERA5,dat3$Historical_Mu)
datME3$BIAS[i] <- Metrics::bias(dat3$ERA5, dat3$Historical_Mu)
datME3$MAE[i] <- Metrics::mae(dat3$ERA5, dat3$Historical_Mu)

datME4$Region[i+1] <- 'Global'
datME4$RMSE[i+1] <-  sqrt(mean((dat4$ERA5-dat4$Historical_Mu)^2))
Metrics::rmse(dat4$ERA5,dat4$Historical_Mu)
datME4$BIAS[i] <- Metrics::bias(dat4$ERA5, dat4$Historical_Mu)
datME4$MAE[i] <- Metrics::mae(dat4$ERA5, dat4$Historical_Mu)

datV <-cbind(datME1$Region, 
             round(datME1$RMSE,4), round(datME1$BIAS,4), round(datME1$MAE,4),
             round(datME3$RMSE,4), round(datME3$BIAS,4), round(datME3$MAE,4), 
             round(datME4$RMSE,4), round(datME4$BIAS,4), round(datME4$MAE,4))
colnames(datV) <- c('Region', 
                    paste0(var[1], '_RMSE'), paste0(var[1], '_BIAS'), paste0(var[1], '_MAE'), 
                    paste0(var[3], '_RMSE'), paste0(var[3], '_BIAS'), paste0(var[3], '_MAE'), 
                    paste0(var[4], '_RMSE'), paste0(var[4], '_BIAS'), paste0(var[4], '_MAE'))
write.csv(datV, file = 
            paste0(fileloc1,'Data/Results/', 'MODEL_EVAL_VAR','.csv'), 
          row.names = FALSE)
# . 2.3 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2',
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2',
                           'datV')])
# Part 3 Compound Events #######################################################
# . 3.1 Opening Files ----------------------------------------------------------
relV1 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[1],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV2 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[2],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV3 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[3],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV8 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[8],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))

obs1 <- read_csv(paste0(fileloc1,'Data/',loc1[2],loc2[9],'OCC_DAY_',comp[1],
                        '_Hist_ERA5_8010.csv'), 
                 col_names = TRUE, cols(.default = col_double()))
obs2 <- read_csv(paste0(fileloc1,'Data/',loc1[2],loc2[9],'OCC_DAY_',comp[2],
                        '_Hist_ERA5_8010.csv'), 
                 col_names = TRUE, cols(.default = col_double()))
obs3 <- read_csv(paste0(fileloc1,'Data/',loc1[2],loc2[9],'OCC_DAY_',comp[3],
                        '_Hist_ERA5_8010.csv'), 
                 col_names = TRUE, cols(.default = col_double()))
obs8 <- read_csv(paste0(fileloc1,'Data/',loc1[2],loc2[9],'OCC_DAY_',comp[8],
                        '_Hist_ERA5_8010.csv'), 
                 col_names = TRUE, cols(.default = col_double()))
# . 3.2 Regions ----------------------------------------------------------------
dat1 <- relV1 %>%
  select(lon, lat, Historical_mu) %>%
  cbind(obs1$ERA5)
dat2 <- relV2 %>%
  select(lon, lat, Historical_mu) %>%
  cbind(obs2$ERA5)
dat3 <- relV3 %>%
  select(lon, lat, Historical_mu) %>%
  cbind(obs3$ERA5)
dat8 <- relV8 %>%
  select(lon, lat, Historical_mu) %>%
  cbind(obs8$ERA5)
colnames(dat1) <- c('lon','lat','Historical_Mu','ERA5')
colnames(dat2) <- c('lon','lat','Historical_Mu','ERA5')
colnames(dat3) <- c('lon','lat','Historical_Mu','ERA5')
colnames(dat8) <- c('lon','lat','Historical_Mu','ERA5')

# Check distribution of errors
hist(dat1$Historical_Mu - dat1$ERA5)
hist(dat2$Historical_Mu - dat2$ERA5)
hist(dat3$Historical_Mu - dat3$ERA5)
hist(dat8$Historical_Mu - dat8$ERA5)

datME1 <- tibble(
  'Region' = character(length = 8),
  'RMSE' = numeric(length = 8),
  'MAE' = numeric(length = 8),
  'BIAS' = numeric(length = 8)
)
datME2 <- tibble(
  'Region' = character(length = 8),
  'RMSE' = numeric(length = 8),
  'MAE' = numeric(length = 8),
  'BIAS' = numeric(length = 8)
)
datME3 <- tibble(
  'Region' = character(length = 8),
  'RMSE' = numeric(length = 8),
  'MAE' = numeric(length = 8),
  'BIAS' = numeric(length = 8)
)
datME8 <- tibble(
  'Region' = character(length = 8),
  'RMSE' = numeric(length = 8),
  'MAE' = numeric(length = 8),
  'BIAS' = numeric(length = 8)
)

# Root Mean Square Error sqrt(mean((actual - predicted)^2))
# Mean Absolute Error mean(abs(actual - predicted))

for (i in 1:length(locT)){
  lonA <- lon1[i]
  lonB <- lon2[i]
  latA <- lat1[i]
  latB <- lat2[i]
  
  dat <- dat1
  dat$lon[dat$lon < lonA | dat$lon > lonB] <- NA
  dat$lat[dat$lat > latA | dat$lat < latB] <- NA
  dat <- na.omit(dat)
  datME1$Region[i] <- locT[i]
  datME1$RMSE[i] <- 
    Metrics::rmse(dat$ERA5,dat$Historical_Mu)
  datME1$BIAS[i] <- Metrics::bias(dat$ERA5, dat$Historical_Mu)
  datME1$MAE[i] <- Metrics::mae(dat$ERA5, dat$Historical_Mu)
  
  if (i == 2){
    print(summary(dat))
  }
  
  dat <- dat2
  dat$lon[dat$lon < lonA | dat$lon > lonB] <- NA
  dat$lat[dat$lat > latA | dat$lat < latB] <- NA
  dat <- na.omit(dat)
  datME2$Region[i] <- locT[i]
  datME2$RMSE[i] <- 
    Metrics::rmse(dat$ERA5,dat$Historical_Mu)
  datME2$BIAS[i] <- Metrics::bias(dat$ERA5, dat$Historical_Mu)
  datME2$MAE[i] <- Metrics::mae(dat$ERA5, dat$Historical_Mu)
  
  if (i == 2){
    print(summary(dat))
  }
  
  dat <- dat3
  dat$lon[dat$lon < lonA | dat$lon > lonB] <- NA
  dat$lat[dat$lat > latA | dat$lat < latB] <- NA
  dat <- na.omit(dat)
  datME3$Region[i] <- locT[i]
  datME3$RMSE[i] <- 
    Metrics::rmse(dat$ERA5,dat$Historical_Mu)
  datME3$BIAS[i] <- Metrics::bias(dat$ERA5, dat$Historical_Mu)
  datME3$MAE[i] <- Metrics::mae(dat$ERA5, dat$Historical_Mu)
  
  if (i == 2){
    print(summary(dat))
  }
  
  dat <- dat8
  dat$lon[dat$lon < lonA | dat$lon > lonB] <- NA
  dat$lat[dat$lat > latA | dat$lat < latB] <- NA
  dat <- na.omit(dat)
  datME8$Region[i] <- locT[i]
  datME8$RMSE[i] <- 
    Metrics::rmse(dat$ERA5,dat$Historical_Mu)
  datME8$BIAS[i] <- Metrics::bias(dat$ERA5, dat$Historical_Mu)
  datME8$MAE[i] <- Metrics::mae(dat$ERA5, dat$Historical_Mu)
  
  if (i == 2){
    print(summary(dat))
  }
}

datME1$Region[i+1] <- 'Global'
datME1$RMSE[i+1] <-  sqrt(mean((dat1$ERA5-dat1$Historical_Mu)^2))
Metrics::rmse(dat1$ERA5,dat1$Historical_Mu)
datME1$BIAS[i] <- Metrics::bias(dat1$ERA5, dat1$Historical_Mu)
datME1$MAE[i] <- Metrics::mae(dat1$ERA5, dat1$Historical_Mu)

datME2$Region[i+1] <- 'Global'
datME2$RMSE[i+1] <-  sqrt(mean((dat2$ERA5-dat2$Historical_Mu)^2))
Metrics::rmse(dat2$ERA5,dat2$Historical_Mu)
datME2$BIAS[i] <- Metrics::bias(dat2$ERA5, dat2$Historical_Mu)
datME2$MAE[i] <- Metrics::mae(dat2$ERA5, dat2$Historical_Mu)

datME3$Region[i+1] <- 'Global'
datME3$RMSE[i+1] <-  sqrt(mean((dat3$ERA5-dat3$Historical_Mu)^2))
Metrics::rmse(dat3$ERA5,dat3$Historical_Mu)
datME3$BIAS[i] <- Metrics::bias(dat3$ERA5, dat3$Historical_Mu)
datME1$MAE[i] <- Metrics::mae(dat3$ERA5, dat3$Historical_Mu)

datME8$Region[i+1] <- 'Global'
datME8$RMSE[i+1] <-  sqrt(mean((dat8$ERA5-dat8$Historical_Mu)^2))
Metrics::rmse(dat8$ERA5,dat8$Historical_Mu)
datME8$BIAS[i] <- Metrics::bias(dat8$ERA5, dat8$Historical_Mu)
datME8$MAE[i] <- Metrics::mae(dat8$ERA5, dat8$Historical_Mu)

datC <- cbind(datME1$Region, 
              round(datME1$RMSE,4), round(datME1$BIAS,4), round(datME1$MAE,4),
              round(datME2$RMSE,4), round(datME2$BIAS,4), round(datME2$MAE,4),
              round(datME3$RMSE,4), round(datME3$BIAS,4), round(datME3$MAE,4),
              round(datME8$RMSE,4), round(datME8$BIAS,4), round(datME8$MAE,4))
colnames(datC) <- c('Region', 
                    paste0(comp[1], '_RMSE'), paste0(comp[1], '_BIAS'), paste0(comp[1], '_MAE'), 
                    paste0(comp[2], '_RMSE'), paste0(comp[2], '_BIAS'), paste0(comp[2], '_MAE'), 
                    paste0(comp[3], '_RMSE'), paste0(comp[3], '_BIAS'), paste0(comp[3], '_MAE'), 
                    paste0(comp[8], '_RMSE'), paste0(comp[8], '_BIAS'), paste0(comp[8], '_MAE'))
write.csv(datC, file = 
            paste0(fileloc1,'Data/Results/', 'MODEL_EVAL_COMP','.csv'), 
          row.names = FALSE)
# END ##########################################################################