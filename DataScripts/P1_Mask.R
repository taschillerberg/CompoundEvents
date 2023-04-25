# P1_Mask.R
# 
# About: This program will create a land mask using the CMIP6 historical 
#      soil moisture data from the first avaliable year (1980) for each model.

# TA Schillerberg
#               Oct. 2022
#      Updated: Apr. 2023

# Mac
# setwd("~/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Code2")
# fileloc1 <- '~/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Data/'
# Office Computer
setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# Libraries ####################################################################
library(tidyverse)
library(ncdf4)

# Part I Variables To Change ###################################################
mNum <- 1 # Select a model (1-8)
mFile <- c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_',
           '_day_EC-Earth3_historical_r1i1p1f1_gr_',
           '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
           '_day_INM-CM4-8_historical_r1i1p1f1_gr1_',
           '_day_INM-CM5-0_historical_r1i1p1f1_gr1_',
           '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
           '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_',
           '_day_NorESM2-MM_historical_r1i1p1f1_gn_')
loc1 <- 'CMIP6_historical/'
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')

# Part II Mask creation ########################################################
# . 2.1 Opening the soil moisture file -----------------------------------------
# Soil moisture file is used for the creation of a mask(s) because it is only 
# land.
if (mNum == 1){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 2){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1980,'0101-',1980,'1231.nc'))
} else if (mNum == 3){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1970,'0101-',1989,'1231.nc'))
} else if (mNum == 4){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 5){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 6){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1980,'0101-',1984,'1231.nc'))
} else if (mNum == 7){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 8){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[mNum],'regrid360x180_mrsos',mFile[mNum],1980,'0101-',1989,'1231.nc'))
} else {
  print('Model Number (mNum) not reconized.')
}
lonNC <- ncdf4::ncvar_get(datNC, 'lon')
latNC <- ncdf4::ncvar_get(datNC, 'lat')
# tNC <- ncdf4::ncvar_get(datNC, 'time')
# depthNC <- ncdf4::ncvar_get(datNC, 'depth')
varNC <- ncdf4::ncvar_get(datNC, 'mrsos')
fillvalue <- ncdf4::ncatt_get(datNC, 'mrsos', '_FillValue')
varNC[varNC == fillvalue$value] <- NA
ncdf4::nc_close(datNC)

# . 2.2 Test Plotting ----------------------------------------------------------
dat <- raster::raster(t(varNC[,,1]), xmn=min(lonNC), xmx=max(lonNC),
                      ymn=min(latNC), ymx=max(latNC)) %>%
  raster::flip(direction = 2)
sp::plot(dat)

# . 2.3 Making the mask --------------------------------------------------------
dat <- as_tibble(matrix(varNC[,,1], ncol=1, byrow=TRUE),
                 .name_repair = 'minimal')
dat <- cbind(expand.grid(lonNC,latNC) %>%
               tibble(),
             dat)
colnames(dat) <- c('lon','lat','SMmask')
summary(dat)
# Convert ocean values to NA
if (mNum == 3| mNum == 4 | mNum == 5 | mNum == 8){
  dat$SMmask[dat$SMmask > 0] <-1
  dat$SMmask[dat$SMmask <= 0] <- NA
} else {
  dat$SMmask[dat$SMmask >= -2] <- 1
}
summary(dat)
# . 2.4 Modifying longitude ----------------------------------------------------
# Starts at 0deg (Africa)
seqA <- seq(180,0,by= -1)
seqB <- seq(180, 360,by= 1)

# if (mNum == 2){
#   seqA <- seq(179.375,0.625,by= -(lonNC[3]-lonNC[2]))
#   seqB <- seq(180.625, 360.625,by= (lonNC[3]-lonNC[2]))
# } else {
#   seqA <- seq(180,0,by= -1)
#   seqB <- seq(180, 360,by= 1)
# }

dat$lon2 <- dat$lon
i <- 2
repeat{
  m <- which(dat$lon == seqB[i])
  if (length(m) == 0){
    m <- which(dat$lon == seqB[i-1])
    m <- m + 1
    dat$lon2[m] <- seqA[i] * (-1)
    i <- i + 1
  } else {
    dat$lon2[m] <- seqA[i] * (-1)
    i <- i + 1
  }
  if (i == length(seqB)){ break }
}
# . . 2.4.1 Test plotting 
baseData <- map_data('world')
ggplot(data=dat, aes(x=lon2, y=lat, fill=SMmask)) +
  geom_tile() +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  labs(title = paste0(loc1, loc2), 
       x = 'longitude', y = 'latitude')
ggsave(filename = paste0(fileloc1, loc1, loc2, 'MASK', mFile[mNum], ".tiff"),
       width = 8, height = 5.5, dpi = 350, bg='white')

# . 2.5 Saving -----------------------------------------------------------------
write.csv(dat, file=paste0(fileloc1, loc1, loc2[mNum], 'MASK', mFile[mNum], '.csv'),
          row.names = FALSE)
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'mFile','mNum')])

# Part III Combining SM Mask ###################################################
# . 3.1 Opening & prepossessing the files --------------------------------------
mask <- read_csv(paste0(fileloc1, loc1, loc2[1], 'MASK', mFile[1], '.csv'), 
                col_names = TRUE, cols(.default = col_double()))
mask <- cbind(mask$lon, mask$lon2, mask$lat, mask$SMmask)
colnames(mask) <- c('lon','lon2','lat',strsplit(loc2[1],'/'))
mask <- as_tibble(mask)

for (i in 2:8){
  dat <- read_csv(paste0(fileloc1, loc1, loc2[i], 'MASK', mFile[i], '.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
  name <- strsplit(loc2[i],'/') %>% 
    unlist()
  mask <- cbind(mask, dat$SMmask)
  colnames(mask)[3+i] <- name
}

mask$FullMask <- apply(mask[4:11], MARGIN = 1, FUN = sum, na.rm=TRUE)
mask$FullMask[mask$FullMask < 8] <- NA
mask$FullMask[mask$FullMask == 8] <- 1

# . 3.2 Plotting and saving ----------------------------------------------------
baseData <- map_data('world')
ggplot(data=mask, aes(x=lon2, y=lat, fill=FullMask)) +
  geom_tile() +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  labs(title = paste0(loc1, loc2), 
       x = 'longitude', y = 'latitude')
ggsave(filename = paste0(fileloc1, loc1, 'MASK_FULL', ".tiff"),
       width = 8, height = 5.5, dpi = 350, bg='white')

write.csv(mask, file=paste0(fileloc1, loc1, 'MASK_FULL', '.csv'),
          row.names = FALSE)
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'mFile')])

# Part IV Exploring the files ##################################################
# . 4.1 Opening the file -------------------------------------------------------
var <- c('tasmax','tasmin','pr','mrsos')
if (mNum == 1){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[1],'regrid360x180_mrsos',mFile[1],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 2){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[2],'regrid360x180_mrsos',mFile[2],1980,'0101-',1980,'1231.nc'))
} else if (mNum == 3){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[3],'regrid360x180_mrsos',mFile[3],1970,'0101-',1989,'1231.nc'))
} else if (mNum == 4){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[4],'regrid360x180_mrsos',mFile[4],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 5){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[5],'regrid360x180_mrsos',mFile[5],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 6){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[6],'regrid360x180_mrsos',mFile[6],1980,'0101-',1984,'1231.nc'))
} else if (mNum == 7){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[7],'regrid360x180_mrsos',mFile[7],1950,'0101-',1999,'1231.nc'))
} else if (mNum == 8){
  datNC <- ncdf4::nc_open(paste0(fileloc1,loc1,loc2[8],'regrid360x180_mrsos',mFile[8],1980,'0101-',1989,'1231.nc'))
} else {
  print('Model Number (mNum) not reconized.')
}
print(datNC)
tNC <- ncdf4::ncvar_get(datNC, 'time')
tNC
tail(tNC)
ncdf4::nc_close(datNC)

### END ###