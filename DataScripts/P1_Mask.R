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
library(cowplot)
library(geosphere)

# Part I Variables To Change ###################################################
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

# Part II Functions ############################################################
landArea <- function(center){
  # This function will receive the center of the grid sell and convert it to a 
  # polygon. The polygon is calculated from the top left to top right to bottom 
  # right to bottom left and back to the top left for a total of five points. 
  # Then the polygon points will be imputed into geosphere::areaPolygon to 
  # calculate the area of the polygon. The area of the polygon is given in 
  # square meters. This is converted into square km.
  #
  # center <- c(lon, lat)
  # Assumed to be for Earth. See geosphere documentation for values
  
  require(geosphere)
  
  ptX1 <- center[1] - 0.5
  if(center[1] == 180) { ptX2 <- -179.5} else { ptX2 <- center[1] + 0.5}
  if(center[1] == 180) { ptX3 <- -179.5} else { ptX3 <- center[1] + 0.5}
  ptX4 <- center[1] - 0.5
  
  ptY1 <- center[2] + 0.5
  ptY2 <- center[2] + 0.5
  ptY3 <- center[2] - 0.5
  ptY4 <- center[2] - 0.5
  
  poly <- rbind(c(ptX1, ptY1), c(ptX2, ptY2), c(ptX3, ptY3), c(ptX4, ptY4), 
                c(ptX1, ptY1))
  
  area <- geosphere::areaPolygon(poly) # in square meters 
  
  # Convert to km^2
  area <- area / 1000000
  return(area)
}

# Part III Mask creation #######################################################
for (mNum in 1: length(mFile)){
  # . 3.1 Opening the soil moisture file -----------------------------------------
  # Soil moisture file is used for the creation of a mask(s) because it is only 
  # land.
  print(mNum)
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
  
  # . 3.2 Test Plotting ----------------------------------------------------------
  dat <- raster::raster(t(varNC[,,1]), xmn=min(lonNC), xmx=max(lonNC),
                        ymn=min(latNC), ymx=max(latNC)) %>%
    raster::flip(direction = 2)
  sp::plot(dat)
  
  # . 3.3 Making the mask --------------------------------------------------------
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
  # . 3.4 Modifying longitude ----------------------------------------------------
  # Starts at 0deg (Africa)
  seqA <- seq(180,0,by= -1)
  seqB <- seq(180, 360,by= 1)
  
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
  # . . 3.4.1 Test plotting 
  baseData <- map_data('world')
  ggplot(data=dat, aes(x=lon2, y=lat, fill=SMmask)) +
    geom_tile() +
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
                 colour="black", fill="NA", linewidth=0.5) +
    labs(title = paste0(loc1, loc2), 
         x = 'longitude', y = 'latitude')
  ggsave(filename = paste0(fileloc1, loc1, loc2[mNum], 'MASK', mFile[mNum], ".tiff"),
         width = 8, height = 5.5, dpi = 350, bg='white')
  
  # . 3.5 Saving -----------------------------------------------------------------
  write.csv(dat, file=paste0(fileloc1, loc1, loc2[mNum], 'MASK', mFile[mNum], '.csv'),
            row.names = FALSE)
  rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'mFile','mNum')])
  
}

# Part IV Combining SM Mask ####################################################
# . 4.1 Opening & prepossessing the files --------------------------------------
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
mask$Diff <- mask$FullMask
mask$FullMask[mask$FullMask < 8] <- NA
mask$FullMask[mask$FullMask == 8] <- 1
mask$Diff[mask$Diff == 0] <- NA
mask$Diff <- as.factor(mask$Diff)

# . 4.2 Plotting and saving ----------------------------------------------------
baseData <- map_data('world')
p1 <- ggplot(data=mask, aes(x=lon2, y=lat, fill=FullMask)) +
  geom_tile() +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  labs(title = paste0(loc1, loc2), 
       x = 'longitude', y = 'latitude')
p2 <- ggplot(data=mask, aes(x=lon2, y=lat, fill=Diff)) +
  geom_tile() +
  scale_fill_discrete() +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  labs(title = paste0(loc1, loc2), 
       x = 'longitude', y = 'latitude')
p1 <- plot_grid(p1, p2, 
                nrow = 1,# labels = c('A','B','C','D'),
                rel_widths = c(1,1))

ggsave(p1, filename = paste0(fileloc1, loc1, 'MASK_FULL', ".tiff"),
       width = 8, height = 3, dpi = 350, bg='white')

write.csv(mask, file=paste0(fileloc1, loc1, 'MASK_FULL', '.csv'),
          row.names = FALSE)
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'mFile', 'mask')])

# Part V Exploring the files ###################################################
# . 5.1 Opening the file -------------------------------------------------------
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
# . 5.2 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'baseData')])
# Part VI Spherical trigonometry ###############################################
# . 6.1 Variables Needed -------------------------------------------------------
mask <- read_csv(paste0(fileloc1,'CMIP6_historical/','MASK_FULL.csv'),
                 col_names = TRUE, cols(.default = col_double()))
mask$LandAreakm2 <- 0

# . 6.2 Calculations -----------------------------------------------------------
mask$LandAreakm2 <- apply(X = cbind(mask$lon2, mask$lat), MARGIN = 1, FUN = landArea)

write.csv(mask, file=paste0(fileloc1, loc1, 'MASK_FULL', '.csv'),
          row.names = FALSE)

### END ########################################################################