# P5_LandChangePop.R
# About: This program will change the land use and population data into a 
#        useable format to determine exposure.
# 
# Inputs: LUH2, popdynamics
# Outputs: 
#
# T. A. Schillerberg
#               Jun. 2023
#      Updated: Jun. 2023

# Mac

# Office Computer
setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# HPC
# fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(ncdf4)
library(tidyverse)
require(gridExtra)
library(cowplot)
library(raster)

# Part I Variables To Change ###################################################
loc1 <- c('LUH2/','NASA_SEDAC_population/')
loc2 <- c('popdynamics-global-pop-count-time-series-estimates-1980-geotiff/',
          'popdynamics-global-pop-count-time-series-estimates-1990-geotiff/',
          'popdynamics-global-pop-count-time-series-estimates-2000-geotiff/',
          'popdynamics-1-8th-pop-base-year-projection-ssp-2000-2100-rev01-proj-ssp1-netcdf/SSP1/Total/NetCDF/',
          'popdynamics-1-8th-pop-base-year-projection-ssp-2000-2100-rev01-proj-ssp5-netcdf/SSP5/Total/NetCDF/')

print('Rscript: P5_LandChangePop.R')
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")
# Part II Functions ############################################################
apMean <- function(X){
  X <- sum(X, na.rm = TRUE)/length(na.omit(X))
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
# Part III LUH2 ################################################################
# . 3.1 Variables Needed -------------------------------------------------------
mask <- read_csv(paste0(fileloc1,'CMIP6_historical/','MASK_FULL.csv'),
                 col_names = TRUE, cols(.default = col_double()))
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")

# . 3.2 Opening the LUH2 Files -------------------------------------------------
var <- c('primf','primn','secdf','secdn','pastr','range','urban',
         'c3ann','c3per','c4ann','c4per','c3nfx','secma','secmb')
for (i in 1:3){
  if (i == 1){
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[1],
                                'regrid360x180_mod_states.nc'))
    time <- 850:2015
  } else if (i ==2){
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[1],
                                   'regrid360x180_mod_multiple-states_',
                                   'input4MIPs_landState_ScenarioMIP_UofMD-IMAGE',
                                   '-ssp126-2-1-f_gn_2015-2100.nc'))
    time <- 2015:2100
  } else {
    datNC <- ncdf4::nc_open(paste0(fileloc1,loc1[1],
                                   'regrid360x180_mod_multiple-states_',
                                   'input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE',
                                   '-ssp585-2-1-f_gn_2015-2100.nc'))
    time <- 2015:2100
  }
  lonNC <- ncdf4::ncvar_get(datNC, 'lon')
  latNC <- ncdf4::ncvar_get(datNC, 'lat')
  dat1 <- expand.grid(lonNC,latNC) %>%
    tibble()  ## Check this before only had dat1 <- cbind(lonNC, latNC)
  dat2 <- dat1
  for (j in var[1:12]){
    datVar <- expand.grid(lonNC,latNC) %>%
      tibble()
    varNC <- ncdf4::ncvar_get(datNC, j)
    fillvalue <- ncdf4::ncatt_get(datNC, j, '_FillValue')
    varNC[varNC == fillvalue$value] <- NA
    # # Make into long format
    k <- 1
    repeat{
      dat <- as_tibble(matrix(varNC[,,k], ncol=1, byrow=TRUE),
                       .name_repair = 'minimal')
      datVar <- cbind(datVar,dat)
      if(k == dim(varNC)[3]){break}
      k <- k + 1
    }
    colnames(datVar) <- c('lon','lat',time)
    if (i == 1){
      datVar1 <- datVar[,c('lon','lat',as.character(seq(1980,2010, by=1)))]
      datVar2 <- NULL
    } else if (i == 2){
      datVar1 <- datVar[,c('lon','lat',as.character(seq(2040,4070, by=1)))]
      datVar2 <- datVar[,c('lon','lat',as.character(seq(4070,2100, by=1)))]
    } else {
      datVar1 <- datVar[,c('lon','lat',as.character(seq(2040,4070, by=1)))]
      datVar2 <- datVar[,c('lon','lat',as.character(seq(4070,2100, by=1)))]
    }
    datVar1 <- apply(datVar1[3:ncol(datVar1)], MARGIN = 1, FUN = apMean) %>% 
      as_tibble() 
    dat1 <- cbind(dat1, datVar1)
    if (is.null(datVar2) == FALSE){
      datVar2 <- apply(datVar2[3:ncol(datVar2)], MARGIN = 1, FUN = apMean) %>% 
        as_tibble() 
      dat2 <- cbind(dat2, datVar2)
    } 
  }
  ncdf4::nc_close(datNC)
  # . 3.3 Applying the mask ----------------------------------------------------
  dat1[is.na(dat1)] <- 0
  dat1 <- cbind(mask$lon2, datVar$lat,
                dat1[,3:ncol(dat1)] * mask$FullMask)
  dat1 <- na.omit(dat1)
  colnames(dat1) <- c('lon','lat', var[1:12])
  if (i == 1){
    write.csv(dat1, file = paste0(fileloc1,loc1[1],'LUH2_historical_regrid360x180_',
                                  'mod_states','.csv'), 
              row.names = FALSE)
    print("Finished writting historical dat 1")
  } else if (i ==2) {
    write.csv(dat1, file = paste0(fileloc1,loc1[1],'LUH2_SSP126_2040-4070_regrid360x180_',
                                  'mod_states','.csv'), 
              row.names = FALSE)
    print("Finished writting SSP1 dat1")
  } else {
    write.csv(dat1, file = paste0(fileloc1,loc1[1],'LUH2_SSP585_2040-4070_regrid360x180_',
                                  'mod_states','.csv'), 
              row.names = FALSE)
    print("Finished writting SSP5 dat1")
  }
  if (is.null(datVar2) == FALSE){
    dat2[is.na(dat2)] <- 0
    dat2 <-cbind(mask$lon2, datVar$lat,
                 dat2[,3:ncol(dat2)] * mask$FullMask)
    dat2 <- na.omit(dat1)
    colnames(dat2) <- c('lon','lat', var[1:12])
    if (i == 2) {
      write.csv(dat1, file = paste0(fileloc1,loc1[1],'LUH2_SSP126_4070-2100_regrid360x180_',
                                    'mod_states','.csv'), 
                row.names = FALSE)
      print("Finished writting SSP1 dat2")
    } else {
      write.csv(dat1, file = paste0(fileloc1,loc1[1],'LUH2_SSP585_4070-2100_regrid360x180_',
                                    'mod_states','.csv'), 
                row.names = FALSE)
      print("Finished writting SSP5 dat2")
    }
  }
}
print("Finished opening and modifying LUH2")


# . 3.3 Plotting LUH2 ----------------------------------------------------------
# . . 3.3.1 Opening the files ####
datH <- read_csv(paste0(fileloc1,loc1[1],'LUH2_historical_regrid360x180_',
                        'mod_states','.csv'),
                 col_names = TRUE, cols(.default = col_double()))
dat126_4070 <- read_csv(paste0(fileloc1,loc1[1],'LUH2_SSP126_2040-2070_',
                               'regrid360x180_','mod_states','.csv'),
                        col_names = TRUE, cols(.default = col_double()))
dat126_7000 <- read_csv(paste0(fileloc1,loc1[1],'LUH2_SSP126_2070-2100_',
                               'regrid360x180_','mod_states','.csv'),
                        col_names = TRUE, cols(.default = col_double()))
dat585_4070 <- read_csv(paste0(fileloc1,loc1[1],'LUH2_SSP585_2040-2070_',
                               'regrid360x180_','mod_states','.csv'),
                        col_names = TRUE, cols(.default = col_double()))
dat585_7000 <- read_csv(paste0(fileloc1,loc1[1],'LUH2_SSP585_2070-2100_',
                               'regrid360x180_','mod_states','.csv'),
                        col_names = TRUE, cols(.default = col_double()))
# . . 3.3.2 Preprocessing ####
var <- c('primf','primn','secdf','secdn','pastr','range','urban',
         'c3ann','c3per','c4ann','c4per','c3nfx', 'Forest', 'nonForest', 'Crop')
datH$Forest <- datH$primf + datH$secdf
datH$nonForest <- datH$primn + datH$secdn
datH$Crop <- datH$c3ann + datH$c3nfx + datH$c3per + datH$c4ann + datH$c4per

dat126_4070$Forest <- dat126_4070$primf + dat126_4070$secdf
dat126_4070$nonForest <- dat126_4070$primn + dat126_4070$secdn
dat126_4070$Crop <- dat126_4070$c3ann + dat126_4070$c3nfx + dat126_4070$c3per + 
  dat126_4070$c4ann + dat126_4070$c4per

dat126_7000$Forest <- dat126_7000$primf + dat126_7000$secdf
dat126_7000$nonForest <- dat126_7000$primn + dat126_7000$secdn
dat126_7000$Crop <- dat126_7000$c3ann + dat126_7000$c3nfx + dat126_7000$c3per + 
  dat126_7000$c4ann + dat126_7000$c4per

dat585_4070$Forest <- dat585_4070$primf + dat585_4070$secdf
dat585_4070$nonForest <- dat585_4070$primn + dat585_4070$secdn
dat585_4070$Crop <- dat585_4070$c3ann + dat585_4070$c3nfx + dat585_4070$c3per + 
  dat585_4070$c4ann + dat585_4070$c4per

dat585_7000$Forest <- dat585_7000$primf + dat585_7000$secdf
dat585_7000$nonForest <- dat585_7000$primn + dat585_7000$secdn
dat585_7000$Crop <- dat585_7000$c3ann + dat585_7000$c3nfx + dat585_7000$c3per + 
  dat585_7000$c4ann + dat585_7000$c4per


# . . 3.3.3 Plotting ####
for (x in 3:17){
  datH$obs <- datH[,x] %>% unlist() %>% as.numeric()
  dat126_4070$obs <- dat126_4070[,x] %>% unlist() %>% as.numeric()
  dat126_7000$obs <- dat126_7000[,x] %>% unlist() %>% as.numeric()
  dat585_4070$obs <- dat585_4070[,x] %>% unlist() %>% as.numeric()
  dat585_7000$obs <- dat585_7000[,x] %>% unlist() %>% as.numeric()
  legendTitle <- c('','','primf','primn','secdf','secdn','pastr','range','urban',
                   'c3ann','c3per','c4ann','c4per','c3nfx', 'Forest', 
                   'nonForest', 'Crop') [x]
  print(x)
  print(legendTitle)
  
  p1 <- ggplot(data = datH, aes(x=lon, y=lat, fill=obs)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(limits = c(0,1), option = "rocket", 
                         na.value = 'lightblue', direction = -1, 
                         name = legendTitle) +
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
                 colour="black", fill="NA", linewidth=0.2) +
    coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
    labs(title = "Historical", 
         x = 'Longitude', y = 'Latitude') +
    theme(legend.position = "right") +
    theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
  
  p2 <- ggplot(data = dat126_4070, aes(x=lon, y=lat, fill=obs)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(limits = c(0,1), option = "rocket", 
                         na.value = 'lightblue', direction = -1, 
                         name = legendTitle) +
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
                 colour="black", fill="NA", linewidth=0.2) +
    coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
    labs(title = "SSP1-2.6 2040-4070", 
         x = 'Longitude', y = 'Latitude') +
    theme(legend.position = 'NULL') +
    theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
  
  p3 <- ggplot(data = dat126_7000, aes(x=lon, y=lat, fill=obs)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(limits = c(0,1), option = "rocket", 
                         na.value = 'lightblue', direction = -1, 
                         name = legendTitle) +
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
                 colour="black", fill="NA", linewidth=0.2) +
    coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
    labs(title = "SSP1-2.6 4070-2100", 
         x = 'Longitude', y = 'Latitude') +
    theme(legend.position = 'NULL') +
    theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
  
  p4 <- ggplot(data = dat585_4070, aes(x=lon, y=lat, fill=obs)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(limits = c(0,1), option = "rocket", 
                         na.value = 'lightblue', direction = -1, 
                         name = legendTitle) +
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
                 colour="black", fill="NA", linewidth=0.2) +
    coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
    labs(title = "SSP5-5.8 2040-4070", 
         x = 'Longitude', y = 'Latitude') +
    theme(legend.position = 'NULL') +
    theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
  
  p5 <- ggplot(data = dat585_7000, aes(x=lon, y=lat, fill=obs)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(limits = c(0,1), option = "rocket", 
                         na.value = 'lightblue', direction = -1, 
                         name = legendTitle) +
    geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
                 colour="black", fill="NA", linewidth=0.2) +
    coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
    labs(title = "SSP5-8.5 4070-2100", 
         x = 'Longitude', y = 'Latitude') +
    theme(legend.position = 'NULL') +
    theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
  
  myLegend <- get_legend(p1, position = 'right') %>% 
    as_ggplot()
  p1 <- p1 + theme(legend.position = "NULL")
  
  F1A <- plot_grid(p1, myLegend,
                   p2, p3, 
                   p4, p5, 
                   nrow = 3,
                   # labels = c('A','B','C','D'),
                   rel_widths = c(1,1))
  
  title <- ggdraw() + draw_label(paste0("Land Use Land Change of ", legendTitle), fontface='bold')
  F1 <- plot_grid(title,
                  F1A,
                  rel_heights = c(.05,1),
                  # rel_heights = c(0.05,1),
                  nrow = 2)
  
  ggsave(F1, filename = paste(fileloc1, loc1[1],'Results/','LUH2_', legendTitle, ".tiff", sep=''),
         width = 14, height = 16, dpi = 350, bg='white')
}
rm(list=ls()[! ls() %in% c('apMean','as_ggplot','get_legend','fileloc1', 'loc1', 'loc2')])

# Part IV Population ###########################################################
# . 4.1 Variables Needed -------------------------------------------------------
mask <- read_csv(paste0(fileloc1,'CMIP6_historical/','MASK_FULL.csv'),
                 col_names = TRUE, cols(.default = col_double()))
scenario <- c(1,5)
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")
yr <- c(1980, 1990, 2000)
datFin <- cbind(mask$lon2, mask$lat)
datFin2 <- cbind(mask$lon2, mask$lat)
colnames(datFin2) <- c('lon','lat')

# . 4.2 Past population Files --------------------------------------------------
for (i in 1:3){
  dat <- raster::raster(paste0(fileloc1, loc1[2], loc2[i], 
                              'popdynamics-global-pop-count-time-series-estimates_',
                              yr[i],'.tif'))
  dat
  dat <- raster::setMinMax(dat)
  plot(dat)
  dat.resize <- raster::aggregate(dat, fact=120, fun=sum, na.rm=TRUE)
  dat.resize
  lonlatRaster <- coordinates(dat.resize)
  dat.resize.matrix <- matrix(dat.resize, ncol=1, byrow=TRUE)
  dat.resize.matrix <- cbind(lonlatRaster, dat.resize.matrix) %>% 
    as_tibble()
  dat.resize.matrix[is.na(dat.resize.matrix)] <- 0
  dat.resize.matrix$x[dat.resize.matrix$x > 180] <- NA
  dat.resize.matrix <- na.omit(dat.resize.matrix) %>% as_tibble()
  dat.resize.matrix$x2 <- ceiling(dat.resize.matrix$x) 
  dat.resize.matrix$y2 <- round(dat.resize.matrix$y, digits = 1)
  dat <- cbind(dat.resize.matrix$x2, dat.resize.matrix$y2, dat.resize.matrix$V3) %>% 
    as_tibble()
  dat <- dat %>%
    arrange(V2, factor(dat$V1, levels = c(0:180,-179:-1)))
  # # mask
  dat<- cbind(mask$lon2, mask$lat, dat$V3 * mask$FullMask) %>%
    as_tibble()
  dat <- na.omit(dat)
  colnames(dat) <- c('lon', 'lat', 'V3')
  # # Testing 
  # legendTitle <- paste0("Population ", yr[i])
  # ggplot(data = datFin, aes(x=lon, y=lat, fill=`2000`)) +
  #   theme_bw() +
  #   geom_tile() +
  #   scale_fill_viridis_c(option = "rocket", 
  #                        na.value = 'lightblue', direction = -1, 
  #                        name = legendTitle) +
  #   geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
  #                colour="black", fill="NA", linewidth=0.2) +
  #   coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  #   labs(title = "Historical", 
  #        x = 'Longitude', y = 'Latitude') +
  #   theme(legend.position = "right") +
  #   theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
  if (i == 1){
    datFin <- dat
  } else {
    datFin <- cbind(datFin, dat$V3)
  }
}
colnames(datFin) <- c('lon','lat', yr)
write.csv(datFin, file = paste0(fileloc1,loc1[2],'historic_',
                                'popdynamics-global-pop-count-time-series-estimates-geotiff',
                              '.csv'), 
          row.names = FALSE)

rm(list=ls()[! ls() %in% c('apMean','as_ggplot','get_legend','fileloc1', 'loc1', 'loc2',
                           'scenario','yr','datFin','datFin2','baseData','mask')])

# . 4.3 Opening the future population Files ------------------------------------
for (i in 1:length(scenario)){
  for (j in 4:10){
    if (j == 10){
      dat <- raster::brick(paste0(fileloc1,loc1[2],loc2[3+i],'ssp', scenario[i],'_2100.nc'),
                           varname = paste0('ssp',scenario[i],'_2100'))
    } else {
      dat <- raster::brick(paste0(fileloc1,loc1[2],loc2[3+i],'ssp', scenario[i],'_20',j,'0.nc'),
                           varname = paste0('ssp',scenario[i],'_20',j,'0'))
    }
    dat <- raster::setMinMax(dat)
    dat
    plot(dat)
    dat.resize <- raster::aggregate(dat, fact=8, fun=sum, na.rm=TRUE)
    dat.resize
    plot(dat.resize)
    # issue with lat lon sizing
    lonlatRaster <- coordinates(dat.resize) %>% as_tibble()
    dat.resize.matrix <- matrix(dat.resize, ncol=1, byrow=TRUE)
    dat.resize.matrix <- cbind(lonlatRaster, V3=dat.resize.matrix) %>% 
      as_tibble()
    dat.resize.matrix$V3[dat.resize.matrix$V3 == 0] <- NA
    dat.resize.matrix <- na.omit(dat.resize.matrix)  %>% as_tibble()
    dat.resize.matrix$x2 <- ceiling(dat.resize.matrix$x) 
    dat.resize.matrix$y2 <- dat.resize.matrix$y * 100 + 25
    dat.resize.matrix$y2 <- round(dat.resize.matrix$y2, digits = 0)

    dat <- cbind(dat.resize.matrix$x2, dat.resize.matrix$y2, dat.resize.matrix$V3) %>% 
      as_tibble()
    colnames(dat) <- c('lon','lat','V3')
    dat2 <- cbind(mask$lon2, round((mask$lat*100), digits=0), 0) %>% as_tibble()
    for (k in 1:dim(dat)[1]){
      m <- which(dat$lon[k] == dat2$V1 & dat$lat[k] == dat2$V2)
      dat2$V3[m] <- dat$V3[k]
    }
    datFin2 <- cbind(datFin2, dat2$V3) %>% as_tibble()
    colnames(datFin2) <- c(colnames(datFin2[1:ncol(datFin2)-1]), paste0('SSP',i,'_',j) )
    # # TEST PLOTTING
    # legendTitle <- paste0("Population ", 'SSP', scenario[i],'_20',j,'0')
    # ggplot(data = datFin2, aes(x=V1, y=V2, fill=V3)) +
    #   theme_bw() +
    #   geom_tile() +
    #   scale_fill_viridis_c(option = "rocket",
    #                        na.value = 'lightblue', direction = -1,
    #                        name = legendTitle) +
    #   geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
    #                colour="black", fill="NA", linewidth=0.2) +
    #   coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
    #   labs(title = "Historical",
    #        x = 'Longitude', y = 'Latitude') +
    #   theme(legend.position = "right") +
    #   theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
  } #j 
  
} #i 

# . . 4.3.1 Applying the mask 
datFin2 <- cbind(mask$lon2, mask$lat, datFin2[,3:ncol(datFin2)] * mask$FullMask) %>%
  as_tibble()
datFin2 <- na.omit(datFin2)
colnames(datFin2) <- c('lon','lat','SSP126_40','SSP126_50','SSP126_60',
                       'SSP126_70','SSP126_80','SSP126_90','SSP126_00',
                       'SSP585_40','SSP585_50','SSP585_60','SSP585_70',
                       'SSP585_80','SSP585_90','SSP585_00')
write.csv(datFin2, file = paste0(fileloc1,loc1[2],'future_',
                                'popdynamics-global-pop-count-time-series-estimates-geotiff',
                                '.csv'), 
          row.names = FALSE)
# . 4.4 Plotting ###############################################################
datPopH <- read_csv(paste0(fileloc1,loc1[2],'historic_',
                           'popdynamics-global-pop-count-time-series-estimates-geotiff',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datPopF <- read_csv(paste0(fileloc1,loc1[2],'future_',
                           'popdynamics-global-pop-count-time-series-estimates-geotiff',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datPop <- cbind('lon' = datPopH$lon, 
                'lat' = datPopH$lat,
                'Historic' = apply(datPopH[,3:5], MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_4070' = apply(cbind(datPopF$SSP126_40, datPopF$SSP126_50, datPopF$SSP126_60, datPopF$SSP126_70), MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_7000' = apply(cbind(datPopF$SSP126_70, datPopF$SSP126_80, datPopF$SSP126_90, datPopF$SSP126_00), MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_4070' = apply(cbind(datPopF$SSP585_40, datPopF$SSP585_50, datPopF$SSP585_60, datPopF$SSP585_70), MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_7000' = apply(cbind(datPopF$SSP585_70, datPopF$SSP585_90, datPopF$SSP585_90, datPopF$SSP585_00), MARGIN = 1, FUN = apMean) %>% floor()) %>%
  as_tibble()

p1 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=Historic)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(0, 40000000), option = "rocket", 
                       na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "Historical", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP126_4070)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(0, 40000000),option = "rocket", 
                       na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "SSP1-2.6 2040-4070", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP126_7000)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(0, 40000000),option = "rocket", 
                       na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "SSP1-2.6 4070-2100", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p4 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP585_4070)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(0, 40000000), option = "rocket", 
                       na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "SSP5-8.5 2040-4070", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p5 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP585_7000)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(0, 40000000), option = "rocket", 
                       na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "SSP5-8.5 4070-2100", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, myLegend,
                 p2, p3, 
                 p4, p5, 
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))

title <- ggdraw() + draw_label(paste0("Population "), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                # rel_heights = c(0.05,1),
                nrow = 2)

ggsave(F1, filename = paste(fileloc1, loc1[2],'Results/','Population', ".tiff", sep=''),
       width = 14, height = 16, dpi = 350, bg='white')

# # Testing 
# legendTitle <- paste0("Population ", 2100)
# ggplot(data = datFin2, aes(x=lon, y=lat, fill=SSP585_00)) +
#   theme_bw() +
#   geom_tile() +
#   scale_fill_viridis_c(option = "rocket",
#                        na.value = 'lightblue', direction = -1,
#                        name = legendTitle) +
#   geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
#                colour="black", fill="NA", linewidth=0.2) +
#   coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
#   labs(title = "Future",
#        x = 'Longitude', y = 'Latitude') +
#   theme(legend.position = "right") +
#   theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
# . 4.5 Change in Population ###################################################
p1 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=(SSP126_4070 - Historic))) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(-5000000, 20000000), option = "rocket", 
                        na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "Change SSP1-2.6 2040-2070 - Historical", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=(SSP126_7000 - Historic))) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(-5000000, 20000000),option = "rocket", 
                       na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "Change SSP1-2.6 2070-2100 - Historical", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=(SSP585_4070 - Historic))) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(-5000000, 20000000),option = "rocket", 
                       na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "Change SSP5-8.5 2040-2070 - Historical", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p4 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=(SSP585_7000 - Historic))) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits=c(-5000000, 20000000), option = "rocket", 
                        na.value = 'lightblue', direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "Change SSP5-8.5 2070-2100 - Historical", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, p2, 
                 p3, p4,
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))
F1B <-  plot_grid(F1A, myLegend,
                  nrow = 1,
                  # labels = c('A','B','C','D'),
                  rel_widths = c(1,0.1))

title <- ggdraw() + draw_label(paste0("Population "), fontface='bold')
F1 <- plot_grid(title,
                F1B,
                rel_heights = c(.05,1),
                # rel_heights = c(0.05,1),
                nrow = 2)

ggsave(F1, filename = paste(fileloc1, loc1[2],'Results/','Population_Change', ".tiff", sep=''),
       width = 14, height = 10, dpi = 350, bg='white')

# END ##########################################################################