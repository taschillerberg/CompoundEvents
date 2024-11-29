# P7_Exposure.R
# About: This program will change the land use and population data into a 
#        usable format to determine exposure.
# 
# Inputs: LUH2, popdynamics
# Outputs: 
#
# T. A. Schillerberg
#               Jun. 2023
#      Updated: Jan. 2024

# Computer
setwd("Source File Location") 
fileloc1 <- 'Main project folder' 

# HPC
# fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)
require(gridExtra)
library(cowplot)

# Part I Variables To Change ###################################################
loc1 <- c('NASA_SEDAC_population/','LUH2/',
          'CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')

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
compNum <- 8
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')[compNum]
compTitle <- c('Simultanious Heat & Drought','Sequential Heat & Drought',
               'Sequential Drought & Heat','Simultanious Heat & Extreme Precip',
               'Sequential Heat & Extreme Precip','Sequential Precipitation & Heat',
               'Sequential Extreme Precip & Drought',
               'Sequential Drought & Extreme Precip')[compNum]

baseData <- map_data('world')
print('Rscript: P7_Exposure.R')
print(paste0('Compound Event: ', comp))
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

# Part III Population Exposure #################################################
# . 3.1 Variables Needed -------------------------------------------------------
# . 3.2 Opening the Files ------------------------------------------------------
# . . 3.2.1 Population ####
datPopH <- read_csv(paste0(fileloc1,loc1[1],'historic_',
                           'popdynamics-global-pop-count-time-series-estimates-geotiff',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datPopF <- read_csv(paste0(fileloc1,loc1[1],'future_',
                           'popdynamics-global-pop-count-time-series-estimates-geotiff',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datPop <- cbind('lon' = datPopH$lon, 
                'lat' = datPopH$lat,
                'Historic' = apply(datPopH[,3:5], MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_1040' = apply(cbind(datPopF$SSP126_10, datPopF$SSP126_20, 
                                            datPopF$SSP126_30, datPopF$SSP126_40), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_4070' = apply(cbind(datPopF$SSP126_40, datPopF$SSP126_50, 
                                            datPopF$SSP126_60, datPopF$SSP126_70), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_7000' = apply(cbind(datPopF$SSP126_70, datPopF$SSP126_80, 
                                            datPopF$SSP126_90, datPopF$SSP126_00), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_1040' = apply(cbind(datPopF$SSP585_10, datPopF$SSP585_20, 
                                            datPopF$SSP585_30, datPopF$SSP585_40), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_4070' = apply(cbind(datPopF$SSP585_40, datPopF$SSP585_50, 
                                            datPopF$SSP585_60, datPopF$SSP585_70), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_7000' = apply(cbind(datPopF$SSP585_70, datPopF$SSP585_90, 
                                            datPopF$SSP585_90, datPopF$SSP585_00), 
                                      MARGIN = 1, FUN = apMean) %>% floor()) %>%
  as_tibble()

# . . 3.2.2 Compound Event ####
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . 3.3 Calculating Exposure ---------------------------------------------------
exposurePopulation <- tibble(
  'lon' = datComp$lon,
  'lat' = datComp$lat,
  'Historic_mu' =  numeric(length = 12476),
  'SSP126_1040_mu' = numeric(length = 12476),
  'SSP126_1040_delta' = numeric(length = 12476),
  'SSP126_4070_mu' = numeric(length = 12476),
  'SSP126_4070_delta' = numeric(length = 12476),
  'SSP126_7000_mu' = numeric(length = 12476),
  'SSP126_7000_delta' = numeric(length = 12476),
  'SSP585_1040_mu' = numeric(length = 12476),
  'SSP585_1040_delta' = numeric(length = 12476),
  'SSP585_4070_mu' = numeric(length = 12476),
  'SSP585_4070_delta' = numeric(length = 12476),
  'SSP585_7000_mu' = numeric(length = 12476),
  'SSP585_7000_delta' = numeric(length = 12476)
)

# . . 3.3.1 Calculating ###
exposurePopulation$Historic_mu <- datPop$Historic * datComp$Historical_mu
exposurePopulation$SSP126_1040_mu <- datPop$SSP126_1040 * datComp$SSP126_1040_mu
exposurePopulation$SSP126_4070_mu <- datPop$SSP126_4070 * datComp$SSP126_4070_mu
exposurePopulation$SSP126_7000_mu <- datPop$SSP126_7000 * datComp$SSP126_7000_mu
exposurePopulation$SSP585_1040_mu <- datPop$SSP585_1040 * datComp$SSP585_1040_mu
exposurePopulation$SSP585_4070_mu <- datPop$SSP585_4070 * datComp$SSP585_4070_mu
exposurePopulation$SSP585_7000_mu <- datPop$SSP585_7000 * datComp$SSP585_7000_mu

exposurePopulation$SSP126_1040_delta <- 
  exposurePopulation$SSP126_1040_mu - exposurePopulation$Historic_mu
exposurePopulation$SSP126_4070_delta <- 
  exposurePopulation$SSP126_4070_mu - exposurePopulation$Historic_mu
exposurePopulation$SSP126_7000_delta <- 
  exposurePopulation$SSP126_7000_mu - exposurePopulation$Historic_mu
exposurePopulation$SSP585_1040_delta <- 
  exposurePopulation$SSP585_1040_mu - exposurePopulation$Historic_mu
exposurePopulation$SSP585_4070_delta <- 
  exposurePopulation$SSP585_4070_mu - exposurePopulation$Historic_mu
exposurePopulation$SSP585_7000_delta <- 
  exposurePopulation$SSP585_7000_mu - exposurePopulation$Historic_mu

# . 3.4 Creating a table of values ---------------------------------------------
sEH <- summary(exposurePopulation$Historic_mu)
sEE126 <- summary(exposurePopulation$SSP126_1040_mu)
sEM126 <- summary(exposurePopulation$SSP126_4070_mu)
sEL126 <- summary(exposurePopulation$SSP126_7000_mu)
sEE585 <- summary(exposurePopulation$SSP585_1040_mu)
sEM585 <- summary(exposurePopulation$SSP585_4070_mu)
sEL585 <- summary(exposurePopulation$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sFmin <- min(sE$'Freq Min.')
sFmax <- max(sE$'Freq Max.')

# . 3.5 Plotting ---------------------------------------------------------------
a <- 'Historical'
p1 <- ggplot(data = exposurePopulation, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Compound Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposurePopulation, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposurePopulation, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposurePopulation, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposurePopulation, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposurePopulation, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposurePopulation, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Population Exposure to ", compTitle), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)

ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_POP_', comp, ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')
write.csv(exposurePopulation,file=paste0(fileloc1,'Results/','EXPOSURE_POP_',comp,'.csv'),
          row.names = FALSE)
# . 3.6 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('apMean','as_ggplot','get_legend','fileloc1', 
                           'loc1', 'loc2', 'mFileH', 'mFile126', 'mFile585', 
                           'compNum', 'comp', 'compTitle', 'baseData')])

# Part IV Land Use Exposure ####################################################
# . 4.1 Variables Needed -------------------------------------------------------
# . 4.2 Opening the Files ------------------------------------------------------
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
# . . 4.2.1 Land Use -----------------------------------------------------------
datLUHH <- read_csv(paste0(fileloc1,loc1[2],'LUH2_historical_regrid360x180_mod_states',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datLUH126_1040 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP126_2010-2040_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH126_4070 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP126_2040-2070_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH126_7000 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP126_2070-2100_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH585_1040 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP585_2010-2040_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH585_4070 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP585_2040-2070_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH585_7000 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP585_2070-2100_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))

datLUH_AG <- cbind('lon' = datLUHH$lon, 
                   'lat' = datLUHH$lat,
                   'Historic' = apply(datLUHH[,10:14], MARGIN = 1, FUN = sum) ,
                   'SSP126_1040' = apply(datLUH126_1040[,10:14], MARGIN = 1, FUN = sum),
                   'SSP126_4070' = apply(datLUH126_4070[,10:14], MARGIN = 1, FUN = sum),
                   'SSP126_7000' = apply(datLUH126_7000[,10:14], MARGIN = 1, FUN = sum),
                   'SSP585_1040' = apply(datLUH585_1040[,10:14], MARGIN = 1, FUN = sum) ,
                   'SSP585_4070' = apply(datLUH585_4070[,10:14], MARGIN = 1, FUN = sum),
                   'SSP585_7000' = apply(datLUH585_7000[,10:14], MARGIN = 1, FUN = sum)) %>% 
  as_tibble()
datLUH_FOR <- cbind('lon' = datLUHH$lon, 
                    'lat' = datLUHH$lat,
                    'Historic' = apply(datLUHH[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP126_1040' = apply(datLUH126_1040[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP126_4070' = apply(datLUH126_4070[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP126_7000' = apply(datLUH126_7000[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP585_1040' = apply(datLUH585_1040[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP585_4070' = apply(datLUH585_4070[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP585_7000' = apply(datLUH585_7000[,c(3,5)], MARGIN = 1, FUN = sum)) %>% 
  as_tibble()

# Convert sum total to acres
datLUH_AG$Historic <- datLUH_AG$Historic * datLUHH$LandAreakm2
datLUH_AG$SSP126_1040 <- datLUH_AG$SSP126_1040 * datLUH126_1040$LandAreakm2
datLUH_AG$SSP126_4070 <- datLUH_AG$SSP126_4070 * datLUH126_4070$LandAreakm2
datLUH_AG$SSP126_7000 <- datLUH_AG$SSP126_7000 * datLUH126_7000$LandAreakm2
datLUH_AG$SSP585_1040 <- datLUH_AG$SSP585_1040 * datLUH585_1040$LandAreakm2
datLUH_AG$SSP585_4070 <- datLUH_AG$SSP585_4070 * datLUH585_4070$LandAreakm2
datLUH_AG$SSP585_7000 <- datLUH_AG$SSP585_7000 * datLUH585_7000$LandAreakm2

datLUH_FOR$Historic <- datLUH_FOR$Historic * datLUHH$LandAreakm2
datLUH_FOR$SSP126_1040 <- datLUH_FOR$SSP126_1040 * datLUH126_1040$LandAreakm2
datLUH_FOR$SSP126_4070 <- datLUH_FOR$SSP126_4070 * datLUH126_4070$LandAreakm2
datLUH_FOR$SSP126_7000 <- datLUH_FOR$SSP126_7000 * datLUH126_7000$LandAreakm2
datLUH_FOR$SSP585_1040 <- datLUH_FOR$SSP585_1040 * datLUH585_1040$LandAreakm2
datLUH_FOR$SSP585_4070 <- datLUH_FOR$SSP585_4070 * datLUH585_4070$LandAreakm2
datLUH_FOR$SSP585_7000 <- datLUH_FOR$SSP585_7000 * datLUH585_7000$LandAreakm2

# . 4.3 Calculating Exposure ---------------------------------------------------
# . . 4.3.1 Agriculture --------------------------------------------------------
exposureAg <- tibble(
  'lon' = datComp$lon,
  'lat' = datComp$lat,
  'Historic_mu' =  numeric(length = 12476),
  'SSP126_1040_mu' = numeric(length = 12476),
  'SSP126_1040_delta' = numeric(length = 12476),
  'SSP126_4070_mu' = numeric(length = 12476),
  'SSP126_4070_delta' = numeric(length = 12476),
  'SSP126_7000_mu' = numeric(length = 12476),
  'SSP126_7000_delta' = numeric(length = 12476),
  'SSP585_1040_mu' = numeric(length = 12476),
  'SSP585_1040_delta' = numeric(length = 12476),
  'SSP585_4070_mu' = numeric(length = 12476),
  'SSP585_4070_delta' = numeric(length = 12476),
  'SSP585_7000_mu' = numeric(length = 12476),
  'SSP585_7000_delta' = numeric(length = 12476)
)

exposureAg$Historic_mu <- datLUH_AG$Historic * datComp$Historical_mu
exposureAg$SSP126_1040_mu <- datLUH_AG$SSP126_1040 * datComp$SSP126_1040_mu
exposureAg$SSP126_4070_mu <- datLUH_AG$SSP126_4070 * datComp$SSP126_4070_mu
exposureAg$SSP126_7000_mu <- datLUH_AG$SSP126_7000 * datComp$SSP126_7000_mu
exposureAg$SSP585_1040_mu <- datLUH_AG$SSP585_1040 * datComp$SSP585_1040_mu
exposureAg$SSP585_4070_mu <- datLUH_AG$SSP585_4070 * datComp$SSP585_4070_mu
exposureAg$SSP585_7000_mu <- datLUH_AG$SSP585_7000 * datComp$SSP585_7000_mu

exposureAg$SSP126_1040_delta <- 
  exposureAg$SSP126_1040_mu - exposureAg$Historic_mu
exposureAg$SSP126_4070_delta <- 
  exposureAg$SSP126_4070_mu - exposureAg$Historic_mu
exposureAg$SSP126_7000_delta <- 
  exposureAg$SSP126_7000_mu - exposureAg$Historic_mu
exposureAg$SSP585_1040_delta <- 
  exposureAg$SSP585_1040_mu - exposureAg$Historic_mu
exposureAg$SSP585_4070_delta <- 
  exposureAg$SSP585_4070_mu - exposureAg$Historic_mu
exposureAg$SSP585_7000_delta <- 
  exposureAg$SSP585_7000_mu - exposureAg$Historic_mu

# . . 4.3.2 Forestry -----------------------------------------------------------
exposureFOR <- tibble(
  'lon' = datComp$lon,
  'lat' = datComp$lat,
  'Historic_mu' =  numeric(length = 12476),
  'SSP126_1040_mu' = numeric(length = 12476),
  'SSP126_1040_delta' = numeric(length = 12476),
  'SSP126_4070_mu' = numeric(length = 12476),
  'SSP126_4070_delta' = numeric(length = 12476),
  'SSP126_7000_mu' = numeric(length = 12476),
  'SSP126_7000_delta' = numeric(length = 12476),
  'SSP585_1040_mu' = numeric(length = 12476),
  'SSP585_1040_delta' = numeric(length = 12476),
  'SSP585_4070_mu' = numeric(length = 12476),
  'SSP585_4070_delta' = numeric(length = 12476),
  'SSP585_7000_mu' = numeric(length = 12476),
  'SSP585_7000_delta' = numeric(length = 12476)
)

exposureFOR$Historic_mu <- datLUH_FOR$Historic * datComp$Historical_mu
exposureFOR$SSP126_1040_mu <- datLUH_FOR$SSP126_1040 * datComp$SSP126_1040_mu
exposureFOR$SSP126_4070_mu <- datLUH_FOR$SSP126_4070 * datComp$SSP126_4070_mu
exposureFOR$SSP126_7000_mu <- datLUH_FOR$SSP126_7000 * datComp$SSP126_7000_mu
exposureFOR$SSP585_1040_mu <- datLUH_FOR$SSP585_1040 * datComp$SSP585_1040_mu
exposureFOR$SSP585_4070_mu <- datLUH_FOR$SSP585_4070 * datComp$SSP585_4070_mu
exposureFOR$SSP585_7000_mu <- datLUH_FOR$SSP585_7000 * datComp$SSP585_7000_mu

exposureFOR$SSP126_1040_delta <- 
  exposureFOR$SSP126_1040_mu - exposureFOR$Historic_mu
exposureFOR$SSP126_4070_delta <- 
  exposureFOR$SSP126_4070_mu - exposureFOR$Historic_mu
exposureFOR$SSP126_7000_delta <- 
  exposureFOR$SSP126_7000_mu - exposureFOR$Historic_mu
exposureFOR$SSP585_1040_delta <- 
  exposureFOR$SSP585_1040_mu - exposureFOR$Historic_mu
exposureFOR$SSP585_4070_delta <- 
  exposureFOR$SSP585_4070_mu - exposureFOR$Historic_mu
exposureFOR$SSP585_7000_delta <- 
  exposureFOR$SSP585_7000_mu - exposureFOR$Historic_mu

# . 4.4 Creating a table of values ---------------------------------------------
sEH <- summary(exposureAg$Historic_mu)
sEE126 <- summary(exposureAg$SSP126_1040_mu)
sEM126 <- summary(exposureAg$SSP126_4070_mu)
sEL126 <- summary(exposureAg$SSP126_7000_mu)
sEE585 <- summary(exposureAg$SSP585_1040_mu)
sEM585 <- summary(exposureAg$SSP585_4070_mu)
sEL585 <- summary(exposureAg$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sAmin <- min(sE$'Freq Min.')
sAmax <- max(sE$'Freq Max.')

sEH <- summary(exposureFOR$Historic_mu)
sEE126 <- summary(exposureFOR$SSP126_1040_mu)
sEM126 <- summary(exposureFOR$SSP126_4070_mu)
sEL126 <- summary(exposureFOR$SSP126_7000_mu)
sEE585 <- summary(exposureFOR$SSP585_1040_mu)
sEM585 <- summary(exposureFOR$SSP585_4070_mu)
sEL585 <- summary(exposureFOR$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sFmin <- min(sE$'Freq Min.')
sFmax <- max(sE$'Freq Max.')

# . 4.5 Plotting AG ------------------------------------------------------------
a <- 'Historical'
p1 <- ggplot(data = exposureAg, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAmin,sAmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Compound Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureAg, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAmin,sAmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureAg, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAmin,sAmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureAg, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAmin,sAmax), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureAg, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAmin,sAmax), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureAg, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAmin,sAmax), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureAg, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAmin,sAmax), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Agriculture (km2) Exposure to ", compTitle),
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)

ggsave(F1, filename = paste0(fileloc1,'Results/','EXPOSURE_AG_', comp, ".tiff"),
       width = 9.5, height = 6.5, dpi = 350, bg='white')
write.csv(exposureAg,file=paste0(fileloc1,'Results/','EXPOSURE_AG_',comp,'.csv'),
          row.names = FALSE)

# . 4.5 Plotting FOR -----------------------------------------------------------
a <- 'Historical'
p1 <- ggplot(data = exposureFOR, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Compound Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureFOR, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureFOR, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureFOR, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureFOR, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureFOR, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureFOR, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Forestry (km2) Exposure to ", compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)

ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_FOR_', comp, ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')
write.csv(exposureFOR,file=paste0(fileloc1,'Results/','EXPOSURE_FOR_',comp,'.csv'),
          row.names = FALSE)

# . 4.6 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('as_ggplot','get_legend','fileloc1', 'timeStep',
                           'loc1', 'loc2', 
                           'baseData')])
# Part V Individual Extreme Exposure ###########################################
# . 5.1 Variables Needed -------------------------------------------------------
var <- c('tasmax', 'tasmin','pr', 'mrsos')

# . 5.2 Opening Files ----------------------------------------------------------
# . . 5.2.1 Population ---------------------------------------------------------
datPopH <- read_csv(paste0(fileloc1,loc1[1],'historic_',
                           'popdynamics-global-pop-count-time-series-estimates-geotiff',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datPopF <- read_csv(paste0(fileloc1,loc1[1],'future_',
                           'popdynamics-global-pop-count-time-series-estimates-geotiff',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datPop <- cbind('lon' = datPopH$lon, 
                'lat' = datPopH$lat,
                'Historic' = apply(datPopH[,3:5], MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_1040' = apply(cbind(datPopF$SSP126_10, datPopF$SSP126_20, 
                                            datPopF$SSP126_30, datPopF$SSP126_40), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_4070' = apply(cbind(datPopF$SSP126_40, datPopF$SSP126_50, 
                                            datPopF$SSP126_60, datPopF$SSP126_70), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP126_7000' = apply(cbind(datPopF$SSP126_70, datPopF$SSP126_80, 
                                            datPopF$SSP126_90, datPopF$SSP126_00), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_1040' = apply(cbind(datPopF$SSP585_10, datPopF$SSP585_20, 
                                            datPopF$SSP585_30, datPopF$SSP585_40), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_4070' = apply(cbind(datPopF$SSP585_40, datPopF$SSP585_50, 
                                            datPopF$SSP585_60, datPopF$SSP585_70), 
                                      MARGIN = 1, FUN = apMean) %>% floor(),
                'SSP585_7000' = apply(cbind(datPopF$SSP585_70, datPopF$SSP585_90, 
                                            datPopF$SSP585_90, datPopF$SSP585_00), 
                                      MARGIN = 1, FUN = apMean) %>% floor()) %>%
  as_tibble()
# . . 5.2.2 Land Use Land Change -----------------------------------------------
datLUHH <- read_csv(paste0(fileloc1,loc1[2],'LUH2_historical_regrid360x180_mod_states',
                           '.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datLUH126_1040 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP126_2010-2040_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH126_4070 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP126_2040-2070_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH126_7000 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP126_2070-2100_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH585_1040 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP585_2010-2040_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH585_4070 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP585_2040-2070_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLUH585_7000 <- read_csv(paste0(fileloc1,loc1[2],
                                  'LUH2_SSP585_2070-2100_regrid360x180_mod_states',
                                  '.csv'),
                           col_names = TRUE, cols(.default = col_double()))

datLUH_AG <- cbind('lon' = datLUHH$lon, 
                   'lat' = datLUHH$lat,
                   'Historic' = apply(datLUHH[,10:14], MARGIN = 1, FUN = sum) ,
                   'SSP126_1040' = apply(datLUH126_1040[,10:14], MARGIN = 1, FUN = sum),
                   'SSP126_4070' = apply(datLUH126_4070[,10:14], MARGIN = 1, FUN = sum),
                   'SSP126_7000' = apply(datLUH126_7000[,10:14], MARGIN = 1, FUN = sum),
                   'SSP585_1040' = apply(datLUH585_1040[,10:14], MARGIN = 1, FUN = sum) ,
                   'SSP585_4070' = apply(datLUH585_4070[,10:14], MARGIN = 1, FUN = sum),
                   'SSP585_7000' = apply(datLUH585_7000[,10:14], MARGIN = 1, FUN = sum)) %>% 
  as_tibble()
datLUH_FOR <- cbind('lon' = datLUHH$lon, 
                    'lat' = datLUHH$lat,
                    'Historic' = apply(datLUHH[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP126_1040' = apply(datLUH126_1040[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP126_4070' = apply(datLUH126_4070[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP126_7000' = apply(datLUH126_7000[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP585_1040' = apply(datLUH585_1040[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP585_4070' = apply(datLUH585_4070[,c(3,5)], MARGIN = 1, FUN = sum),
                    'SSP585_7000' = apply(datLUH585_7000[,c(3,5)], MARGIN = 1, FUN = sum)) %>% 
  as_tibble()

# Convert sum total to acres
datLUH_AG$Historic <- datLUH_AG$Historic * datLUHH$LandAreakm2
datLUH_AG$SSP126_1040 <- datLUH_AG$SSP126_1040 * datLUH126_1040$LandAreakm2
datLUH_AG$SSP126_4070 <- datLUH_AG$SSP126_4070 * datLUH126_4070$LandAreakm2
datLUH_AG$SSP126_7000 <- datLUH_AG$SSP126_7000 * datLUH126_7000$LandAreakm2
datLUH_AG$SSP585_1040 <- datLUH_AG$SSP585_1040 * datLUH585_1040$LandAreakm2
datLUH_AG$SSP585_4070 <- datLUH_AG$SSP585_4070 * datLUH585_4070$LandAreakm2
datLUH_AG$SSP585_7000 <- datLUH_AG$SSP585_7000 * datLUH585_7000$LandAreakm2

datLUH_FOR$Historic <- datLUH_AG$Historic * datLUHH$LandAreakm2
datLUH_FOR$SSP126_1040 <- datLUH_FOR$SSP126_1040 * datLUH126_1040$LandAreakm2
datLUH_FOR$SSP126_4070 <- datLUH_FOR$SSP126_4070 * datLUH126_4070$LandAreakm2
datLUH_FOR$SSP126_7000 <- datLUH_FOR$SSP126_7000 * datLUH126_7000$LandAreakm2
datLUH_FOR$SSP585_1040 <- datLUH_FOR$SSP585_1040 * datLUH585_1040$LandAreakm2
datLUH_FOR$SSP585_4070 <- datLUH_FOR$SSP585_4070 * datLUH585_4070$LandAreakm2
datLUH_FOR$SSP585_7000 <- datLUH_FOR$SSP585_7000 * datLUH585_7000$LandAreakm2

# . . 5.2.3 Climate Extremes ---------------------------------------------------
datOccH_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/', 
                              'OCC_DAY_', var[1],'_Hist_8010', '.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datOcc126_1040_V1 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[1],'_SSP126_1040', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc126_4070_V1 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[1],'_SSP126_4070', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc126_7000_V1 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[1],'_SSP126_7000', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_1040_V1 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[1],'_SSP585_1040', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_4070_V1 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[1],'_SSP585_4070', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_7000_V1 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[1],'_SSP585_7000', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
# Var 3
datOccH_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                              'OCC_DAY_', var[3],'_Hist_8010', '.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datOcc126_1040_V3 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[3],'_SSP126_1040', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc126_4070_V3 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[3],'_SSP126_4070', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc126_7000_V3 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[3],'_SSP126_7000', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_1040_V3 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[3],'_SSP585_1040', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_4070_V3 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[3],'_SSP585_4070', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_7000_V3 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[3],'_SSP585_7000', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
# Var 4
datOccH_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                              'OCC_DAY_', var[4],'_Hist_8010', '.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datOcc126_1040_V4 <- read_csv(paste0(fileloc1, loc1[4], 'Results/', 
                                     'OCC_DAY_', var[4],'_SSP126_1040', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc126_4070_V4 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[4],'_SSP126_4070', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc126_7000_V4 <- read_csv(paste0(fileloc1, loc1[4], 'Results/',
                                     'OCC_DAY_', var[4],'_SSP126_7000', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_1040_V4 <- read_csv(paste0(fileloc1, loc1[5], 'Results/', 
                                     'OCC_DAY_', var[4],'_SSP585_1040', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_4070_V4 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[4],'_SSP585_4070', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))
datOcc585_7000_V4 <- read_csv(paste0(fileloc1, loc1[5], 'Results/',
                                     'OCC_DAY_', var[4],'_SSP585_7000', '.csv'),
                              col_names = TRUE, cols(.default = col_double()))

# . 5.3 Calculating Exposure ---------------------------------------------------
exposurePop_V1 <- tibble(
  'lon' = datPop$lon,
  'lat' = datPop$lat,
  'Historic_mu' =  numeric(length = 12476),
  'SSP126_1040_mu' = numeric(length = 12476),
  'SSP126_1040_delta' = numeric(length = 12476),
  'SSP126_4070_mu' = numeric(length = 12476),
  'SSP126_4070_delta' = numeric(length = 12476),
  'SSP126_7000_mu' = numeric(length = 12476),
  'SSP126_7000_delta' = numeric(length = 12476),
  'SSP585_1040_mu' = numeric(length = 12476),
  'SSP585_1040_delta' = numeric(length = 12476),
  'SSP585_4070_mu' = numeric(length = 12476),
  'SSP585_4070_delta' = numeric(length = 12476),
  'SSP585_7000_mu' = numeric(length = 12476),
  'SSP585_7000_delta' = numeric(length = 12476)
)
exposurePop_V3 <- exposurePop_V1
exposurePop_V4 <- exposurePop_V1
exposureAg_V1 <- exposurePop_V1
exposureAg_V3 <- exposurePop_V1
exposureAg_V4 <- exposurePop_V1
exposureFor_V1 <- exposurePop_V1
exposureFor_V3 <- exposurePop_V1
exposureFor_V4 <- exposurePop_V1

# . . 5.3.1 Population ---------------------------------------------------------
exposurePop_V1$Historic_mu <- datPop$Historic * datOccH_V1$Mu
exposurePop_V1$SSP126_1040_mu <- datPop$SSP126_1040 * datOcc126_1040_V1$Mu
exposurePop_V1$SSP126_4070_mu <- datPop$SSP126_4070 * datOcc126_4070_V1$Mu
exposurePop_V1$SSP126_7000_mu <- datPop$SSP126_7000 * datOcc126_7000_V1$Mu
exposurePop_V1$SSP585_1040_mu <- datPop$SSP585_1040 * datOcc585_1040_V1$Mu
exposurePop_V1$SSP585_4070_mu <- datPop$SSP585_4070 * datOcc585_4070_V1$Mu
exposurePop_V1$SSP585_7000_mu <- datPop$SSP585_7000 * datOcc585_7000_V1$Mu
exposurePop_V1$SSP126_1040_delta <- 
  exposurePop_V1$SSP126_1040_mu - exposurePop_V1$Historic_mu
exposurePop_V1$SSP126_4070_delta <- 
  exposurePop_V1$SSP126_4070_mu - exposurePop_V1$Historic_mu
exposurePop_V1$SSP126_7000_delta <- 
  exposurePop_V1$SSP126_7000_mu - exposurePop_V1$Historic_mu
exposurePop_V1$SSP585_1040_delta <- 
  exposurePop_V1$SSP585_1040_mu - exposurePop_V1$Historic_mu
exposurePop_V1$SSP585_4070_delta <- 
  exposurePop_V1$SSP585_4070_mu - exposurePop_V1$Historic_mu
exposurePop_V1$SSP585_7000_delta <- 
  exposurePop_V1$SSP585_7000_mu - exposurePop_V1$Historic_mu

exposurePop_V3$Historic_mu <- datPop$Historic * datOccH_V3$Mu
exposurePop_V3$SSP126_1040_mu <- datPop$SSP126_1040 * datOcc126_1040_V3$Mu
exposurePop_V3$SSP126_4070_mu <- datPop$SSP126_4070 * datOcc126_4070_V3$Mu
exposurePop_V3$SSP126_7000_mu <- datPop$SSP126_7000 * datOcc126_7000_V3$Mu
exposurePop_V3$SSP585_1040_mu <- datPop$SSP585_1040 * datOcc585_1040_V3$Mu
exposurePop_V3$SSP585_4070_mu <- datPop$SSP585_4070 * datOcc585_4070_V3$Mu
exposurePop_V3$SSP585_7000_mu <- datPop$SSP585_7000 * datOcc585_7000_V3$Mu
exposurePop_V3$SSP126_1040_delta <- 
  exposurePop_V3$SSP126_1040_mu - exposurePop_V3$Historic_mu
exposurePop_V3$SSP126_4070_delta <- 
  exposurePop_V3$SSP126_4070_mu - exposurePop_V3$Historic_mu
exposurePop_V3$SSP126_7000_delta <- 
  exposurePop_V3$SSP126_7000_mu - exposurePop_V3$Historic_mu
exposurePop_V3$SSP585_1040_delta <- 
  exposurePop_V3$SSP585_1040_mu - exposurePop_V3$Historic_mu
exposurePop_V3$SSP585_4070_delta <- 
  exposurePop_V3$SSP585_4070_mu - exposurePop_V3$Historic_mu
exposurePop_V3$SSP585_7000_delta <- 
  exposurePop_V3$SSP585_7000_mu - exposurePop_V3$Historic_mu

exposurePop_V4$Historic_mu <- datPop$Historic * datOccH_V4$Mu
exposurePop_V4$SSP126_1040_mu <- datPop$SSP126_1040 * datOcc126_1040_V4$Mu
exposurePop_V4$SSP126_4070_mu <- datPop$SSP126_4070 * datOcc126_4070_V4$Mu
exposurePop_V4$SSP126_7000_mu <- datPop$SSP126_7000 * datOcc126_7000_V4$Mu
exposurePop_V4$SSP585_1040_mu <- datPop$SSP585_1040 * datOcc585_1040_V4$Mu
exposurePop_V4$SSP585_4070_mu <- datPop$SSP585_4070 * datOcc585_4070_V4$Mu
exposurePop_V4$SSP585_7000_mu <- datPop$SSP585_7000 * datOcc585_7000_V4$Mu
exposurePop_V4$SSP126_1040_delta <- 
  exposurePop_V4$SSP126_1040_mu - exposurePop_V4$Historic_mu
exposurePop_V4$SSP126_4070_delta <- 
  exposurePop_V4$SSP126_4070_mu - exposurePop_V4$Historic_mu
exposurePop_V4$SSP126_7000_delta <- 
  exposurePop_V4$SSP126_7000_mu - exposurePop_V4$Historic_mu
exposurePop_V4$SSP585_1040_delta <- 
  exposurePop_V4$SSP585_1040_mu - exposurePop_V4$Historic_mu
exposurePop_V4$SSP585_4070_delta <- 
  exposurePop_V4$SSP585_4070_mu - exposurePop_V4$Historic_mu
exposurePop_V4$SSP585_7000_delta <- 
  exposurePop_V4$SSP585_7000_mu - exposurePop_V4$Historic_mu

# . . 5.3.2 Agriculture --------------------------------------------------------
exposureAg_V1$Historic_mu <- datLUH_AG$Historic * datOccH_V1$Mu
exposureAg_V1$SSP126_1040_mu <- datLUH_AG$SSP126_1040 * datOcc126_1040_V1$Mu
exposureAg_V1$SSP126_4070_mu <- datLUH_AG$SSP126_4070 * datOcc126_4070_V1$Mu
exposureAg_V1$SSP126_7000_mu <- datLUH_AG$SSP126_7000 * datOcc126_7000_V1$Mu
exposureAg_V1$SSP585_1040_mu <- datLUH_AG$SSP585_1040 * datOcc585_1040_V1$Mu
exposureAg_V1$SSP585_4070_mu <- datLUH_AG$SSP585_4070 * datOcc585_4070_V1$Mu
exposureAg_V1$SSP585_7000_mu <- datLUH_AG$SSP585_7000 * datOcc585_7000_V1$Mu
exposureAg_V1$SSP126_1040_delta <- 
  exposureAg_V1$SSP126_1040_mu - exposureAg_V1$Historic_mu
exposureAg_V1$SSP126_4070_delta <- 
  exposureAg_V1$SSP126_4070_mu - exposureAg_V1$Historic_mu
exposureAg_V1$SSP126_7000_delta <- 
  exposureAg_V1$SSP126_7000_mu - exposureAg_V1$Historic_mu
exposureAg_V1$SSP585_1040_delta <- 
  exposureAg_V1$SSP585_1040_mu - exposureAg_V1$Historic_mu
exposureAg_V1$SSP585_4070_delta <- 
  exposureAg_V1$SSP585_4070_mu - exposureAg_V1$Historic_mu
exposureAg_V1$SSP585_7000_delta <- 
  exposureAg_V1$SSP585_7000_mu - exposureAg_V1$Historic_mu

exposureAg_V3$Historic_mu <- datLUH_AG$Historic * datOccH_V3$Mu
exposureAg_V3$SSP126_1040_mu <- datLUH_AG$SSP126_1040 * datOcc126_1040_V3$Mu
exposureAg_V3$SSP126_4070_mu <- datLUH_AG$SSP126_4070 * datOcc126_4070_V3$Mu
exposureAg_V3$SSP126_7000_mu <- datLUH_AG$SSP126_7000 * datOcc126_7000_V3$Mu
exposureAg_V3$SSP585_1040_mu <- datLUH_AG$SSP585_1040 * datOcc585_1040_V3$Mu
exposureAg_V3$SSP585_4070_mu <- datLUH_AG$SSP585_4070 * datOcc585_4070_V3$Mu
exposureAg_V3$SSP585_7000_mu <- datLUH_AG$SSP585_7000 * datOcc585_7000_V3$Mu
exposureAg_V3$SSP126_1040_delta <- 
  exposureAg_V3$SSP126_1040_mu - exposureAg_V3$Historic_mu
exposureAg_V3$SSP126_4070_delta <- 
  exposureAg_V3$SSP126_4070_mu - exposureAg_V3$Historic_mu
exposureAg_V3$SSP126_7000_delta <- 
  exposureAg_V3$SSP126_7000_mu - exposureAg_V3$Historic_mu
exposureAg_V3$SSP585_1040_delta <- 
  exposureAg_V3$SSP585_1040_mu - exposureAg_V3$Historic_mu
exposureAg_V3$SSP585_4070_delta <- 
  exposureAg_V3$SSP585_4070_mu - exposureAg_V3$Historic_mu
exposureAg_V3$SSP585_7000_delta <- 
  exposureAg_V3$SSP585_7000_mu - exposureAg_V3$Historic_mu

exposureAg_V4$Historic_mu <- datLUH_AG$Historic * datOccH_V4$Mu
exposureAg_V4$SSP126_1040_mu <- datLUH_AG$SSP126_1040 * datOcc126_1040_V4$Mu
exposureAg_V4$SSP126_4070_mu <- datLUH_AG$SSP126_4070 * datOcc126_4070_V4$Mu
exposureAg_V4$SSP126_7000_mu <- datLUH_AG$SSP126_7000 * datOcc126_7000_V4$Mu
exposureAg_V4$SSP585_1040_mu <- datLUH_AG$SSP585_1040 * datOcc585_1040_V4$Mu
exposureAg_V4$SSP585_4070_mu <- datLUH_AG$SSP585_4070 * datOcc585_4070_V4$Mu
exposureAg_V4$SSP585_7000_mu <- datLUH_AG$SSP585_7000 * datOcc585_7000_V4$Mu
exposureAg_V4$SSP126_1040_delta <- 
  exposureAg_V4$SSP126_1040_mu - exposureAg_V4$Historic_mu
exposureAg_V4$SSP126_4070_delta <- 
  exposureAg_V4$SSP126_4070_mu - exposureAg_V4$Historic_mu
exposureAg_V4$SSP126_7000_delta <- 
  exposureAg_V4$SSP126_7000_mu - exposureAg_V4$Historic_mu
exposureAg_V4$SSP585_1040_delta <- 
  exposureAg_V4$SSP585_1040_mu - exposureAg_V4$Historic_mu
exposureAg_V4$SSP585_4070_delta <- 
  exposureAg_V4$SSP585_4070_mu - exposureAg_V4$Historic_mu
exposureAg_V4$SSP585_7000_delta <- 
  exposureAg_V4$SSP585_7000_mu - exposureAg_V4$Historic_mu

# . . 5.3.3 Forestry -----------------------------------------------------------
exposureFor_V1$Historic_mu <- datLUH_FOR$Historic * datOccH_V1$Mu
exposureFor_V1$SSP126_1040_mu <- datLUH_FOR$SSP126_1040 * datOcc126_1040_V1$Mu
exposureFor_V1$SSP126_4070_mu <- datLUH_FOR$SSP126_4070 * datOcc126_4070_V1$Mu
exposureFor_V1$SSP126_7000_mu <- datLUH_FOR$SSP126_7000 * datOcc126_7000_V1$Mu
exposureFor_V1$SSP585_1040_mu <- datLUH_FOR$SSP585_1040 * datOcc585_1040_V1$Mu
exposureFor_V1$SSP585_4070_mu <- datLUH_FOR$SSP585_4070 * datOcc585_4070_V1$Mu
exposureFor_V1$SSP585_7000_mu <- datLUH_FOR$SSP585_7000 * datOcc585_7000_V1$Mu
exposureFor_V1$SSP126_1040_delta <- 
  exposureFor_V1$SSP126_1040_mu - exposureFor_V1$Historic_mu
exposureFor_V1$SSP126_4070_delta <- 
  exposureFor_V1$SSP126_4070_mu - exposureFor_V1$Historic_mu
exposureFor_V1$SSP126_7000_delta <- 
  exposureFor_V1$SSP126_7000_mu - exposureFor_V1$Historic_mu
exposureFor_V1$SSP585_1040_delta <- 
  exposureFor_V1$SSP585_1040_mu - exposureFor_V1$Historic_mu
exposureFor_V1$SSP585_4070_delta <- 
  exposureFor_V1$SSP585_4070_mu - exposureFor_V1$Historic_mu
exposureFor_V1$SSP585_7000_delta <- 
  exposureFor_V1$SSP585_7000_mu - exposureFor_V1$Historic_mu

exposureFor_V3$Historic_mu <- datLUH_FOR$Historic * datOccH_V3$Mu
exposureFor_V3$SSP126_1040_mu <- datLUH_FOR$SSP126_1040 * datOcc126_1040_V3$Mu
exposureFor_V3$SSP126_4070_mu <- datLUH_FOR$SSP126_4070 * datOcc126_4070_V3$Mu
exposureFor_V3$SSP126_7000_mu <- datLUH_FOR$SSP126_7000 * datOcc126_7000_V3$Mu
exposureFor_V3$SSP585_1040_mu <- datLUH_FOR$SSP585_1040 * datOcc585_1040_V3$Mu
exposureFor_V3$SSP585_4070_mu <- datLUH_FOR$SSP585_4070 * datOcc585_4070_V3$Mu
exposureFor_V3$SSP585_7000_mu <- datLUH_FOR$SSP585_7000 * datOcc585_7000_V3$Mu
exposureFor_V3$SSP126_1040_delta <- 
  exposureFor_V3$SSP126_1040_mu - exposureFor_V3$Historic_mu
exposureFor_V3$SSP126_4070_delta <- 
  exposureFor_V3$SSP126_4070_mu - exposureFor_V3$Historic_mu
exposureFor_V3$SSP126_7000_delta <- 
  exposureFor_V3$SSP126_7000_mu - exposureFor_V3$Historic_mu
exposureFor_V3$SSP585_1040_delta <- 
  exposureFor_V3$SSP585_1040_mu - exposureFor_V3$Historic_mu
exposureFor_V3$SSP585_4070_delta <- 
  exposureFor_V3$SSP585_4070_mu - exposureFor_V3$Historic_mu
exposureFor_V3$SSP585_7000_delta <- 
  exposureFor_V3$SSP585_7000_mu - exposureFor_V3$Historic_mu

exposureFor_V4$Historic_mu <- datLUH_FOR$Historic * datOccH_V4$Mu
exposureFor_V4$SSP126_1040_mu <- datLUH_FOR$SSP126_1040 * datOcc126_1040_V4$Mu
exposureFor_V4$SSP126_4070_mu <- datLUH_FOR$SSP126_4070 * datOcc126_4070_V4$Mu
exposureFor_V4$SSP126_7000_mu <- datLUH_FOR$SSP126_7000 * datOcc126_7000_V4$Mu
exposureFor_V4$SSP585_1040_mu <- datLUH_FOR$SSP585_1040 * datOcc585_1040_V4$Mu
exposureFor_V4$SSP585_4070_mu <- datLUH_FOR$SSP585_4070 * datOcc585_4070_V4$Mu
exposureFor_V4$SSP585_7000_mu <- datLUH_FOR$SSP585_7000 * datOcc585_7000_V4$Mu
exposureFor_V4$SSP126_1040_delta <- 
  exposureFor_V4$SSP126_1040_mu - exposureFor_V4$Historic_mu
exposureFor_V4$SSP126_4070_delta <- 
  exposureFor_V4$SSP126_4070_mu - exposureFor_V4$Historic_mu
exposureFor_V4$SSP126_7000_delta <- 
  exposureFor_V4$SSP126_7000_mu - exposureFor_V4$Historic_mu
exposureFor_V4$SSP585_1040_delta <- 
  exposureFor_V4$SSP585_1040_mu - exposureFor_V4$Historic_mu
exposureFor_V4$SSP585_4070_delta <- 
  exposureFor_V4$SSP585_4070_mu - exposureFor_V4$Historic_mu
exposureFor_V4$SSP585_7000_delta <- 
  exposureFor_V4$SSP585_7000_mu - exposureFor_V4$Historic_mu

# . 5.4 Creating a table of values ---------------------------------------------
sEH <- summary(exposurePop_V1$Historic_mu)
sEE126 <- summary(exposurePop_V1$SSP126_1040_mu)
sEM126 <- summary(exposurePop_V1$SSP126_4070_mu)
sEL126 <- summary(exposurePop_V1$SSP126_7000_mu)
sEE585 <- summary(exposurePop_V1$SSP585_1040_mu)
sEM585 <- summary(exposurePop_V1$SSP585_4070_mu)
sEL585 <- summary(exposurePop_V1$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sPminV1 <- min(sE$'Freq Min.')
sPmaxV1 <- max(sE$'Freq Max.')

sEH <- summary(exposurePop_V3$Historic_mu)
sEE126 <- summary(exposurePop_V3$SSP126_1040_mu)
sEM126 <- summary(exposurePop_V3$SSP126_4070_mu)
sEL126 <- summary(exposurePop_V3$SSP126_7000_mu)
sEE585 <- summary(exposurePop_V3$SSP585_1040_mu)
sEM585 <- summary(exposurePop_V3$SSP585_4070_mu)
sEL585 <- summary(exposurePop_V3$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sPminV3 <- min(sE$'Freq Min.')
sPmaxV3 <- max(sE$'Freq Max.')

sEH <- summary(exposurePop_V4$Historic_mu)
sEE126 <- summary(exposurePop_V4$SSP126_1040_mu)
sEM126 <- summary(exposurePop_V4$SSP126_4070_mu)
sEL126 <- summary(exposurePop_V4$SSP126_7000_mu)
sEE585 <- summary(exposurePop_V4$SSP585_1040_mu)
sEM585 <- summary(exposurePop_V4$SSP585_4070_mu)
sEL585 <- summary(exposurePop_V4$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sPminV4 <- min(sE$'Freq Min.')
sPmaxV4 <- max(sE$'Freq Max.')
###
sEH <- summary(exposureAg_V1$Historic_mu)
sEE126 <- summary(exposureAg_V1$SSP126_1040_mu)
sEM126 <- summary(exposureAg_V1$SSP126_4070_mu)
sEL126 <- summary(exposureAg_V1$SSP126_7000_mu)
sEE585 <- summary(exposureAg_V1$SSP585_1040_mu)
sEM585 <- summary(exposureAg_V1$SSP585_4070_mu)
sEL585 <- summary(exposureAg_V1$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sAminV1 <- min(sE$'Freq Min.')
sAmaxV1 <- max(sE$'Freq Max.')

sEH <- summary(exposureAg_V3$Historic_mu)
sEE126 <- summary(exposureAg_V3$SSP126_1040_mu)
sEM126 <- summary(exposureAg_V3$SSP126_4070_mu)
sEL126 <- summary(exposureAg_V3$SSP126_7000_mu)
sEE585 <- summary(exposureAg_V3$SSP585_1040_mu)
sEM585 <- summary(exposureAg_V3$SSP585_4070_mu)
sEL585 <- summary(exposureAg_V3$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sAminV3 <- min(sE$'Freq Min.')
sAmaxV3 <- max(sE$'Freq Max.')

sEH <- summary(exposureAg_V4$Historic_mu)
sEE126 <- summary(exposureAg_V4$SSP126_1040_mu)
sEM126 <- summary(exposureAg_V4$SSP126_4070_mu)
sEL126 <- summary(exposureAg_V4$SSP126_7000_mu)
sEE585 <- summary(exposureAg_V4$SSP585_1040_mu)
sEM585 <- summary(exposureAg_V4$SSP585_4070_mu)
sEL585 <- summary(exposureAg_V4$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sAminV4 <- min(sE$'Freq Min.')
sAmaxV4 <- max(sE$'Freq Max.')
###
sEH <- summary(exposureFor_V1$Historic_mu)
sEE126 <- summary(exposureFor_V1$SSP126_1040_mu)
sEM126 <- summary(exposureFor_V1$SSP126_4070_mu)
sEL126 <- summary(exposureFor_V1$SSP126_7000_mu)
sEE585 <- summary(exposureFor_V1$SSP585_1040_mu)
sEM585 <- summary(exposureFor_V1$SSP585_4070_mu)
sEL585 <- summary(exposureFor_V1$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sFminV1 <- min(sE$'Freq Min.')
sFmaxV1 <- max(sE$'Freq Max.')

sEH <- summary(exposureFor_V3$Historic_mu)
sEE126 <- summary(exposureFor_V3$SSP126_1040_mu)
sEM126 <- summary(exposureFor_V3$SSP126_4070_mu)
sEL126 <- summary(exposureFor_V3$SSP126_7000_mu)
sEE585 <- summary(exposureFor_V3$SSP585_1040_mu)
sEM585 <- summary(exposureFor_V3$SSP585_4070_mu)
sEL585 <- summary(exposureFor_V3$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sFminV3 <- min(sE$'Freq Min.')
sFmaxV3 <- max(sE$'Freq Max.')

sEH <- summary(exposureFor_V4$Historic_mu)
sEE126 <- summary(exposureFor_V4$SSP126_1040_mu)
sEM126 <- summary(exposureFor_V4$SSP126_4070_mu)
sEL126 <- summary(exposureFor_V4$SSP126_7000_mu)
sEE585 <- summary(exposureFor_V4$SSP585_1040_mu)
sEM585 <- summary(exposureFor_V4$SSP585_4070_mu)
sEL585 <- summary(exposureFor_V4$SSP585_7000_mu)
sE <- rbind(sEH, sEE126, sEM126, sEL126, sEE585, sEM585, sEL585) %>% as_tibble()
colnames(sE) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sFminV4 <- min(sE$'Freq Min.')
sFmaxV4 <- max(sE$'Freq Max.')

# . 5.5 Plotting ---------------------------------------------------------------
# . . 5.5.1 Writing Files ------------------------------------------------------
write.csv(exposurePop_V1,file=paste0(fileloc1,'Results/','EXPOSURE_POP_',var[1],'.csv'),
          row.names = FALSE)
write.csv(exposurePop_V3,file=paste0(fileloc1,'Results/','EXPOSURE_POP_',var[3],'.csv'),
          row.names = FALSE)
write.csv(exposurePop_V4,file=paste0(fileloc1,'Results/','EXPOSURE_POP_',var[4],'.csv'),
          row.names = FALSE)

write.csv(exposureAg_V1,file=paste0(fileloc1,'Results/','EXPOSURE_AG_',var[1],'.csv'),
          row.names = FALSE)
write.csv(exposureAg_V3,file=paste0(fileloc1,'Results/','EXPOSURE_AG_',var[3],'.csv'),
          row.names = FALSE)
write.csv(exposureAg_V4,file=paste0(fileloc1,'Results/','EXPOSURE_AG_',var[4],'.csv'),
          row.names = FALSE)

write.csv(exposureFor_V1,file=paste0(fileloc1,'Results/','EXPOSURE_FOR_',var[1],'.csv'),
          row.names = FALSE)
write.csv(exposureFor_V3,file=paste0(fileloc1,'Results/','EXPOSURE_FOR_',var[3],'.csv'),
          row.names = FALSE)
write.csv(exposureFor_V4,file=paste0(fileloc1,'Results/','EXPOSURE_FOR_',var[4],'.csv'),
          row.names = FALSE)

# . . 5.5.2 Population ---------------------------------------------------------
varTitle <- c("Heatwaves","Coldwaves",'Extreme Precipitation', 'Flash Droughts')
var <- c('tasmax', 'tasmin','pr', 'mrsos')

a <- 'Historical'
p1 <- ggplot(data = exposurePop_V1, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV1,sPmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposurePop_V1, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV1,sPmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposurePop_V1, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV1,sPmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposurePop_V1, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV1,sPmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposurePop_V1, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV1,sPmaxV1), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposurePop_V1, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV1,sPmaxV1), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposurePop_V1, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV1,sPmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Population Exposure to ", varTitle[1]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_POP_', var[1], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# Var3
a <- 'Historical'
p1 <- ggplot(data = exposurePop_V3, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV3,sPmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposurePop_V3, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV3,sPmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposurePop_V3, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV3,sPmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposurePop_V3, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV3,sPmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposurePop_V3, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV3,sPmaxV3), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposurePop_V3, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV3,sPmaxV3), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposurePop_V3, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV3,sPmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Population Exposure to ", varTitle[3]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_POP_', var[3], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# Var4
a <- 'Historical'
p1 <- ggplot(data = exposurePop_V4, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposurePop_V4, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposurePop_V4, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposurePop_V4, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposurePop_V4, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposurePop_V4, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposurePop_V4, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Population Exposure to ", varTitle[4]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_POP_', var[4], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# . . 5.5.2 Agriculture --------------------------------------------------------
a <- 'Historical'
p1 <- ggplot(data = exposureAg_V1, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV1,sAmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureAg_V1, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV1,sAmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureAg_V1, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV1,sAmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureAg_V1, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV1,sAmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureAg_V1, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV1,sAmaxV1), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureAg_V1, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV1,sAmaxV1), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureAg_V1, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV1,sAmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Agriculture Exposure to ", varTitle[1]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_AG_', var[1], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# Var3
a <- 'Historical'
p1 <- ggplot(data = exposureAg_V3, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV3,sAmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureAg_V3, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV3,sAmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureAg_V3, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV3,sAmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureAg_V3, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV3,sAmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureAg_V3, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV3,sAmaxV3), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureAg_V3, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV3,sAmaxV3), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureAg_V3, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sAminV3,sAmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Agriculture Exposure to ", varTitle[3]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_AG_', var[3], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# Var4
a <- 'Historical'
p1 <- ggplot(data = exposureAg_V4, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureAg_V4, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureAg_V4, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureAg_V4, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureAg_V4, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureAg_V4, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureAg_V4, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sPminV4,sPmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Agriculture Exposure to ", varTitle[4]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_AG_', var[4], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# . . 5.5.2 Forestry -----------------------------------------------------------
a <- 'Historical'
p1 <- ggplot(data = exposureFor_V1, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV1,sFmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureFor_V1, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV1,sFmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureFor_V1, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV1,sFmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureFor_V1, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV1,sFmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureFor_V1, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV1,sFmaxV1), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureFor_V1, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV1,sFmaxV1), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureFor_V1, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV1,sFmaxV1), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Forestry Exposure to ", varTitle[1]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_FOR_', var[1], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# Var3
a <- 'Historical'
p1 <- ggplot(data = exposureFor_V3, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV3,sFmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureFor_V3, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV3,sFmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureFor_V3, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV3,sFmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureFor_V3, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV3,sFmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureFor_V3, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV3,sFmaxV3), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureFor_V3, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV3,sFmaxV3), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureFor_V3, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV3,sFmaxV3), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Forestry Exposure to ", varTitle[3]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_FOR_', var[3], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# Var4
a <- 'Historical'
p1 <- ggplot(data = exposureFor_V4, aes(x=lon, y=lat, fill=Historic_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV4,sFmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Exposure to Climate Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
p2 <- ggplot(data = exposureFor_V4, aes(x=lon, y=lat, fill=SSP126_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV4,sFmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p3 <- ggplot(data = exposureFor_V4, aes(x=lon, y=lat, fill=SSP126_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV4,sFmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p4 <- ggplot(data = exposureFor_V4, aes(x=lon, y=lat, fill=SSP126_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV4,sFmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p5 <- ggplot(data = exposureFor_V4, aes(x=lon, y=lat, fill=SSP585_1040_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV4,sFmaxV4), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
p6 <- ggplot(data = exposureFor_V4, aes(x=lon, y=lat, fill=SSP585_4070_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV4,sFmaxV4), option = "rocket",
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
p7 <- ggplot(data = exposureFor_V4, aes(x=lon, y=lat, fill=SSP585_7000_mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFminV4,sFmaxV4), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0("Forestry Exposure to ", varTitle[4]), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)
ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_FOR_', var[4], ".tiff", sep=''),
       width = 9.5, height = 6.5, dpi = 350, bg='white')

# END ##########################################################################