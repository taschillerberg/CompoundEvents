# P4_Compound_Month.R
# About: This program will open the exceedance file for the selected 
#       variable and calculate a 'wave' of when the exceedance occurs.
#
# Inputs: COMP
# Outputs: COMPOUND Figures
#
# T. A. Schillerberg
#               Feb. 2022
#      Updated: Jul. 2023

# Office Computer
fileloc1 <- 'C:/Research/Data/'

# HPC
# fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)

# Part I Variables To Change ##############################################
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
timeStep <- c('DAY_','WEEK_FD_','WEEK_D_','MONTH_FD_','MONTH_D_')[1]
compNum <- 8 # 1:8 1 & 7
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')[compNum]
compTitle <- c('Simultanious Heat & Flash Drought','Sequential Heat & Flash Drought',
               'Sequential Flash Drought & Heat','Simultanious Heat & Extreme Precip',
               'Sequential Heat & Extreme Precip','Sequential Precipitation & Heat',
               'Sequential Extreme Precip & Flash Drought',
               'Sequential Flash Drought & Extreme Precip')[compNum]
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")

print(paste0('Model ',loc2))
print('Rscript: P4_Compound_Figures.R')
print(paste0('Compound Event: ', comp))
# Part Functions ###############################################################
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
# Part III Mean Figure (no sig) ################################################
# . 3.1 Variables Needed -------------------------------------------------------
datCompH <- tibble(
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
datComp126_1040 <- datCompH
datComp126_4070 <- datCompH
datComp126_7000 <- datCompH
datComp585_1040 <- datCompH
datComp585_4070 <- datCompH
datComp585_7000 <- datCompH

# . 3.2 Opening & Formatting Exceed Files --------------------------------------
# . . 3.2.1 Historical ---------------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[1],loc2[i],'COMP_',timeStep,comp,
                         mFileH[i],1980,'-',2010,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  if (i == 1){
    datCompH$lon <- dat$lon
    datCompH$lat <- dat$lat
    datCompH$`CMCC-ESM2` <- dat$Prec_Events
  } else {
    datCompH[,2+i] <- dat$Prec_Events
  }
}
datCompH$Mu <- apply(datCompH[,3:10], MARGIN = 1, function(x){
  sum(x)/8
})
# . . 3.2.2 SSP126 2010-40 -----------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[2],loc2[i],'COMP_',timeStep,comp,
                         mFile126[i],2010,'-',2040,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  if (i == 1){
    datComp126_1040$lon <- dat$lon
    datComp126_1040$lat <- dat$lat
    datComp126_1040$`CMCC-ESM2` <- dat$Prec_Events
  } else {
    datComp126_1040[,2+i] <- dat$Prec_Events
  }
}
datComp126_1040$Mu <- apply(datComp126_1040[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})

# . . 3.2.3 SSP126 2040-70 -----------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[2],loc2[i],'COMP_',timeStep,comp,
                         mFile126[i],2040,'-',2070,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  if (i == 1){
    datComp126_4070$lon <- dat$lon
    datComp126_4070$lat <- dat$lat
    datComp126_4070$`CMCC-ESM2` <- dat$Prec_Events
  } else {
    datComp126_4070[,2+i] <- dat$Prec_Events
  }
}
datComp126_4070$Mu <- apply(datComp126_4070[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})

# . . 3.2.4 SSP126 2070-2100 ---------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[2],loc2[i],'COMP_',timeStep,comp,
                         mFile126[i],2070,'-',2100,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  if (i == 1){
    datComp126_7000$lon <- dat$lon
    datComp126_7000$lat <- dat$lat
    datComp126_7000$`CMCC-ESM2` <- dat$Prec_Events
  } else {
    datComp126_7000[,2+i] <- dat$Prec_Events
  }
}
datComp126_7000$Mu <- apply(datComp126_7000[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})

# . . 3.2.5 SSP585 2010-40 -----------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[3],loc2[i],'COMP_',timeStep,comp,
                         mFile585[i],2010,'-',2040,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  if (i == 1){
    datComp585_1040$lon <- dat$lon
    datComp585_1040$lat <- dat$lat
    datComp585_1040$`CMCC-ESM2` <- dat$Prec_Events
  } else {
    datComp585_1040[,2+i] <- dat$Prec_Events
  }
}
datComp585_1040$Mu <- apply(datComp585_1040[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})

# . . 3.2.6 SSP585 2040-70 -----------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[3],loc2[i],'COMP_',timeStep,comp,
                         mFile585[i],2040,'-',2070,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  if (i == 1){
    datComp585_4070$lon <- dat$lon
    datComp585_4070$lat <- dat$lat
    datComp585_4070$`CMCC-ESM2` <- dat$Prec_Events
  } else {
    datComp585_4070[,2+i] <- dat$Prec_Events
  }
}
datComp585_4070$Mu <- apply(datComp585_4070[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})

# . . 3.2.7 SSP585 2070-2100 ---------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[3],loc2[i],'COMP_',timeStep,comp,
                         mFile585[i],2070,'-',2100,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  if (i == 1){
    datComp585_7000$lon <- dat$lon
    datComp585_7000$lat <- dat$lat
    datComp585_7000$`CMCC-ESM2` <- dat$Prec_Events
  } else {
    datComp585_7000[,2+i] <- dat$Prec_Events
  }
}
datComp585_7000$Mu <- apply(datComp585_7000[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})

# . 3.3 Creating a table of values ---------------------------------------------
sFH <- summary(datCompH$Mu) %>% round(digits = 3)
sFE126 <- summary(datComp126_1040$Mu) %>% round(digits = 3)
sFM126 <- summary(datComp126_4070$Mu) %>% round(digits = 3)
sFL126 <- summary(datComp126_7000$Mu) %>% round(digits = 3)
sFE585 <- summary(datComp585_1040$Mu) %>% round(digits = 3)
sFM585 <- summary(datComp585_4070$Mu) %>% round(digits = 3)
sFL585 <- summary(datComp585_7000$Mu) %>% round(digits = 3)
sF <- rbind(sFH, sFE126, sFM126, sFL126, sFE585, sFM585, sFL585) %>% as_tibble()
colnames(sF) <- c('Min.','1st Qu.','Median','Mean',
                  '3rd Qu.','Max.')
sFmin <- min(sF$'Min.')
sFmax <- max(sF$'Max.')

# . 3.4 Plotting the Compound Frequencies --------------------------------------
a <- 'Historical'
pM1 <- ggplot(data = datCompH, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Occurrence of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Early-Century'
pM2 <- ggplot(data = datComp126_1040, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
pM3 <- ggplot(data = datComp126_4070, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
pM4 <- ggplot(data = datComp126_7000, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
pM5 <- ggplot(data = datComp585_1040, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century'
pM6 <- ggplot(data = datComp585_4070, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century'
pM7 <- ggplot(data = datComp585_7000, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(pM1, position = 'right') %>% 
  as_ggplot()
pM1 <- pM1 + theme(legend.position = "NULL")

F1A <- plot_grid(pM2, pM3, pM4, 
                 pM5, pM6, pM7,
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))
rownames(sF) <- c('Historical',
                  'SSP126 Early-Cent.', 'SSP126 Mid-Cent.','SSP126 Late-Cent.',
                  'SSP585 Early-Cent.', 'SSP585 Mid-Cent.','SSP585 Late-Cent.')
F1B <- plot_grid(gridExtra::tableGrob(sF),
                 rel_widths = c(1),
                 nrow= 1)
F1B <- plot_grid(pM1, myLegend, F1B,
                 nrow = 1,
                 rel_widths = c(0.70,0.2,1.2))
title <- ggdraw() + draw_label(paste0("Occurrence of ", compTitle), fontface='bold')
F1 <- plot_grid(title,
                F1B,
                F1A,
                rel_heights = c(.05,.5,1),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','COMP_', timeStep ,comp, ".tiff", sep=''),
       width = 14, height = 9, dpi = 350, bg='white')

# Part IV Relative Change ######################################################
# . 4.1 Variables Needed -------------------------------------------------------
relativeChange <- tibble(
  'lon' = datCompH$lon,
  'lat' = datCompH$lat,
  'Historical_mu' = numeric(length = 12476),
  'SSP126_1040_mu' = numeric(length = 12476),
  'SSP126_1040_delta' = numeric(length = 12476),
  'SSP126_1040_Sig' = numeric(length = 12476),
  'SSP126_4070_mu' = numeric(length = 12476),
  'SSP126_4070_delta' = numeric(length = 12476),
  'SSP126_4070_Sig' = numeric(length = 12476),
  'SSP126_7000_mu' = numeric(length = 12476),
  'SSP126_7000_delta' = numeric(length = 12476),
  'SSP126_7000_Sig' = numeric(length = 12476),
  
  'SSP585_1040_mu' = numeric(length = 12476),
  'SSP585_1040_delta' = numeric(length = 12476),
  'SSP585_1040_Sig' = numeric(length = 12476),
  'SSP585_4070_mu' = numeric(length = 12476),
  'SSP585_4070_delta' = numeric(length = 12476),
  'SSP585_4070_Sig' = numeric(length = 12476),
  'SSP585_7000_mu' = numeric(length = 12476),
  'SSP585_7000_delta' = numeric(length = 12476),
  'SSP585_7000_Sig' = numeric(length = 12476)
)

dat <- tibble(
  'lon' = datCompH$lon,
  'lat' = datCompH$lat,
  'CMCC-ESM2'  = numeric(length = 12476), 
  'EC-Earth3'  = numeric(length = 12476),
  'GFDL-ESM4' = numeric(length = 12476),
  'INM-CM4-8' = numeric(length = 12476),
  'INM-CM5-0' = numeric(length = 12476),
  'MPI-ESM1-2-HR' = numeric(length = 12476),
  'MRI-ESM2-0' = numeric(length = 12476), 
  'NorESM2-MM'  = numeric(length = 12476),
  'Mu' = numeric(length = 12476),
  "Positive" = numeric(length = 12476),
  "Negative" = numeric(length = 12476)
)

# . 4.2 Calculating ------------------------------------------------------------
relativeChange$Historical_mu <- datCompH$Mu

# . . 4.2.1 SSP126 2010-40 -----------------------------------------------------
# . . . 5.2.1.1 Difference ---
for (i in 1: length(loc2)){
  dat[,2+i] <- datComp126_1040[,2+i] - datCompH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relativeChange$SSP126_1040_delta <- dat$Mu
relativeChange$SSP126_1040_mu <- datComp126_1040$Mu

# . . . 5.2.1.2 Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relativeChange$SSP126_1040_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . 4.2.2 SSP126 2040-70 -----------------------------------------------------
# . . . 5.2.2.1 Difference ---
for (i in 1: length(loc2)){
  dat[,2+i] <- datComp126_4070[,2+i] - datCompH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relativeChange$SSP126_4070_delta <- dat$Mu
relativeChange$SSP126_4070_mu <- datComp126_4070$Mu

# . . . 5.2.2.2 Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relativeChange$SSP126_4070_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . 4.2.3 SSP126 2070-2100 ---------------------------------------------------
# . . . 5.2.3.1 Difference ---
for (i in 1: length(loc2)){
  dat[,2+i] <- datComp126_7000[,2+i] - datCompH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relativeChange$SSP126_7000_delta <- dat$Mu
relativeChange$SSP126_7000_mu <- datComp126_7000$Mu

# . . . 5.2.3.2 Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relativeChange$SSP126_7000_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . 4.2.4 SSP585 2010-40 -----------------------------------------------------
# . . . 5.2.4.1 Difference ---
for (i in 1: length(loc2)){
  dat[,2+i] <- datComp585_1040[,2+i] - datCompH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relativeChange$SSP585_1040_delta <- dat$Mu
relativeChange$SSP585_1040_mu <- datComp585_1040$Mu

# . . . 5.2.4.2 Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relativeChange$SSP585_1040_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . 4.2.5 SSP585 2040-70 -----------------------------------------------------
# . . . 5.2.5.1 Difference ---
for (i in 1: length(loc2)){
  dat[,2+i] <- datComp585_4070[,2+i] - datCompH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relativeChange$SSP585_4070_delta <- dat$Mu
relativeChange$SSP585_4070_mu <- datComp585_4070$Mu

# . . . 5.2.5.2 Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relativeChange$SSP585_4070_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . 4.2.6 SSP585 2070-2100 ---------------------------------------------------
# . . . 5.2.6.1 Difference ---
for (i in 1: length(loc2)){
  dat[,2+i] <- datComp585_7000[,2+i] - datCompH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relativeChange$SSP585_7000_delta <- dat$Mu
relativeChange$SSP585_7000_mu <- datComp585_7000$Mu

# . . . 5.2.6.2 Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relativeChange$SSP585_7000_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . 4.3 Saving -----------------------------------------------------------------
write.csv(relativeChange,file=paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
          row.names = FALSE)

# Part V Relative Change Figures w/ significance ###############################
# . 5.2 Opening File -----------------------------------------------------------
dat <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
                col_names = TRUE, cols(.default = col_double()))
# . . 5.2.1 Formatting ---
maxLimt <- cbind(dat$SSP126_1040_delta, dat$SSP126_4070_delta, 
                 dat$SSP126_7000_delta, dat$SSP585_1040_delta, 
                 dat$SSP585_4070_delta, dat$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(dat$SSP126_1040_delta, dat$SSP126_4070_delta, 
                 dat$SSP126_7000_delta, dat$SSP585_1040_delta, 
                 dat$SSP585_4070_delta, dat$SSP585_7000_delta) %>%
  min()

# . 5.3 Plotting ---------------------------------------------------------------
a <- 'SSP126 Early-Century - Historical'
p1 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP126_1040_delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_1040_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century - Historical'
p2 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century - Historical'
p3 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century - Historical'
p4 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP585_1040_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_1040_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Mid-Century - Historical'
p5 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Late-Century - Historical'
p6 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, p2, p3,
                 p4, p5, p6,
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))

title <- ggdraw() + draw_label(paste0("Change in occurrence of ", compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','MU_CHANG_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9.5, dpi = 350, bg='white')

# Part VI Percent Change #######################################################
# . 6.1 Variables Needed -------------------------------------------------------
percentChange <- tibble(
  'lon' = datCompH$lon,
  'lat' = datCompH$lat,
  'SSP126_1040' = numeric(length = 12476),
  'SSP126_4070' = numeric(length = 12476),
  'SSP126_7000' = numeric(length = 12476),
  'SSP585_1040' = numeric(length = 12476),
  'SSP585_4070' = numeric(length = 12476),
  'SSP585_7000' = numeric(length = 12476),
)

# . 6.2 Opening Files ----------------------------------------------------------
dat <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
                col_names = TRUE, cols(.default = col_double()))

# . 6.3 Calculation of the Percent Change --------------------------------------
percentChange$SSP126_1040 <- (datComp126_1040$Mu - datCompH$Mu) /datCompH$Mu *100
percentChange$SSP126_4070 <- (datComp126_4070$Mu - datCompH$Mu) /datCompH$Mu *100
percentChange$SSP126_7000 <- (datComp126_7000$Mu - datCompH$Mu) /datCompH$Mu *100
percentChange$SSP585_1040 <- (datComp585_1040$Mu - datCompH$Mu) /datCompH$Mu *100
percentChange$SSP585_4070 <- (datComp585_4070$Mu - datCompH$Mu) /datCompH$Mu *100
percentChange$SSP585_7000 <- (datComp585_7000$Mu - datCompH$Mu) /datCompH$Mu *100

# Addressing NAN : future=0, historical = 0
percentChange$SSP126_1040[is.na(percentChange$SSP126_1040)] <- 0
percentChange$SSP126_4070[is.na(percentChange$SSP126_4070)] <- 0
percentChange$SSP126_7000[is.na(percentChange$SSP126_7000)] <- 0
percentChange$SSP585_1040[is.na(percentChange$SSP585_1040)] <- 0
percentChange$SSP585_4070[is.na(percentChange$SSP585_4070)] <- 0
percentChange$SSP585_7000[is.na(percentChange$SSP585_7000)] <- 0

# Addressing infinity : future=#, historical = 0
percentChange$SSP126_1040[is.infinite(percentChange$SSP126_1040)] <- NA
percentChange$SSP126_4070[is.infinite(percentChange$SSP126_4070)] <- NA
percentChange$SSP126_7000[is.infinite(percentChange$SSP126_7000)] <- NA
percentChange$SSP585_1040[is.infinite(percentChange$SSP585_1040)] <- NA
percentChange$SSP585_4070[is.infinite(percentChange$SSP585_4070)] <- NA
percentChange$SSP585_7000[is.infinite(percentChange$SSP585_7000)] <- NA

percentChange$SSP126_1040[is.na(percentChange$SSP126_1040)] <- 
  max(percentChange$SSP126_1040, na.rm = TRUE)
percentChange$SSP126_4070[is.na(percentChange$SSP126_4070)] <- 
  max(percentChange$SSP126_4070, na.rm = TRUE)
percentChange$SSP126_7000[is.na(percentChange$SSP126_7000)] <- 
  max(percentChange$SSP126_7000, na.rm = TRUE)
percentChange$SSP585_1040[is.na(percentChange$SSP585_1040)] <- 
  max(percentChange$SSP585_1040, na.rm = TRUE)
percentChange$SSP585_4070[is.na(percentChange$SSP585_4070)] <- 
  max(percentChange$SSP585_4070, na.rm = TRUE)
percentChange$SSP585_7000[is.na(percentChange$SSP585_7000)] <- 
  max(percentChange$SSP585_7000, na.rm = TRUE)


sFmin <- min(rbind(percentChange$SSP126_1040, percentChange$SSP126_4070, 
                   percentChange$SSP126_7000, percentChange$SSP585_1040,
                   percentChange$SSP585_4070, percentChange$SSP585_7000))
sFmax <- max(rbind(percentChange$SSP126_1040, percentChange$SSP126_4070, 
                   percentChange$SSP126_7000, percentChange$SSP585_1040,
                   percentChange$SSP585_4070, percentChange$SSP585_7000))
names <- colnames(percentChange)
percentChange <- cbind(percentChange, dat$SSP126_1040_Sig, dat$SSP126_4070_Sig,
                       dat$SSP126_7000_Sig, dat$SSP585_1040_Sig, 
                       dat$SSP126_4070_Sig, dat$SSP585_7000_Sig)
colnames(percentChange) <- c(names, 'SSP126_1040_Sig', 'SSP126_4070_Sig',
                             'SSP126_7000_Sig', 'SSP585_1040_Sig',
                             'SSP585_4070_Sig', 'SSP585_7000_Sig')
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
x <- c(percentChange$SSP126_1040, percentChange$SSP126_4070,
       percentChange$SSP126_7000, percentChange$SSP585_1040,
       percentChange$SSP585_4070, percentChange$SSP585_7000) %>% scale_values
summary(x)

# . 6.4 Plotting ---------------------------------------------------------------
a <- 'SSP126 Early-Century'
p1 <- ggplot(data = percentChange, aes(x=lon, y=lat, fill=SSP126_1040)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "viridis", na.value = 'lightblue',
  #                      direction = -1, name = 'Percent Change',
  #                      trans = 'log10'
  #                      ) +
  scale_fill_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),
                       values = c(0,0.001,0.002,0.003,.03,1)) +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_1040_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a,
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
p2 <- ggplot(data = percentChange, aes(x=lon, y=lat, fill=SSP126_4070)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "viridis", 
  #                      na.value = 'lightblue', direction = -1) +
  scale_fill_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),
                       values = c(0,0.001,0.002,0.003,.03,1)) +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a,
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Late-Century'
p3 <- ggplot(data = percentChange, aes(x=lon, y=lat, fill=SSP126_7000)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "viridis", 
  #                      na.value = 'lightblue', direction = -1) +
  scale_fill_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),
                       values = c(0,0.001,0.002,0.003,.03,1)) +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a,
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP585 Early-Century'
p4 <- ggplot(data = percentChange, aes(x=lon, y=lat, fill=SSP585_1040)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "viridis", 
  #                      na.value = 'lightblue', direction = -1) +
  scale_fill_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),
                       values = c(0,0.001,0.002,0.003,.03,1)) +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_1040_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a,
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'Historic & SSP585 Mid-Century'
p5 <- ggplot(data = percentChange, aes(x=lon, y=lat, fill=SSP585_4070)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "viridis", 
  #                      na.value = 'lightblue', direction = -1) +
  scale_fill_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),
                       values = c(0,0.001,0.002,0.003,.03,1)) +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a,
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'Historic & SSP585 Late-Century'
p6 <- ggplot(data = percentChange, aes(x=lon, y=lat, fill=SSP585_7000)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "viridis", 
  #                      na.value = 'lightblue', direction = -1) +
  scale_fill_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),
                       values = c(0,0.001,0.002,0.003,.03,1)) +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a,
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, p2, p3,
                 p4, p5, p6,
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1,1))

title <- ggdraw() + draw_label(paste0("Percent Change in occurrence of ", compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','Per_CHANG_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9.5, dpi = 350, bg='white')


# END ##########################################################################