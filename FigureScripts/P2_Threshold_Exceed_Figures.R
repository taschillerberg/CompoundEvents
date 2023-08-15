# P2_Threshold_Exceed_Figures.R
# About: This script will plot the Thresholds and frequencies of the variables.
# 
# Inputs: THRESHOLD & EXCEED
# Outputs: Frequency plots, database plots
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
# Libraries ###############################################################
library(tidyverse)
library(cowplot)
# library(sf)
# library(ggforce)
require(gridExtra)

# Part I Variables To Change ##############################################
mNum <- 1 # For Part V
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
var <- c('tasmax', 'tasmin', 'pr', 'mrsos') [4]
startyr <- 1980
endyr <- 2010
timeSpan <- c('DAY_','WEEK_FD_','WEEK_D_','MONTH_FD_','MONTH_D_')[1]
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")

print(paste0('Model ',loc2))
print(paste0('Var ', var))
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))

# Part II Functions ############################################################
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

# Part III Threshold ###########################################################
# . 3.1 Opening & Formatting Historical Thresholds -----------------------------
datThreshM1 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[1],
                               'THRESHOLD_',var, mFileH[1],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datThreshM2 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[2],
                               'THRESHOLD_',var, mFileH[2],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datThreshM3 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[3],
                               'THRESHOLD_',var, mFileH[3],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datThreshM4 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[4],
                               'THRESHOLD_',var, mFileH[4],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datThreshM5 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[5],
                               'THRESHOLD_',var, mFileH[5],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datThreshM6 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[6],
                               'THRESHOLD_',var, mFileH[6],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datThreshM7 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[7],
                               'THRESHOLD_',var, mFileH[7],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datThreshM8 <- read_csv(paste0(fileloc1,'CMIP6_historical/',loc2[8],
                               'THRESHOLD_',var, mFileH[8],1980,'-',2010,'.csv'),
                        col_names = TRUE, cols(.default = col_double()))

if (var == 'tasmax'){
  datThresh <- cbind(datThreshM1$lon, datThreshM1$lat, datThreshM1$P95, 
                     datThreshM2$P95, datThreshM3$P95, datThreshM4$P95,
                     datThreshM5$P95, datThreshM6$P95, datThreshM7$P95, 
                     datThreshM8$P95) %>%
    as_tibble()
  colnames(datThresh) <- c('lon', 'lat','CMCC-ESM2', 'EC-Earth3', 'GFDL-ESM4',
                           'INM-CM4-8', 'INM-CM5-0', 'MPI-ESM1-2-HR', 
                           'MRI-ESM2-0', 'NorESM2-MM')
  datTitle <- 'Threshold for Tmax 95th percentile'
  legendTitle <- 'Degree C'
} else if (var == 'tasmin'){
  datThresh <- cbind(datThreshM1$lon, datThreshM1$lat, datThreshM1$P05,
                       datThreshM2$P05, datThreshM3$P05, datThreshM4$P05,
                       datThreshM5$P05, datThreshM6$P05, datThreshM7$P05, 
                       datThreshM8$P05) %>%
    as_tibble() 
  colnames(datThresh) <- c('lon', 'lat','CMCC-ESM2', 'EC-Earth3', 'GFDL-ESM4',
                           'INM-CM4-8', 'INM-CM5-0', 'MPI-ESM1-2-HR', 
                           'MRI-ESM2-0', 'NorESM2-MM')
  datTitle <- 'Threshold for Tmin 5th percentile'
  legendTitle <- 'Degree C'
} else if (var == 'pr'){
  datThresh <- cbind(datThreshM1$lon, datThreshM1$lat, datThreshM1$P99, 
                       datThreshM2$P99, datThreshM3$P99, datThreshM4$P99, 
                       datThreshM5$P99, datThreshM6$P99, datThreshM7$P99,
                       datThreshM8$P99) %>%
    as_tibble() 
  colnames(datThresh) <- c('lon', 'lat','CMCC-ESM2', 'EC-Earth3', 'GFDL-ESM4',
                           'INM-CM4-8', 'INM-CM5-0', 'MPI-ESM1-2-HR', 
                           'MRI-ESM2-0', 'NorESM2-MM')
  datTitle <- 'Threshold for Pr 99th percentile'
  legendTitle <- 'mm/day'
} else if (var == 'mrsos'){
  datThresh <- cbind(datThreshM1$lon, datThreshM1$lat, datThreshM1$P10,
                       datThreshM2$P10, datThreshM3$P10, datThreshM4$P10,
                       datThreshM5$P10, datThreshM6$P10, datThreshM7$P10,
                       datThreshM8$P10) %>%
    as_tibble()
  colnames(datThresh) <- c('lon', 'lat','CMCC-ESM2', 'EC-Earth3', 'GFDL-ESM4',
                           'INM-CM4-8', 'INM-CM5-0', 'MPI-ESM1-2-HR', 
                           'MRI-ESM2-0', 'NorESM2-MM')
  datTitle <- 'Threshold for Soil Moisture 10th percentile'
  legendTitle <- 'kg/m2'
} else {
  print("Var selection not reconized")
}

# . 3.2 Calculating Mu thresholds ----------------------------------------------
datThresh$Mu <- apply(datThresh[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})

# . 3.3 Creating a table of values ---------------------------------------------
sM1 <- summary(datThresh$`CMCC-ESM2`) %>% round(digits = 4)
sM2 <- summary(datThresh$`EC-Earth3`) %>% round(digits = 4)
sM3 <- summary(datThresh$`GFDL-ESM4`) %>% round(digits = 4) 
sM4 <- summary(datThresh$`INM-CM4-8`) %>% round(digits = 4)
sM5 <- summary(datThresh$`INM-CM5-0`) %>% round(digits = 4)
sM6 <- summary(datThresh$`MPI-ESM1-2-HR`) %>% round(digits = 4)
sM7 <- summary(datThresh$`MRI-ESM2-0`) %>% round(digits = 4) 
sM8 <- summary(datThresh$`NorESM2-MM`) %>% round(digits = 4)
sMmu <- summary(datThresh$Mu) %>% round(digits = 4)

sM1 <- rbind(sM1,sM2, sM3, sM4, sM5, sM6, sM7, sM8, sMmu) %>% 
  as_tibble()
colnames(sM1) <- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','NAs')
sM1 <- sM1 %>% select(-NAs)

mMin <- min(sM1$Min.) %>% 
  round(digits = -1)
mMax <- (max(sM1$Max.) + 5) %>%
  round(digits = 0)

# . 3.4 Plotting the thresholds ------------------------------------------------
a <-strsplit(loc2[1],'/') %>% unlist() 
rowM <- c(a)
pM1 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`CMCC-ESM2`)) +
  theme_bw() +
  # theme_void() +
  # geom_tile(data = datAnt, aes(fill = obs), color = 'lightblue') + 
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket", 
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  
  # coord_map(projection = "mollweide", orientation = c(90,0,0)) +
  # labs(title = a, x = "", y = "") +
  # geom_ellipse(aes(x0 = 0, y0 = 0, a = 240, b = 90, angle = 0, alpha = 100)) + 
  # stat_ellipse(mapping = NULL, data = datAnt, level = 1) +
  # stat_circle()
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[2],'/') %>% unlist() 
rowM <- c(rowM, a)
pM2 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`EC-Earth3`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket",
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[3],'/') %>% unlist() 
rowM <- c(rowM, a)
pM3 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`GFDL-ESM4`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket", 
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[4],'/') %>% unlist() 
rowM <- c(rowM, a)
pM4 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`INM-CM4-8`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket", 
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[5],'/') %>% unlist() 
rowM <- c(rowM, a)
pM5 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`INM-CM5-0`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket",
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[6],'/') %>% unlist() 
rowM <- c(rowM, a)
pM6 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`MPI-ESM1-2-HR`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket",
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[7],'/') %>% unlist() 
rowM <- c(rowM, a)
pM7 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`MRI-ESM2-0`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket",
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[8],'/') %>% unlist() 
rowM <- c(rowM, a)
pM8 <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=`NorESM2-MM`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket",
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

rowM <- c(rowM, 'Model Mean')
pMu <- ggplot(data = datThresh, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMin,mMax), option = "rocket", 
                       na.value = 'lightblue', direction = -1, 
                       name = legendTitle) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.2) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = "Model Mean", 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(pM1, position = 'right') %>% 
  as_ggplot()
pM1 <- pM1 + theme(legend.position = "NULL")

p1 <- plot_grid(pM1, pM2, pM3, pM4,
                pM5, pM6, pM7, pM8,
                nrow = 2,
                # labels = c('A','B','C','D'),
                rel_widths = c(1,1))
F1A <- plot_grid(p1,
                 ncol = 1
                 # myLegend,
                 # ncol=2,
                 # rel_widths = c(1,0.07)
                 )
rownames(sM1) <- rowM
F1B <- plot_grid(pMu, gridExtra::tableGrob(sM1),
                 rel_widths = c(1,1),
                 nrow= 1)
title <- ggdraw() + draw_label(datTitle, fontface='bold')
F1 <- plot_grid(title,
                F1A,
                F1B,
                rel_heights = c(.05,.85,.5),
                nrow = 3)

ggsave(F1, filename = paste0(fileloc1,loc1[1],'Results/','THRESHOLD_',var, ".tiff"),
       width = 14, height = 10, dpi = 350, bg='white')


# Part IV Mean Values ##########################################################
# . 4.1 Variables Needed -------------------------------------------------------
datFreqH <- tibble(
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
datFreq126_4070 <- datFreqH
datFreq126_7000 <- datFreqH
datFreq585_4070 <- datFreqH
datFreq585_7000 <- datFreqH

# . 4.2 Opening & Formatting Exceed Files --------------------------------------
# . . 4.2.1 Historical ---------------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[1],loc2[i],'EXCEED_',timeSpan,var,
                         mFileH[i],1980,'-',2010,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of occurrences
  Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datFreqH$lon <- dat$lon
    datFreqH$lat <- dat$lat
    datFreqH[,2+i] <- as_tibble(Xx)
  } else {
    datFreqH[,2+i] <- as_tibble(Xx)
  }
}
datFreqH$Mu <- apply(datFreqH[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})

# . . 4.2.2 SSP126 2040-70 -----------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[2],loc2[i],'EXCEED_',timeSpan,var,
                         mFile126[i],2040,'-',2070,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of occurrences
  Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datFreq126_4070$lon <- dat$lon
    datFreq126_4070$lat <- dat$lat
    datFreq126_4070[,2+i] <- as_tibble(Xx)
  } else {
    datFreq126_4070[,2+i] <- as_tibble(Xx)
  }
}
datFreq126_4070$Mu <- apply(datFreq126_4070[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})

# . . 4.2.3 SSP126 2070-2100 ---------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[2],loc2[i],'EXCEED_',timeSpan,var,
                         mFile126[i],2070,'-',2100,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of occurrences
  Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datFreq126_7000$lon <- dat$lon
    datFreq126_7000$lat <- dat$lat
    datFreq126_7000[,2+i] <- as_tibble(Xx)
  } else {
    datFreq126_7000[,2+i] <- as_tibble(Xx)
  }
}
datFreq126_7000$Mu <- apply(datFreq126_7000[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})

# . . 4.2.4 SSP585 2040-70 -----------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[3],loc2[i],'EXCEED_',timeSpan,var,
                         mFile585[i],2040,'-',2070,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of occurrences
  Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31) # Frequency
  if (i == 1){
    datFreq585_4070$lon <- dat$lon
    datFreq585_4070$lat <- dat$lat
    datFreq585_4070[,2+i] <- as_tibble(Xx)
  } else {
    datFreq585_4070[,2+i] <- as_tibble(Xx)
  }
}
datFreq585_4070$Mu <- apply(datFreq585_4070[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})

# . . 4.2.5 SSP585 2070-2100 ---------------------------------------------------
for (i in 1: length(loc2)){
  dat <- read_csv(paste0(fileloc1,loc1[3],loc2[i],'EXCEED_',timeSpan,var,
                         mFile585[i],2070,'-',2100,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
  Xx <- apply(X = dat[,3:ncol(dat)], MARGIN = 1,
              FUN = sum, na.rm=TRUE)    #Number of occurrences
  Xx <- Xx/(ncol(dat)-2) * ((ncol(dat)-2)/31)  # Frequency
  if (i == 1){
    datFreq585_7000$lon <- dat$lon
    datFreq585_7000$lat <- dat$lat
    datFreq585_7000[,2+i] <- as_tibble(Xx)
  } else {
    datFreq585_7000[,2+i] <- as_tibble(Xx)
  }
}
datFreq585_7000$Mu <- apply(datFreq585_7000[,3:10], MARGIN = 1, function(x){
  sum(x, na.rm = TRUE)/length(x[!is.na(x)])
})

# . 4.3 Creating a table of values ---------------------------------------------
sFH <- summary(datFreqH$Mu)
sFM126 <- summary(datFreq126_4070$Mu)
sFL126 <- summary(datFreq126_7000$Mu)
sFM585 <- summary(datFreq585_4070$Mu)
sFL585 <- summary(datFreq585_7000$Mu)
sF <- rbind(sFH, sFM126, sFL126, sFM585, sFL585) %>% as_tibble()
colnames(sF) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                  'Freq 3rd Qu.','Freq Max.')
sFmin <- min(sF$'Freq Min.')
sFmax <- max(sF$'Freq Max.')

# . 4.4 Plotting the Frequencies -----------------------------------------------
a <- 'Historical'
pM1 <- ggplot(data = datFreqH, aes(x=lon, y=lat, fill=Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(sFmin,sFmax), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Frequency of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- 'SSP126 Mid-Century'
pM2 <- ggplot(data = datFreq126_4070, aes(x=lon, y=lat, fill=Mu)) +
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
pM3 <- ggplot(data = datFreq126_7000, aes(x=lon, y=lat, fill=Mu)) +
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
pM4 <- ggplot(data = datFreq585_4070, aes(x=lon, y=lat, fill=Mu)) +
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
pM5 <- ggplot(data = datFreq585_7000, aes(x=lon, y=lat, fill=Mu)) +
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

F1A <- plot_grid(pM1, myLegend,
                 pM2, pM3, 
                 pM4, pM5, 
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))
rownames(sF) <- c('Historical','SSP126 Mid-Century','SSP126 Late-Century',
                    'SSP585 Mid-Century','SSP585 Late-Century')
F1B <- plot_grid(gridExtra::tableGrob(sF),
                 rel_widths = c(1),
                 nrow= 1)
title <- ggdraw() + draw_label(paste0("Frequency of ", var), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                F1B,
                rel_heights = c(.05,1,.4),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste0(fileloc1,'Results/','EXCEED_', timeSpan ,'AllFrequency_',var, ".tiff"),
       width = 14, height = 16, dpi = 350, bg='white')


# Part V Models Exceed #########################################################
# . 5.1 Variables Needed -------------------------------------------------------
dat <- c(datFreqH, datFreq126_4070, datFreq126_7000, 
         datFreq585_4070, datFreq585_7000)
dat <- datFreq585_7000
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')[3]

# . 5.2 Creating a table of values ---------------------------------------------
sM1f <- summary(dat$`CMCC-ESM2`) %>% round(digits=4)
sM2f <- summary(dat$`EC-Earth3`) %>% round(digits=4)
sM3f <- summary(dat$`GFDL-ESM4`) %>% round(digits=4) 
sM4f <- summary(dat$`INM-CM4-8`) %>% round(digits=4)
sM5f <- summary(dat$`INM-CM5-0`) %>% round(digits=4)
sM6f <- summary(dat$`MPI-ESM1-2-HR`) %>% round(digits=4)
sM7f <- summary(dat$`MRI-ESM2-0`) %>% round(digits=4) 
sM8f <- summary(dat$`NorESM2-MM`) %>% round(digits=4)
sM1f <- rbind(sM1f,sM2f, sM3f, sM4f, sM5f, sM6f, sM7f, sM8f) %>% as.tibble()
colnames(sM1f) <- c('Freq Min.','Freq 1st Qu.','Freq Median','Freq Mean',
                    'Freq 3rd Qu.','Freq Max.')
mMinf <- min(sM1f$'Freq Min.') - 0.0001
mMaxf <- max(sM1f$'Freq Max.') + 0.0001

# . 5.3 Plotting the frequency or occurrences ----------------------------------
rm(rowM)
a <-strsplit(loc2[1],'/') %>% unlist() 
rowM <- c(a)
pM1 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`CMCC-ESM2`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[2],'/') %>% unlist() 
rowM <- c(rowM, a)
pM2 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`EC-Earth3`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[3],'/') %>% unlist() 
rowM <- c(rowM, a)
pM3 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`GFDL-ESM4`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[4],'/') %>% unlist() 
rowM <- c(rowM, a)
pM4 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`INM-CM4-8`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[5],'/') %>% unlist() 
rowM <- c(rowM, a)
pM5 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`INM-CM5-0`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[6],'/') %>% unlist() 
rowM <- c(rowM, a)
pM6 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`MPI-ESM1-2-HR`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[7],'/') %>% unlist() 
rowM <- c(rowM, a)
pM7 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`MRI-ESM2-0`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
                       direction = -1) +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <-strsplit(loc2[8],'/') %>% unlist() 
rowM <- c(rowM, a)
pM8 <- ggplot(data = dat, aes(x=lon, y=lat, fill=`NorESM2-MM`)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(mMinf,mMaxf), option = "rocket", na.value = 'lightblue',
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

p1 <- plot_grid(pM1, pM2, pM3, pM4, pM5, pM6, pM7, pM8,
                nrow = 2,
                # labels = c('A','B','C','D'),
                rel_widths = c(1,1))
F1A <- plot_grid(p1,
                 myLegend,
                 ncol=2,
                 rel_widths = c(1,0.07))
rownames(sM1f) <- rowM
F1B <- plot_grid(gridExtra::tableGrob(sM1f),
                 rel_widths = c(1),
                 nrow= 1)
F1 <- plot_grid(F1A,
                F1B,
                rel_heights = c(1,.5),
                nrow = 2)

ggsave(F1, filename = paste0(fileloc1,loc1,'Results/','EXCEED_DAY_Frequency_',var, "_7000.tiff"),
       width = 14, height = 10, dpi = 350, bg='white')

# rm(list=ls()[! ls() %in% c('as_ggplot','get_legend','fileloc1', 'loc1', 'loc2', 'mFile','mNum','var',
#                            'startyr','endyr','baseData')])

# END #########################################################################