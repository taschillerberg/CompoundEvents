# P4_Occurrences_Figures.R
# About: This script will plot the wave frequencies of the variables.
# 
# Inputs: OCC_DAY, OCCYr_DAY, OCCMo_DAY
# Outputs: (3.1) - OCC_CHANG_#.csv, OCC_Mu_DAY_#, OCC_CHANGE_DAY_# 
#          (3.2) - OCC_CHANGE_DAY_#_Region#
#          (4.1) - OCCYr_TIMESER_DAY_Region#
#
# T. A. Schillerberg
#               Oct. 2022
#      Updated: Oct. 2023
# ------------------------------------------------------------------------------

# Computer
setwd("Source File Location") 
fileloc1 <- 'Main project folder' 

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)
library(data.table)
library(car)

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
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')
timeSpan <- c('DAY_', 'WEEK_', 'WEEK_FD_','WEEK_D_', 
              'MONTH_', 'MONTH_FD_','MONTH_D_')[1]

locTitle <- c('Oceania', 'South America', 'China', 'North America', 'Europe') # 'Southeast Asia',
lon1 <- c( 93, -82,  94, -125, -10) # 67,
lon2 <- c(153, -34, 124,  -66,  50) # 92,
lat1 <- c( 10,  13,  45,   55,  56) # 30,
lat2 <- c(-10, -35,  20,   23,  36) # 5, 
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")

print(paste0('Model ',loc2))
print(paste0('Var ', var))
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
mean_cl_quantile <- function(x, q = c(0.05, 0.95), na.rm = TRUE){
  dat <- data.frame(y = mean(x, na.rm = na.rm),
                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
  return(dat)
}
acf2 <- function(x){
  dat <- x[1:4]
  y <- acf(dat, plot = FALSE)$acf
  dat <- c(x[1], x[5:7])
  y2 <- acf(dat, plot = FALSE)$acf
  return(c(y,y2))
}
splitMod <- function(dat, position){
  dat <- as.character(dat)
  strsplit(dat,'_')[[1]][position]
}

# Part III Period Occurrences ##################################################
# . 3.1 Global Maps ------------------------------------------------------------
# . . 3.1.1 Variables Needed ---------------------------------------------------
varNum <- 1
var <- c('tasmax', 'tasmin', 'pr', 'mrsos') [varNum]
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')[varNum]
relative <- tibble(
  'lon' = numeric(length = 12476),
  'lat' = numeric(length = 12476),
  'Historical_Mu' = numeric(length = 12476),
  'Historical_Var' = numeric(length = 12476),
  'SSP126_1040_Mu' = numeric(length = 12476),
  'SSP126_1040_Var' = numeric(length = 12476),
  'SSP126_1040_Delta' = numeric(length = 12476),
  'SSP126_1040_Sig' = numeric(length = 12476),
  'SSP126_4070_Mu' = numeric(length = 12476),
  'SSP126_4070_Var' = numeric(length = 12476),
  'SSP126_4070_Delta' = numeric(length = 12476),
  'SSP126_4070_Sig' = numeric(length = 12476),
  'SSP126_7000_Mu' = numeric(length = 12476),
  'SSP126_7000_Var' = numeric(length = 12476),
  'SSP126_7000_Delta' = numeric(length = 12476),
  'SSP126_7000_Sig' = numeric(length = 12476),
  
  'SSP585_1040_Mu' = numeric(length = 12476),
  'SSP585_1040_Var' = numeric(length = 12476),
  'SSP585_1040_Delta' = numeric(length = 12476),
  'SSP585_1040_Sig' = numeric(length = 12476),
  'SSP585_4070_Mu' = numeric(length = 12476),
  'SSP585_4070_Var' = numeric(length = 12476),
  'SSP585_4070_Delta' = numeric(length = 12476),
  'SSP585_4070_Sig' = numeric(length = 12476),
  'SSP585_7000_Mu' = numeric(length = 12476),
  'SSP585_7000_Var' = numeric(length = 12476),
  'SSP585_7000_Delta' = numeric(length = 12476),
  'SSP585_7000_Sig' = numeric(length = 12476)
) 
# Mu : average/mean   
# Var : variance
# Delta : difference with historic period  
# Sig : Significance
dat <- tibble(
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
  'Mu' = numeric(length = 12476),
  "Positive" = numeric(length = 12476),
  "Negative" = numeric(length = 12476)
)

# . . 3.1.2 Opening Variables --------------------------------------------------
datOccH <- read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                           'OCC_DAY_', var, '_Hist_8010','.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datOcc126_1040 <- read_csv(paste0(fileloc1, loc1[2], 'Results/', 
                                  'OCC_DAY_', var, '_SSP126_1040','.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datOcc126_4070 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                  'OCC_DAY_', var, '_SSP126_4070','.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datOcc126_7000 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                  'OCC_DAY_', var, '_SSP126_7000','.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datOcc585_1040 <- read_csv(paste0(fileloc1, loc1[3], 'Results/', 
                                  'OCC_DAY_', var, '_SSP585_1040','.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datOcc585_4070 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                  'OCC_DAY_', var, '_SSP585_4070','.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datOcc585_7000 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                  'OCC_DAY_', var, '_SSP585_7000','.csv'),
                           col_names = TRUE, cols(.default = col_double()))

# . . 3.1.3 Calculate the Significance of Change -------------------------------
# . . . 3.3.2.1 Historical - - - - - - - - - - - - - - - - - - - - - - - - - - -
relative$lon <- datOccH$lon
relative$lat <- datOccH$lat
relative$Historical_Mu <- datOccH$Mu
relative$Historical_Var <- apply(datOccH[,3:10], MARGIN = 1, FUN=stats::var)
# . . . 3.3.2.2 SSP126_1040 - - - - - - - - - - - - - - - - - - - - - - - - -
# . . . . 3.3.2.2.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datOcc126_1040[,2+i] - datOccH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relative$SSP126_1040_Delta <- dat$Mu
relative$SSP126_1040_Mu <- datOcc126_1040$Mu
relative$SSP126_1040_Var <- apply(datOcc126_1040[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 3.3.2.2.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relative$SSP126_1040_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 3.3.2.3 SSP126_4070 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 3.3.2.3.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datOcc126_4070[,2+i] - datOccH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relative$SSP126_4070_Delta <- dat$Mu
relative$SSP126_4070_Mu <- datOcc126_4070$Mu
relative$SSP126_4070_Var <- apply(datOcc126_4070[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 3.3.2.3.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relative$SSP126_4070_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1
# . . . 3.3.2.4 SSP126_7000 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 3.3.2.4.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datOcc126_7000[,2+i] - datOccH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relative$SSP126_7000_Delta <- dat$Mu
relative$SSP126_7000_Mu <- datOcc126_7000$Mu
relative$SSP126_7000_Var <- apply(datOcc126_7000[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 3.3.2.4.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relative$SSP126_7000_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 3.3.2.5 SSP585_1040 - - - - - - - - - - - - - - - - - - - - - - - - -
# . . . . 3.3.2.5.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datOcc585_1040[,2+i] - datOccH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relative$SSP585_1040_Delta <- dat$Mu
relative$SSP585_1040_Mu <- datOcc585_1040$Mu
relative$SSP585_1040_Var <- apply(datOcc585_1040[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 3.3.2.5.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relative$SSP585_1040_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 3.3.2.6 SSP585_4070 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 3.3.2.6.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datOcc585_4070[,2+i] - datOccH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relative$SSP585_4070_Delta <- dat$Mu
relative$SSP585_4070_Mu <- datOcc585_4070$Mu
relative$SSP585_4070_Var <- apply(datOcc585_4070[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 3.3.2.6.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relative$SSP585_4070_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 3.3.2.7 SSP585_7000 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 3.3.2.7.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datOcc585_7000[,2+i] - datOccH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
relative$SSP585_7000_Delta <- dat$Mu
relative$SSP585_7000_Mu <- datOcc585_7000$Mu
relative$SSP585_7000_Var <- apply(datOcc585_7000[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 3.3.2.7.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
relative$SSP585_7000_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . 3.3.2.8 Saving - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
write.csv(relative,file=paste0(fileloc1,'Results/','OCC_CHANG_',var,'.csv'),
          row.names = FALSE)

# . . 3.1.4 Plotting Limits ----------------------------------------------------
relative <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var,'.csv'), 
                     col_names = TRUE, cols(.default = col_double()))
maxLimt <- cbind(relative$Historical_Mu, 
                 relative$SSP126_1040_Mu, relative$SSP126_4070_Mu, 
                 relative$SSP126_7000_Mu, relative$SSP585_1040_Mu, 
                 relative$SSP585_4070_Mu, relative$SSP585_7000_Mu) %>%
  max()
minLimt <- cbind(relative$Historical_Mu, 
                 relative$SSP126_1040_Mu, relative$SSP126_4070_Mu, 
                 relative$SSP126_7000_Mu, relative$SSP585_1040_Mu, 
                 relative$SSP585_4070_Mu, relative$SSP585_7000_Mu) %>%
  min()

# . . 3.1.5 Plotting -----------------------------------------------------------
a <- "Historical"
p1 <- ggplot(data = relative, aes(x=lon, y=lat, fill=Historical_Mu)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Number of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Early-Century"
p2 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP126_1040_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Number of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Mid-Century"
p3 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP126_4070_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Number of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Late-Century"
p4 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP126_7000_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Number of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Early-Century"
p5 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP585_1040_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Number of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Mid-Century"
p6 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP585_4070_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Number of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Late-Century"
p7 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP585_7000_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Number of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'right') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, myLegend, NULL,
                 p2, p3, p4,
                 p4, p5, p6,
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))
title <- ggdraw() + draw_label(paste0("Number of ", varT, " occurances"),
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                # rel_heights = c(0.05,1),
                nrow = 2)
ggsave(F1, filename = paste0(fileloc1,'Results/','OCC_Mu_', timeSpan, var,
                             ".tiff"),
       width = 14, height = 10, dpi = 350, bg='white')

# . . 3.1.6 Plotting Change Limits ---------------------------------------------
maxLimt <- rbind(relative$SSP126_1040_Delta, relative$SSP126_4070_Delta, 
                 relative$SSP126_7000_Delta, relative$SSP585_1040_Delta, 
                 relative$SSP585_4070_Delta, relative$SSP585_7000_Delta) %>%
  max()
minLimt <- rbind(relative$SSP126_1040_Delta, relative$SSP126_4070_Delta, 
                 relative$SSP126_7000_Delta, relative$SSP585_1040_Delta, 
                 relative$SSP585_4070_Delta, relative$SSP585_7000_Delta) %>%
  min()

# . . 3.1.7 Plotting Change ----------------------------------------------------
a <- "SSP126 Early-Century - Historical"
p1 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP126_1040_Delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_1070_Sig == 0,'dot', 'no_dot'))) +
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

a <- "SSP126 Mid-Century - Historical"
p2 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP126_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Late-Century - Historical"
p3 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP126_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Early-Century - Historical"
p4 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP585_1040_Delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Mid-Century - Historical"
p5 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP585_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Late-Century - Historical"
p6 <- ggplot(data = relative, aes(x=lon, y=lat, fill=SSP585_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

myLegend <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1A <- plot_grid(p1, p4,
                 p2, p5,
                 p3, p6, 
                 nrow = 3,
                 # labels = c('A','B','C','D'),
                 rel_heights = c(1,1, 1))
title <- ggdraw() + draw_label(paste0("Change of ", varT), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                # rel_heights = c(.05,1,.4),
                rel_heights = c(0.05,1, 0.05),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/','OCC_CHANGE_', timeSpan, var,
                             ".tiff"),
       width = 14, height = 13, dpi = 350, bg='white')
# . . 3.1.7 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'lat1', 'lat2','lon1','lon2',
                           'location','locTitle','mFileH','mFile126','mFile585',
                           'var', 'varT', 'baseData', 'timeSpan',
                           'get_legend','as_ggplot','mean_cl_quantile','acf2', 
                           'splitMod')])

# . 3.2 Regional Change p-value ------------------------------------------------
# . . 3.2.1 Variables needed ---------------------------------------------------
location <- 1 # 1-5
varNum <- 1 # 1,3,4
var <- c('tasmax', 'tasmin', 'pr', 'mrsos') [varNum]
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')[varNum]
relative <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var,'.csv'), 
                     col_names = TRUE, cols(.default = col_double()))
stats <- tibble(
  'Rows' = numeric(length = 7),
  'Hist_8010' = numeric(length = 7),
  'SSP126_1040' = numeric(length = 7),
  'SSP126_4070' = numeric(length = 7),
  'SSP126_7000' = numeric(length = 7),
  'SSP585_1040' = numeric(length = 7),
  'SSP585_4070' = numeric(length = 7),
  'SSP585_7000' = numeric(length = 7)
)
stats$Rows <- c('AutoCorr Max', 'AutoCorr Min', 'Normality', 'Variance Ratio',
                'Variance', 'T-test', 'Wilcox Test')
lonA <- lon1[location]
lonB <- lon2[location]
latA <- lat1[location]
latB <- lat2[location]

# . . 3.2.2 Statistical --------------------------------------------------------
datRelative <- relative
datRelative$lon[datRelative$lon < lonA | 
                  datRelative$lon > lonB] <- NA
datRelative$lat[datRelative$lat > latA | 
                  datRelative$lat < latB] <- NA
datRelative <- na.omit(datRelative)

# Testing assumptions
# Independence - Autocorrelation  
dat <- cbind(datRelative$Historical_Mu, datRelative$SSP126_1040_Mu, 
             datRelative$SSP126_4070_Mu, datRelative$SSP126_7000_Mu,
             datRelative$SSP585_1040_Mu, datRelative$SSP585_4070_Mu, 
             datRelative$SSP585_7000_Mu)
autoCorr <- apply(dat, MARGIN = 1, FUN = acf2) %>%
  t()
stats$SSP126_1040[1] <- max(autoCorr[,2]) %>% round(digits = 4)
stats$SSP126_4070[1] <- max(autoCorr[,3]) %>% round(digits = 4)
stats$SSP126_7000[1] <- max(autoCorr[,4]) %>% round(digits = 4)
stats$SSP585_1040[1] <- max(autoCorr[,6]) %>% round(digits = 4)
stats$SSP585_4070[1] <- max(autoCorr[,7]) %>% round(digits = 4)
stats$SSP585_7000[1] <- max(autoCorr[,8]) %>% round(digits = 4)
stats$SSP126_1040[2] <- min(autoCorr[,2]) %>% round(digits = 4)
stats$SSP126_4070[2] <- min(autoCorr[,3]) %>% round(digits = 4)
stats$SSP126_7000[2] <- min(autoCorr[,4]) %>% round(digits = 4)
stats$SSP585_1040[2] <- min(autoCorr[,6]) %>% round(digits = 4)
stats$SSP585_4070[2] <- min(autoCorr[,7]) %>% round(digits = 4)
stats$SSP585_7000[2] <- min(autoCorr[,8]) %>% round(digits = 4)
rm(autoCorr)

# Normally Distributions
# The null hypothesis of these tests is that “sample distribution is normal”. 
# If the test is significant, the distribution is non-normal.
stats$Hist_8010[3] <- shapiro.test(datRelative$Historical_Mu)$p.value
stats$SSP126_1040[3] <- shapiro.test(datRelative$SSP126_1040_Mu)$p.value
stats$SSP126_4070[3] <- shapiro.test(datRelative$SSP126_4070_Mu)$p.value
stats$SSP126_7000[3] <- shapiro.test(datRelative$SSP126_7000_Mu)$p.value
stats$SSP585_1040[3] <- shapiro.test(datRelative$SSP585_1040_Mu)$p.value
stats$SSP585_4070[3] <- shapiro.test(datRelative$SSP585_4070_Mu)$p.value
stats$SSP585_7000[3] <- shapiro.test(datRelative$SSP585_7000_Mu)$p.value

plot(density(datRelative$Historical_Mu))
plot(density(datRelative$SSP126_1040_Mu))
plot(density(datRelative$SSP126_4070_Mu))
plot(density(datRelative$SSP126_7000_Mu))
plot(density(datRelative$SSP585_1040_Mu))
plot(density(datRelative$SSP585_4070_Mu))
plot(density(datRelative$SSP585_7000_Mu))

if(stats$Hist_8010[3] <= 0.05){stats$Hist_8010[3] <- 'Non-Normal' } else {stats$Hist_8010[3] <- 'Normal'}
if(stats$SSP126_1040[3] <= 0.05){stats$SSP126_1040[3] <- 'Non-Normal' } else {stats$SSP126_1040[3] <- 'Normal'}
if(stats$SSP126_4070[3] <= 0.05){stats$SSP126_4070[3] <- 'Non-Normal' } else {stats$SSP126_4070[3] <- 'Normal'}
if(stats$SSP126_7000[3] <= 0.05){stats$SSP126_7000[3] <- 'Non-Normal' } else {stats$SSP126_7000[3] <- 'Normal'}
if(stats$SSP585_1040[3] <= 0.05){stats$SSP585_1040[3] <- 'Non-Normal' } else {stats$SSP585_1040[3] <- 'Normal'}
if(stats$SSP585_4070[3] <= 0.05){stats$SSP585_4070[3] <- 'Non-Normal' } else {stats$SSP585_4070[3] <- 'Normal'}
if(stats$SSP585_7000[3] <= 0.05){stats$SSP585_7000[3] <- 'Non-Normal' } else {stats$SSP585_7000[3] <- 'Normal'}

# Similar Variance
# true ratio of variances is not equal to 1
# Note that, the more this ratio deviates from 1, the stronger the evidence for 
# unequal population variances.
stats$SSP126_1040[4] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP126_1040_Mu, 
                                 alternative = "two.sided")$statistic %>% 
  round(digits = 4)
stats$SSP126_1040[5] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP126_1040_Mu, 
                                 alternative = "two.sided")$p.value
# 126_4070
stats$SSP126_4070[4] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP126_4070_Mu, 
                                 alternative = "two.sided")$statistic %>% 
  round(digits = 4)
stats$SSP126_4070[5] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP126_4070_Mu, 
                                 alternative = "two.sided")$p.value
# 126_7000
stats$SSP126_7000[4] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP126_7000_Mu, 
                                 alternative = "two.sided")$statistic %>% 
  round(digits = 4)
stats$SSP126_7000[5] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP126_7000_Mu, 
                                 alternative = "two.sided")$p.value
# 585 1040
stats$SSP585_1040[4] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP585_1040_Mu, 
                                 alternative = "two.sided")$statistic %>% 
  round(digits = 4)
stats$SSP585_1040[5] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP585_1040_Mu, 
                                 alternative = "two.sided")$p.value
# 585 4070
stats$SSP585_4070[4] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP585_4070_Mu, 
                                 alternative = "two.sided")$statistic %>% 
  round(digits = 4)
stats$SSP585_4070[5] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP585_4070_Mu, 
                                 alternative = "two.sided")$p.value
# 585 7000
stats$SSP585_7000[4] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP585_7000_Mu, 
                                 alternative = "two.sided")$statistic %>% 
  round(digits = 4)
stats$SSP585_7000[5] <- var.test(datRelative$Historical_Mu, 
                                 datRelative$SSP585_7000_Mu, 
                                 alternative = "two.sided")$p.value
if(as.numeric(stats$SSP126_1040[5]) <= 0.05){stats$SSP126_1040[5] <- 'Not-equal' } else {stats$SSP126_1040[5] <- 'Equal Var.'}
if(as.numeric(stats$SSP126_4070[5]) <= 0.05){stats$SSP126_4070[5] <- 'Not-equal' } else {stats$SSP126_4070[5] <- 'Equal Var.'}
if(as.numeric(stats$SSP126_7000[5]) <= 0.05){stats$SSP126_7000[5] <- 'Not-equal' } else {stats$SSP126_7000[5] <- 'Equal Var.'}
if(as.numeric(stats$SSP585_1040[5]) <= 0.05){stats$SSP585_1040[5] <- 'Not-equal' } else {stats$SSP585_1040[5] <- 'Equal Var.'}
if(as.numeric(stats$SSP585_4070[5]) <= 0.05){stats$SSP585_4070[5] <- 'Not-equal' } else {stats$SSP585_4070[5] <- 'Equal Var.'}
if(as.numeric(stats$SSP585_7000[5]) <= 0.05){stats$SSP585_7000[5] <- 'Not-equal' } else {stats$SSP585_7000[5] <- 'Equal Var.'}

# Regular t-test significance
stats$SSP126_1040[6] <- t.test(datRelative$Historical_Mu,
                               datRelative$SSP126_1040_Mu, var.equal = FALSE, 
                               na.rm = TRUE)$p.value
stats$SSP126_4070[6] <- t.test(datRelative$Historical_Mu,
                               datRelative$SSP126_4070_Mu, var.equal = FALSE, 
                               na.rm = TRUE)$p.value
stats$SSP126_7000[6] <- t.test(datRelative$Historical_Mu,
                               datRelative$SSP126_7000_Mu, var.equal = FALSE, 
                               na.rm = TRUE)$p.value
stats$SSP585_1040[6] <- t.test(datRelative$Historical_Mu,
                               datRelative$SSP585_1040_Mu, var.equal = FALSE, 
                               na.rm = TRUE)$p.value
stats$SSP585_4070[6] <- t.test(datRelative$Historical_Mu,
                               datRelative$SSP585_4070_Mu, var.equal = FALSE, 
                               na.rm = TRUE)$p.value
stats$SSP585_7000[6] <- t.test(datRelative$Historical_Mu,
                               datRelative$SSP585_7000_Mu, var.equal = FALSE, 
                               na.rm = TRUE)$p.value
if(as.numeric(stats$SSP126_1040[6]) <= 0.05){stats$SSP126_1040[6] <- 'Diff. Mu' } else {stats$SSP126_1040[6] <- 'Mu=Mu'}
if(as.numeric(stats$SSP126_4070[6]) <= 0.05){stats$SSP126_4070[6] <- 'Diff. Mu' } else {stats$SSP126_4070[6] <- 'Mu=Mu'}
if(as.numeric(stats$SSP126_7000[6]) <= 0.05){stats$SSP126_7000[6] <- 'Diff. Mu' } else {stats$SSP126_7000[6] <- 'Mu=Mu'}
if(as.numeric(stats$SSP585_1040[6]) <= 0.05){stats$SSP585_1040[6] <- 'Diff. Mu' } else {stats$SSP585_1040[6] <- 'Mu=Mu'}
if(as.numeric(stats$SSP585_4070[6]) <= 0.05){stats$SSP585_4070[6] <- 'Diff. Mu' } else {stats$SSP585_4070[6] <- 'Mu=Mu'}
if(as.numeric(stats$SSP585_7000[6]) <= 0.05){stats$SSP585_7000[6] <- 'Diff. Mu' } else {stats$SSP585_7000[6] <- 'Mu=Mu'}

# non-parametric significance
stats$SSP126_1040[7] <- wilcox.test(datRelative$Historical_Mu, 
                                    datRelative$SSP126_1040_Mu, na.rm = TRUE,
                                    exact = FALSE, conf.int =TRUE)$p.value
stats$SSP126_4070[7] <- wilcox.test(datRelative$Historical_Mu, 
                                    datRelative$SSP126_4070_Mu, na.rm = TRUE,
                                    exact = FALSE, conf.int =TRUE)$p.value
stats$SSP126_7000[7] <- wilcox.test(datRelative$Historical_Mu, 
                                    datRelative$SSP126_7000_Mu, na.rm = TRUE,
                                    exact = FALSE, conf.int =TRUE)$p.value
stats$SSP585_1040[7] <- wilcox.test(datRelative$Historical_Mu, 
                                    datRelative$SSP585_1040_Mu, na.rm = TRUE,
                                    exact = FALSE, conf.int =TRUE)$p.value
stats$SSP585_4070[7] <- wilcox.test(datRelative$Historical_Mu, 
                                    datRelative$SSP585_4070_Mu, na.rm = TRUE,
                                    exact = FALSE, conf.int =TRUE)$p.value
stats$SSP585_7000[7] <- wilcox.test(datRelative$Historical_Mu, 
                                    datRelative$SSP585_7000_Mu, na.rm = TRUE,
                                    exact = FALSE, conf.int =TRUE)$p.value
if(as.numeric(stats$SSP126_1040[7]) <= 0.05){stats$SSP126_1040[7] <- 'Diff. Dist.' } else {stats$SSP126_1040[7] <- 'Same Dist.'}
if(as.numeric(stats$SSP126_4070[7]) <= 0.05){stats$SSP126_4070[7] <- 'Diff. Dist.' } else {stats$SSP126_4070[7] <- 'Same Dist.'}
if(as.numeric(stats$SSP126_7000[7]) <= 0.05){stats$SSP126_7000[7] <- 'Diff. Dist.' } else {stats$SSP126_7000[7] <- 'Same Dist.'}
if(as.numeric(stats$SSP585_1040[7]) <= 0.05){stats$SSP585_1040[7] <- 'Diff. Dist.' } else {stats$SSP585_1040[7] <- 'Same Dist.'}
if(as.numeric(stats$SSP585_4070[7]) <= 0.05){stats$SSP585_4070[7] <- 'Diff. Dist.' } else {stats$SSP585_4070[7] <- 'Same Dist.'}
if(as.numeric(stats$SSP585_7000[7]) <= 0.05){stats$SSP585_7000[7] <- 'Diff. Dist.' } else {stats$SSP585_7000[7] <- 'Same Dist.'}

stats <- column_to_rownames(stats, var = 'Rows')

# . . 3.2.3 Plotting -----------------------------------------------------------
# . . . 3.2.3.1 Plotting Limits - - - 
maxLimt <- max(datRelative$Historical_Mu)
minLimt <- min(datRelative$Historical_Mu)

# . . . 3.2.3.2 Ploting - - -
dat <- cbind('Historical',datRelative$Historical_Mu, 'Historical') %>%
  rbind(cbind('Early-Century', datRelative$SSP126_1040_Mu, 'SSP126')) %>%
  rbind(cbind('Mid-Century',   datRelative$SSP126_4070_Mu, 'SSP126')) %>%
  rbind(cbind('Late-Century',  datRelative$SSP126_7000_Mu, 'SSP126')) %>%
  rbind(cbind('Early-Century', datRelative$SSP585_1040_Mu, 'SSP585')) %>%
  rbind(cbind('Mid-Century',   datRelative$SSP585_4070_Mu, 'SSP585')) %>%
  rbind(cbind('Late-Century',  datRelative$SSP585_7000_Mu, 'SSP585'))
dat <- as_tibble(dat)
colnames(dat) <- c('Time Period', 'Value','Scenario')
dat$`Time Period` <- factor(dat$`Time Period`, 
                            levels = c('Historical','Early-Century','Mid-Century','Late-Century'))

p1 <- plot_grid(gridExtra::tableGrob(stats))

p2 <- ggplot(dat, aes(x = `Time Period`, y = as.numeric(Value), fill = Scenario)) +
  theme_bw() +
  geom_boxplot() +
  ylab(label = 'Number of Events')
myLegend2 <- get_legend(p2, position = 'right') %>% 
  as_ggplot()
p2 <- p2 + theme(legend.position = "NULL")

a <- 'Historical'
p3 <- ggplot(data = datRelative, aes(x=lon, y=lat, fill=Historical_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Frequency of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(lonA,lonB), ylim=c(latB,latA), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="right") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))
myLegend3 <- get_legend(p3, position = 'right') %>% 
  as_ggplot()
p3 <- p3 + theme(legend.position = "NULL")

title <- ggdraw() + draw_label(paste0("Change of ", varT, ' for ', locTitle[location]), fontface='bold')
F1A <- plot_grid(myLegend2, 
                 myLegend3,
                 nrow = 2,
                 rel_heights = c(1,1))
F1B <- plot_grid(p2, F1A, p3,
                 nrow = 1,
                 rel_widths = c(1,0.4,1))
F1 <- plot_grid(title,
                p1,
                F1B,
                nrow = 3,
                rel_heights = c(0.05,0.75,1))
ggsave(F1, filename = paste0(fileloc1,'Results/','OCC_CHANGE_', timeSpan, var,
                             '_Region',location, ".tiff"),
       width = 12, height = 7, dpi = 350, bg='white')

# . . 3.2.4 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'lat1', 'lat2','lon1','lon2',
                           'location','locTitle','mFileH','mFile126','mFile585',
                           'var', 'varT', 'baseData', 'timeSpan',
                           'get_legend','as_ggplot','mean_cl_quantile','acf2', 
                           'splitMod')])
# Part IV Annual Occurrences ###################################################
# . 4.1 Regional Time Series ---------------------------------------------------
# . . 4.1.1 Variables Needed ---------------------------------------------------
location <- 5 # 1-5
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')
lonA <- lon1[location]
lonB <- lon2[location]
latA <- lat1[location]
latB <- lat2[location]

# . . 4.1.2 Opening Files Needed -----------------------------------------------
datOccHYr_V1 <- read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                'OCCYr_DAY_', var[1],'_Hist_8010', '_full.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_1040', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_4070', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_7000', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_1040', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_4070', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_7000', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 3
datOccHYr_V3 <- read_csv(paste0(fileloc1, loc1[1], 'Results/',
                                'OCCYr_DAY_', var[3],'_Hist_8010', '_full.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_1040', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_4070', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_7000', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_1040', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_4070', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_7000', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 4
datOccHYr_V4 <- read_csv(paste0(fileloc1, loc1[1], 'Results/',
                                'OCCYr_DAY_', var[4],'_Hist_8010', '_full.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP126_1040', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_4070', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_7000', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP585_1040', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_4070', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_7000', '_full.csv'),
                                col_names = TRUE, cols(.default = col_double()))

# . . 4.1.3 Defining locations -------------------------------------------------
datOccHYr_V1$lon[datOccHYr_V1$lon < lonA | datOccHYr_V1$lon > lonB] <- NA
datOccHYr_V1$lat[datOccHYr_V1$lat > latA | datOccHYr_V1$lat < latB] <- NA
datOcc126_1040Yr_V1$lon[datOcc126_1040Yr_V1$lon < lonA | datOcc126_1040Yr_V1$lon > lonB] <- NA
datOcc126_1040Yr_V1$lat[datOcc126_1040Yr_V1$lat > latA | datOcc126_1040Yr_V1$lat < latB] <- NA
datOcc126_4070Yr_V1$lon[datOcc126_4070Yr_V1$lon < lonA | datOcc126_4070Yr_V1$lon > lonB] <- NA
datOcc126_4070Yr_V1$lat[datOcc126_4070Yr_V1$lat > latA | datOcc126_4070Yr_V1$lat < latB] <- NA
datOcc126_7000Yr_V1$lon[datOcc126_7000Yr_V1$lon < lonA | datOcc126_7000Yr_V1$lon > lonB] <- NA
datOcc126_7000Yr_V1$lat[datOcc126_7000Yr_V1$lat > latA | datOcc126_7000Yr_V1$lat < latB] <- NA
datOcc585_1040Yr_V1$lon[datOcc585_1040Yr_V1$lon < lonA | datOcc585_1040Yr_V1$lon > lonB] <- NA
datOcc585_1040Yr_V1$lat[datOcc585_1040Yr_V1$lat > latA | datOcc585_1040Yr_V1$lat < latB] <- NA
datOcc585_4070Yr_V1$lon[datOcc585_4070Yr_V1$lon < lonA | datOcc585_4070Yr_V1$lon > lonB] <- NA
datOcc585_4070Yr_V1$lat[datOcc585_4070Yr_V1$lat > latA | datOcc585_4070Yr_V1$lat < latB] <- NA
datOcc585_7000Yr_V1$lon[datOcc585_7000Yr_V1$lon < lonA | datOcc585_7000Yr_V1$lon > lonB] <- NA
datOcc585_7000Yr_V1$lat[datOcc585_7000Yr_V1$lat > latA | datOcc585_7000Yr_V1$lat < latB] <- NA
datOccHYr_V1 <- na.omit(datOccHYr_V1)
datOcc126_1040Yr_V1 <- na.omit(datOcc126_1040Yr_V1)
datOcc126_4070Yr_V1 <- na.omit(datOcc126_4070Yr_V1)
datOcc126_7000Yr_V1 <- na.omit(datOcc126_7000Yr_V1)
datOcc585_1040Yr_V1 <- na.omit(datOcc585_1040Yr_V1)
datOcc585_4070Yr_V1 <- na.omit(datOcc585_4070Yr_V1)
datOcc585_7000Yr_V1 <- na.omit(datOcc585_7000Yr_V1)

datOccHYr_V3$lon[datOccHYr_V3$lon < lonA | datOccHYr_V3$lon > lonB] <- NA
datOccHYr_V3$lat[datOccHYr_V3$lat > latA | datOccHYr_V3$lat < latB] <- NA
datOcc126_1040Yr_V3$lon[datOcc126_1040Yr_V3$lon < lonA | datOcc126_1040Yr_V3$lon > lonB] <- NA
datOcc126_1040Yr_V3$lat[datOcc126_1040Yr_V3$lat > latA | datOcc126_1040Yr_V3$lat < latB] <- NA
datOcc126_4070Yr_V3$lon[datOcc126_4070Yr_V3$lon < lonA | datOcc126_4070Yr_V3$lon > lonB] <- NA
datOcc126_4070Yr_V3$lat[datOcc126_4070Yr_V3$lat > latA | datOcc126_4070Yr_V3$lat < latB] <- NA
datOcc126_7000Yr_V3$lon[datOcc126_7000Yr_V3$lon < lonA | datOcc126_7000Yr_V3$lon > lonB] <- NA
datOcc126_7000Yr_V3$lat[datOcc126_7000Yr_V3$lat > latA | datOcc126_7000Yr_V3$lat < latB] <- NA
datOcc585_1040Yr_V3$lon[datOcc585_1040Yr_V3$lon < lonA | datOcc585_1040Yr_V3$lon > lonB] <- NA
datOcc585_1040Yr_V3$lat[datOcc585_1040Yr_V3$lat > latA | datOcc585_1040Yr_V3$lat < latB] <- NA
datOcc585_4070Yr_V3$lon[datOcc585_4070Yr_V3$lon < lonA | datOcc585_4070Yr_V3$lon > lonB] <- NA
datOcc585_4070Yr_V3$lat[datOcc585_4070Yr_V3$lat > latA | datOcc585_4070Yr_V3$lat < latB] <- NA
datOcc585_7000Yr_V3$lon[datOcc585_7000Yr_V3$lon < lonA | datOcc585_7000Yr_V3$lon > lonB] <- NA
datOcc585_7000Yr_V3$lat[datOcc585_7000Yr_V3$lat > latA | datOcc585_7000Yr_V3$lat < latB] <- NA
datOccHYr_V3 <- na.omit(datOccHYr_V3)
datOcc126_1040Yr_V3 <- na.omit(datOcc126_1040Yr_V3)
datOcc126_4070Yr_V3 <- na.omit(datOcc126_4070Yr_V3)
datOcc126_7000Yr_V3 <- na.omit(datOcc126_7000Yr_V3)
datOcc585_1040Yr_V3 <- na.omit(datOcc585_1040Yr_V3)
datOcc585_4070Yr_V3 <- na.omit(datOcc585_4070Yr_V3)
datOcc585_7000Yr_V3 <- na.omit(datOcc585_7000Yr_V3)

datOccHYr_V4$lon[datOccHYr_V4$lon < lonA | datOccHYr_V4$lon > lonB] <- NA
datOccHYr_V4$lat[datOccHYr_V4$lat > latA | datOccHYr_V4$lat < latB] <- NA
datOcc126_1040Yr_V4$lon[datOcc126_1040Yr_V4$lon < lonA | datOcc126_1040Yr_V4$lon > lonB] <- NA
datOcc126_1040Yr_V4$lat[datOcc126_1040Yr_V4$lat > latA | datOcc126_1040Yr_V4$lat < latB] <- NA
datOcc126_4070Yr_V4$lon[datOcc126_4070Yr_V4$lon < lonA | datOcc126_4070Yr_V4$lon > lonB] <- NA
datOcc126_4070Yr_V4$lat[datOcc126_4070Yr_V4$lat > latA | datOcc126_4070Yr_V4$lat < latB] <- NA
datOcc126_7000Yr_V4$lon[datOcc126_7000Yr_V4$lon < lonA | datOcc126_7000Yr_V4$lon > lonB] <- NA
datOcc126_7000Yr_V4$lat[datOcc126_7000Yr_V4$lat > latA | datOcc126_7000Yr_V4$lat < latB] <- NA
datOcc585_1040Yr_V4$lon[datOcc585_1040Yr_V4$lon < lonA | datOcc585_1040Yr_V4$lon > lonB] <- NA
datOcc585_1040Yr_V4$lat[datOcc585_1040Yr_V4$lat > latA | datOcc585_1040Yr_V4$lat < latB] <- NA
datOcc585_4070Yr_V4$lon[datOcc585_4070Yr_V4$lon < lonA | datOcc585_4070Yr_V4$lon > lonB] <- NA
datOcc585_4070Yr_V4$lat[datOcc585_4070Yr_V4$lat > latA | datOcc585_4070Yr_V4$lat < latB] <- NA
datOcc585_7000Yr_V4$lon[datOcc585_7000Yr_V4$lon < lonA | datOcc585_7000Yr_V4$lon > lonB] <- NA
datOcc585_7000Yr_V4$lat[datOcc585_7000Yr_V4$lat > latA | datOcc585_7000Yr_V4$lat < latB] <- NA
datOccHYr_V4 <- na.omit(datOccHYr_V4)
datOcc126_1040Yr_V4 <- na.omit(datOcc126_1040Yr_V4)
datOcc126_4070Yr_V4 <- na.omit(datOcc126_4070Yr_V4)
datOcc126_7000Yr_V4 <- na.omit(datOcc126_7000Yr_V4)
datOcc585_1040Yr_V4 <- na.omit(datOcc585_1040Yr_V4)
datOcc585_4070Yr_V4 <- na.omit(datOcc585_4070Yr_V4)
datOcc585_7000Yr_V4 <- na.omit(datOcc585_7000Yr_V4)

# . . 4.1.4 Formatting ---------------------------------------------------------
# Var1 - GridCell
datOccHYr_V1 <- gather(datOccHYr_V1, key = Year, value = Occurrences, 3:250)
datOccHYr_V1$Year <- apply(datOccHYr_V1[,3], MARGIN = 1, FUN = splitMod, 
                           position = 2 )
datOcc126_1040Yr_V1 <- gather(datOcc126_1040Yr_V1, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_1040Yr_V1$Year <- apply(datOcc126_1040Yr_V1[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc126_4070Yr_V1 <- gather(datOcc126_4070Yr_V1, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_4070Yr_V1$Year <- apply(datOcc126_4070Yr_V1[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc126_7000Yr_V1 <- gather(datOcc126_7000Yr_V1, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_7000Yr_V1$Year <- apply(datOcc126_7000Yr_V1[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_1040Yr_V1 <- gather(datOcc585_1040Yr_V1, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_1040Yr_V1$Year <- apply(datOcc585_1040Yr_V1[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_4070Yr_V1 <- gather(datOcc585_4070Yr_V1, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_4070Yr_V1$Year <- apply(datOcc585_4070Yr_V1[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_7000Yr_V1 <- gather(datOcc585_7000Yr_V1, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_7000Yr_V1$Year <- apply(datOcc585_7000Yr_V1[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOccHYr_V1$Scenario <- 'Historic'
datOcc126_1040Yr_V1$Scenario <- 'SSP126'
datOcc126_4070Yr_V1$Scenario <- 'SSP126'
datOcc126_7000Yr_V1$Scenario <- 'SSP126'
datOcc585_1040Yr_V1$Scenario <- 'SSP585'
datOcc585_4070Yr_V1$Scenario <- 'SSP585'
datOcc585_7000Yr_V1$Scenario <- 'SSP585'
datOccYr_V1 <- rbind(datOccHYr_V1, datOcc126_1040Yr_V1, datOcc126_4070Yr_V1, 
                     datOcc126_7000Yr_V1, datOcc585_1040Yr_V1,
                     datOcc585_4070Yr_V1, datOcc585_7000Yr_V1)
datOccYr_V1$Year <- as.integer(datOccYr_V1$Year)

# Var3 - GridCell
datOccHYr_V3 <- gather(datOccHYr_V3, key = Year, value = Occurrences, 3:250)
datOccHYr_V3$Year <- apply(datOccHYr_V3[,3], MARGIN = 1, FUN = splitMod, 
                           position = 2 )
datOcc126_1040Yr_V3 <- gather(datOcc126_1040Yr_V3, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_1040Yr_V3$Year <- apply(datOcc126_1040Yr_V3[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc126_4070Yr_V3 <- gather(datOcc126_4070Yr_V3, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_4070Yr_V3$Year <- apply(datOcc126_4070Yr_V3[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc126_7000Yr_V3 <- gather(datOcc126_7000Yr_V3, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_7000Yr_V3$Year <- apply(datOcc126_7000Yr_V3[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_1040Yr_V3 <- gather(datOcc585_1040Yr_V3, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_1040Yr_V3$Year <- apply(datOcc585_1040Yr_V3[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_4070Yr_V3 <- gather(datOcc585_4070Yr_V3, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_4070Yr_V3$Year <- apply(datOcc585_4070Yr_V3[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_7000Yr_V3 <- gather(datOcc585_7000Yr_V3, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_7000Yr_V3$Year <- apply(datOcc585_7000Yr_V3[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOccHYr_V3$Scenario <- 'Historic'
datOcc126_1040Yr_V3$Scenario <- 'SSP126'
datOcc126_4070Yr_V3$Scenario <- 'SSP126'
datOcc126_7000Yr_V3$Scenario <- 'SSP126'
datOcc585_1040Yr_V3$Scenario <- 'SSP585'
datOcc585_4070Yr_V3$Scenario <- 'SSP585'
datOcc585_7000Yr_V3$Scenario <- 'SSP585'
datOccYr_V3 <- rbind(datOccHYr_V3, datOcc126_1040Yr_V3, datOcc126_4070Yr_V3, 
                     datOcc126_7000Yr_V3, datOcc585_1040Yr_V3,
                     datOcc585_4070Yr_V3, datOcc585_7000Yr_V3)
datOccYr_V3$Year <- as.integer(datOccYr_V3$Year)

# Var4 - GridCell
datOccHYr_V4 <- gather(datOccHYr_V4, key = Year, value = Occurrences, 3:250)
datOccHYr_V4$Year <- apply(datOccHYr_V4[,3], MARGIN = 1, FUN = splitMod, 
                           position = 2 )
datOcc126_1040Yr_V4 <- gather(datOcc126_1040Yr_V4, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_1040Yr_V4$Year <- apply(datOcc126_1040Yr_V4[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc126_4070Yr_V4 <- gather(datOcc126_4070Yr_V4, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_4070Yr_V4$Year <- apply(datOcc126_4070Yr_V4[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc126_7000Yr_V4 <- gather(datOcc126_7000Yr_V4, key = Year, 
                              value = Occurrences, 3:250)
datOcc126_7000Yr_V4$Year <- apply(datOcc126_7000Yr_V4[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_1040Yr_V4 <- gather(datOcc585_1040Yr_V4, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_1040Yr_V4$Year <- apply(datOcc585_1040Yr_V4[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_4070Yr_V4 <- gather(datOcc585_4070Yr_V4, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_4070Yr_V4$Year <- apply(datOcc585_4070Yr_V4[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOcc585_7000Yr_V4 <- gather(datOcc585_7000Yr_V4, key = Year, 
                              value = Occurrences, 3:250)
datOcc585_7000Yr_V4$Year <- apply(datOcc585_7000Yr_V4[,3], MARGIN = 1, FUN = splitMod, 
                                  position = 2 )
datOccHYr_V4$Scenario <- 'Historic'
datOcc126_1040Yr_V4$Scenario <- 'SSP126'
datOcc126_4070Yr_V4$Scenario <- 'SSP126'
datOcc126_7000Yr_V4$Scenario <- 'SSP126'
datOcc585_1040Yr_V4$Scenario <- 'SSP585'
datOcc585_4070Yr_V4$Scenario <- 'SSP585'
datOcc585_7000Yr_V4$Scenario <- 'SSP585'
datOccYr_V4 <- rbind(datOccHYr_V4, datOcc126_1040Yr_V4, datOcc126_4070Yr_V4, 
                     datOcc126_7000Yr_V4, datOcc585_1040Yr_V4,
                     datOcc585_4070Yr_V4, datOcc585_7000Yr_V4)
datOccYr_V4$Year <- as.integer(datOccYr_V4$Year)

# # . . 4.1.4 Formatting - Stand Dev ---------------------------------------------
# dfreed <- dim(datOccHYr_V1)[1] - 1
# TScore <- qt(p=0.05/2, df= dfreed,lower.tail=F)
# SQn <- sqrt(dim(datOccHYr_V1)[1])
# 
# # Var1 - GridCell
# datOccHYr_V1 <- gather(datOccHYr_V1, key = Year, value = Occurrences, 3:250)
# datOccHYr_V1$Model <- apply(datOccHYr_V1[,3], MARGIN = 1, FUN = splitMod, 
#                             position = 1 )
# datOccHYr_V1$Year <- apply(datOccHYr_V1[,3], MARGIN = 1, FUN = splitMod, 
#                             position = 2 )
# dat <- datOccHYr_V1 %>%
#   group_by(lon,lat,Year) %>%
#   summarise(Mean = mean(Occurrences), Sd = sd(Occurrences))
# # Var1 - Region
# datOccYr_V1 <- dat %>%
#   group_by(Year) %>%
#   summarise(Sd = sd(Mean), Mean = mean(Mean)) %>%
#   cbind(Scenario = 'Historical')
# 
# # . . 4.1.4 Formatting -Original -----------------------------------------------
# datOccHYr_V1 <- gather(datOccHYr_V1, key = Year, value = Occurrences, 3:33)
# datOcc126_1040Yr_V1 <- gather(datOcc126_1040Yr_V1, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc126_4070Yr_V1 <- gather(datOcc126_4070Yr_V1, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc126_7000Yr_V1 <- gather(datOcc126_7000Yr_V1, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_1040Yr_V1 <- gather(datOcc585_1040Yr_V1, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_4070Yr_V1 <- gather(datOcc585_4070Yr_V1, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_7000Yr_V1 <- gather(datOcc585_7000Yr_V1, key = Year, 
#                               value =Occurrences, 3:33)
# datOccHYr_V1$Scenario <- 'Historic'
# datOcc126_1040Yr_V1$Scenario <- 'SSP126'
# datOcc126_4070Yr_V1$Scenario <- 'SSP126'
# datOcc126_7000Yr_V1$Scenario <- 'SSP126'
# datOcc585_1040Yr_V1$Scenario <- 'SSP585'
# datOcc585_4070Yr_V1$Scenario <- 'SSP585'
# datOcc585_7000Yr_V1$Scenario <- 'SSP585'
# datOccYr_V1 <- rbind(datOccHYr_V1, datOcc126_1040Yr_V1, datOcc126_4070Yr_V1, 
#                      datOcc126_7000Yr_V1, datOcc585_1040Yr_V1,
#                      datOcc585_4070Yr_V1, datOcc585_7000Yr_V1)
# datOccYr_V1$Year <- as.integer(datOccYr_V1$Year)
# # Var 3
# datOccHYr_V3 <- gather(datOccHYr_V3, key = Year, value = Occurrences, 3:33)
# datOcc126_1040Yr_V3 <- gather(datOcc126_1040Yr_V3, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc126_4070Yr_V3 <- gather(datOcc126_4070Yr_V3, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc126_7000Yr_V3 <- gather(datOcc126_7000Yr_V3, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_1040Yr_V3 <- gather(datOcc585_1040Yr_V3, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_4070Yr_V3 <- gather(datOcc585_4070Yr_V3, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_7000Yr_V3 <- gather(datOcc585_7000Yr_V3, key = Year, 
#                               value =Occurrences, 3:33)
# datOccHYr_V3$Scenario <- 'Historic'
# datOcc126_1040Yr_V3$Scenario <- 'SSP126'
# datOcc126_4070Yr_V3$Scenario <- 'SSP126'
# datOcc126_7000Yr_V3$Scenario <- 'SSP126'
# datOcc585_1040Yr_V3$Scenario <- 'SSP585'
# datOcc585_4070Yr_V3$Scenario <- 'SSP585'
# datOcc585_7000Yr_V3$Scenario <- 'SSP585'
# datOccYr_V3 <- rbind(datOccHYr_V3, datOcc126_1040Yr_V3, datOcc126_4070Yr_V3, 
#                      datOcc126_7000Yr_V3, datOcc585_1040Yr_V3,
#                      datOcc585_4070Yr_V3, datOcc585_7000Yr_V3)
# datOccYr_V3$Year <- as.integer(datOccYr_V3$Year)
# # Var 4
# # Get 2010 data of future
# # datH_2010 <- cbind(datOccHYr_V4$lon, datOccHYr_V4$lat, datOccHYr_V4$`2010`)
# # dat126_2010 <- cbind(datOcc126_1040Yr_V4$lon, datOcc126_1040Yr_V4$lat,datOcc126_1040Yr_V4$`2010`)
# # dat585_2010 <- cbind(datOcc585_1040Yr_V4$lon, datOcc585_1040Yr_V4$lat,datOcc585_1040Yr_V4$`2010`)
# 
# datOccHYr_V4 <- gather(datOccHYr_V4, key = Year, value = Occurrences, 3:33)
# datOcc126_1040Yr_V4 <- gather(datOcc126_1040Yr_V4, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc126_4070Yr_V4 <- gather(datOcc126_4070Yr_V4, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc126_7000Yr_V4 <- gather(datOcc126_7000Yr_V4, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_1040Yr_V4 <- gather(datOcc585_1040Yr_V4, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_4070Yr_V4 <- gather(datOcc585_4070Yr_V4, key = Year, 
#                               value =Occurrences, 3:33)
# datOcc585_7000Yr_V4 <- gather(datOcc585_7000Yr_V4, key = Year, 
#                               value =Occurrences, 3:33)
# datOccHYr_V4$Scenario <- 'Historic'
# datOcc126_1040Yr_V4$Scenario <- 'SSP126'
# datOcc126_4070Yr_V4$Scenario <- 'SSP126'
# datOcc126_7000Yr_V4$Scenario <- 'SSP126'
# datOcc585_1040Yr_V4$Scenario <- 'SSP585'
# datOcc585_4070Yr_V4$Scenario <- 'SSP585'
# datOcc585_7000Yr_V4$Scenario <- 'SSP585'
# datOccYr_V4 <- rbind(datOccHYr_V4, datOcc126_1040Yr_V4, datOcc126_4070Yr_V4, 
#                      datOcc126_7000Yr_V4, datOcc585_1040Yr_V4,
#                      datOcc585_4070Yr_V4, datOcc585_7000Yr_V4)
# # datOccYr_V4 <- rbind(datOccYr_V4, 
# #                      cbind(lon = datH_2010[,1], lat = datH_2010[,2], Year = '2010',
# #                            Occurrences = datH_2010[,3], Scenario = 'SSP126'),
# #                      cbind(lon = datH_2010[,1], lat = datH_2010[,2], Year = '2010',
# #                            Occurrences = datH_2010[,3], Scenario = 'SSP585'),
# #                      cbind(lon = dat126_2010[,1], lat = dat126_2010[,2], Year = '2010',
# #                            Occurrences = dat126_2010[,3], Scenario = 'Historic'),
# #                      cbind(lon = dat126_2010[,1], lat = dat126_2010[,2], Year = '2010',
# #                            Occurrences = dat126_2010[,3], Scenario = 'SSP585'),
# #                      cbind(lon = dat585_2010[,1], lat = dat585_2010[,2], Year = '2010',
# #                            Occurrences = dat585_2010[,3], Scenario = 'Historic'),
# #                      cbind(lon = dat585_2010[,1], lat = dat585_2010[,2], Year = '2010',
# #                            Occurrences = dat585_2010[,3], Scenario = 'SSP126'))
# datOccYr_V4$Year <- as.integer(datOccYr_V4$Year)
# datOccYr_V4$Occurrences <- as.numeric(datOccYr_V4$Occurrences)

# . . 4.1.5 Plotting time series -----------------------------------------------
a <- paste0('Occurrences of ', varT[1])
p1 <- ggplot(datOccYr_V1, aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, size = 0.5) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='darkgreen','SSP126'='orange',
                               'SSP585' = 'red'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="bottom")

a <- paste0('Occurrences of ', varT[3])
p2 <- ggplot(datOccYr_V3, aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, size = 0.5) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='darkgreen','SSP126'='orange',
                               'SSP585' = 'red'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

a <- paste0('Occurrences of ', varT[4])
p3 <- ggplot(datOccYr_V4, aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, size = 0.5) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='darkgreen','SSP126'='orange',
                               'SSP585' = 'red'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = 'Year', y = 'Occurrences')  +
  theme(legend.position="NULL")

myLegend <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1 <- plot_grid(p1,
                p2,
                p3,
                nrow = 3,
                rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0(locTitle[location]), fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                rel_heights = c(.05,1, 0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/','OCCYr_TIMESER_', timeSpan, 
                             'Region', location, ".tiff"),
       width = 9, height = 6, dpi = 350, bg='white')

# . . 4.1.6 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'lat1', 'lat2','lon1','lon2',
                           'location','locTitle','mFileH','mFile126','mFile585',
                           'var', 'varT', 'baseData', 'timeSpan',
                           'get_legend','as_ggplot','mean_cl_quantile','acf2', 
                           'splitMod')])
# . 4.2 Regional Scatter Plot --------------------------------------------------
# . . 4.2.1 Variables Needed ---------------------------------------------------
location <- 5
lonA <- lon1[location]
lonB <- lon2[location]
latA <- lat1[location]
latB <- lat2[location]

# . . 4.2.2 Opening Files Needed -----------------------------------------------
datOccHYr_V1 <- read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                'OCCYr_DAY_', var[1],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 3
datOccHYr_V3 <- read_csv(paste0(fileloc1, loc1[1], 'Results/',
                                'OCCYr_DAY_', var[3],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 4
datOccHYr_V4 <- read_csv(paste0(fileloc1, loc1[1], 'Results/',
                                'OCCYr_DAY_', var[4],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',  
                                       'OCCYr_DAY_', var[4],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# . . 4.2.3 Defining locations -------------------------------------------------
datOccHYr_V1$lon[datOccHYr_V1$lon < lonA | datOccHYr_V1$lon > lonB] <- NA
datOccHYr_V1$lat[datOccHYr_V1$lat > latA | datOccHYr_V1$lat < latB] <- NA
datOcc126_1040Yr_V1$lon[datOcc126_1040Yr_V1$lon < lonA | datOcc126_1040Yr_V1$lon > lonB] <- NA
datOcc126_1040Yr_V1$lat[datOcc126_1040Yr_V1$lat > latA | datOcc126_1040Yr_V1$lat < latB] <- NA
datOcc126_4070Yr_V1$lon[datOcc126_4070Yr_V1$lon < lonA | datOcc126_4070Yr_V1$lon > lonB] <- NA
datOcc126_4070Yr_V1$lat[datOcc126_4070Yr_V1$lat > latA | datOcc126_4070Yr_V1$lat < latB] <- NA
datOcc126_7000Yr_V1$lon[datOcc126_7000Yr_V1$lon < lonA | datOcc126_7000Yr_V1$lon > lonB] <- NA
datOcc126_7000Yr_V1$lat[datOcc126_7000Yr_V1$lat > latA | datOcc126_7000Yr_V1$lat < latB] <- NA
datOcc585_1040Yr_V1$lon[datOcc585_1040Yr_V1$lon < lonA | datOcc585_1040Yr_V1$lon > lonB] <- NA
datOcc585_1040Yr_V1$lat[datOcc585_1040Yr_V1$lat > latA | datOcc585_1040Yr_V1$lat < latB] <- NA
datOcc585_4070Yr_V1$lon[datOcc585_4070Yr_V1$lon < lonA | datOcc585_4070Yr_V1$lon > lonB] <- NA
datOcc585_4070Yr_V1$lat[datOcc585_4070Yr_V1$lat > latA | datOcc585_4070Yr_V1$lat < latB] <- NA
datOcc585_7000Yr_V1$lon[datOcc585_7000Yr_V1$lon < lonA | datOcc585_7000Yr_V1$lon > lonB] <- NA
datOcc585_7000Yr_V1$lat[datOcc585_7000Yr_V1$lat > latA | datOcc585_7000Yr_V1$lat < latB] <- NA
datOccHYr_V1 <- na.omit(datOccHYr_V1)
datOcc126_1040Yr_V1 <- na.omit(datOcc126_1040Yr_V1)
datOcc126_4070Yr_V1 <- na.omit(datOcc126_4070Yr_V1)
datOcc126_7000Yr_V1 <- na.omit(datOcc126_7000Yr_V1)
datOcc585_1040Yr_V1 <- na.omit(datOcc585_1040Yr_V1)
datOcc585_4070Yr_V1 <- na.omit(datOcc585_4070Yr_V1)
datOcc585_7000Yr_V1 <- na.omit(datOcc585_7000Yr_V1)

datOccHYr_V3$lon[datOccHYr_V3$lon < lonA | datOccHYr_V3$lon > lonB] <- NA
datOccHYr_V3$lat[datOccHYr_V3$lat > latA | datOccHYr_V3$lat < latB] <- NA
datOcc126_1040Yr_V3$lon[datOcc126_1040Yr_V3$lon < lonA | datOcc126_1040Yr_V3$lon > lonB] <- NA
datOcc126_1040Yr_V3$lat[datOcc126_1040Yr_V3$lat > latA | datOcc126_1040Yr_V3$lat < latB] <- NA
datOcc126_4070Yr_V3$lon[datOcc126_4070Yr_V3$lon < lonA | datOcc126_4070Yr_V3$lon > lonB] <- NA
datOcc126_4070Yr_V3$lat[datOcc126_4070Yr_V3$lat > latA | datOcc126_4070Yr_V3$lat < latB] <- NA
datOcc126_7000Yr_V3$lon[datOcc126_7000Yr_V3$lon < lonA | datOcc126_7000Yr_V3$lon > lonB] <- NA
datOcc126_7000Yr_V3$lat[datOcc126_7000Yr_V3$lat > latA | datOcc126_7000Yr_V3$lat < latB] <- NA
datOcc585_1040Yr_V3$lon[datOcc585_1040Yr_V3$lon < lonA | datOcc585_1040Yr_V3$lon > lonB] <- NA
datOcc585_1040Yr_V3$lat[datOcc585_1040Yr_V3$lat > latA | datOcc585_1040Yr_V3$lat < latB] <- NA
datOcc585_4070Yr_V3$lon[datOcc585_4070Yr_V3$lon < lonA | datOcc585_4070Yr_V3$lon > lonB] <- NA
datOcc585_4070Yr_V3$lat[datOcc585_4070Yr_V3$lat > latA | datOcc585_4070Yr_V3$lat < latB] <- NA
datOcc585_7000Yr_V3$lon[datOcc585_7000Yr_V3$lon < lonA | datOcc585_7000Yr_V3$lon > lonB] <- NA
datOcc585_7000Yr_V3$lat[datOcc585_7000Yr_V3$lat > latA | datOcc585_7000Yr_V3$lat < latB] <- NA
datOccHYr_V3 <- na.omit(datOccHYr_V3)
datOcc126_1040Yr_V3 <- na.omit(datOcc126_1040Yr_V3)
datOcc126_4070Yr_V3 <- na.omit(datOcc126_4070Yr_V3)
datOcc126_7000Yr_V3 <- na.omit(datOcc126_7000Yr_V3)
datOcc585_1040Yr_V3 <- na.omit(datOcc585_1040Yr_V3)
datOcc585_4070Yr_V3 <- na.omit(datOcc585_4070Yr_V3)
datOcc585_7000Yr_V3 <- na.omit(datOcc585_7000Yr_V3)

datOccHYr_V4$lon[datOccHYr_V4$lon < lonA | datOccHYr_V4$lon > lonB] <- NA
datOccHYr_V4$lat[datOccHYr_V4$lat > latA | datOccHYr_V4$lat < latB] <- NA
datOcc126_1040Yr_V4$lon[datOcc126_1040Yr_V4$lon < lonA | datOcc126_1040Yr_V4$lon > lonB] <- NA
datOcc126_1040Yr_V4$lat[datOcc126_1040Yr_V4$lat > latA | datOcc126_1040Yr_V4$lat < latB] <- NA
datOcc126_4070Yr_V4$lon[datOcc126_4070Yr_V4$lon < lonA | datOcc126_4070Yr_V4$lon > lonB] <- NA
datOcc126_4070Yr_V4$lat[datOcc126_4070Yr_V4$lat > latA | datOcc126_4070Yr_V4$lat < latB] <- NA
datOcc126_7000Yr_V4$lon[datOcc126_7000Yr_V4$lon < lonA | datOcc126_7000Yr_V4$lon > lonB] <- NA
datOcc126_7000Yr_V4$lat[datOcc126_7000Yr_V4$lat > latA | datOcc126_7000Yr_V4$lat < latB] <- NA
datOcc585_1040Yr_V4$lon[datOcc585_1040Yr_V4$lon < lonA | datOcc585_1040Yr_V4$lon > lonB] <- NA
datOcc585_1040Yr_V4$lat[datOcc585_1040Yr_V4$lat > latA | datOcc585_1040Yr_V4$lat < latB] <- NA
datOcc585_4070Yr_V4$lon[datOcc585_4070Yr_V4$lon < lonA | datOcc585_4070Yr_V4$lon > lonB] <- NA
datOcc585_4070Yr_V4$lat[datOcc585_4070Yr_V4$lat > latA | datOcc585_4070Yr_V4$lat < latB] <- NA
datOcc585_7000Yr_V4$lon[datOcc585_7000Yr_V4$lon < lonA | datOcc585_7000Yr_V4$lon > lonB] <- NA
datOcc585_7000Yr_V4$lat[datOcc585_7000Yr_V4$lat > latA | datOcc585_7000Yr_V4$lat < latB] <- NA
datOccHYr_V4 <- na.omit(datOccHYr_V4)
datOcc126_1040Yr_V4 <- na.omit(datOcc126_1040Yr_V4)
datOcc126_4070Yr_V4 <- na.omit(datOcc126_4070Yr_V4)
datOcc126_7000Yr_V4 <- na.omit(datOcc126_7000Yr_V4)
datOcc585_1040Yr_V4 <- na.omit(datOcc585_1040Yr_V4)
datOcc585_4070Yr_V4 <- na.omit(datOcc585_4070Yr_V4)
datOcc585_7000Yr_V4 <- na.omit(datOcc585_7000Yr_V4)

# . . 4.2.4 Formatting ---------------------------------------------------------
# Compute yearly averages
datOccHYr_V1 <- datOccHYr_V1 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_1040Yr_V1 <- datOcc126_1040Yr_V1 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_4070Yr_V1 <- datOcc126_4070Yr_V1 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_7000Yr_V1 <- datOcc126_7000Yr_V1 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_1040Yr_V1 <- datOcc585_1040Yr_V1 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_4070Yr_V1 <- datOcc585_4070Yr_V1 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_7000Yr_V1 <- datOcc585_7000Yr_V1 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
# Var3
datOccHYr_V3 <- datOccHYr_V3 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_1040Yr_V3 <- datOcc126_1040Yr_V3 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_4070Yr_V3 <- datOcc126_4070Yr_V3 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_7000Yr_V3 <- datOcc126_7000Yr_V3 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_1040Yr_V3 <- datOcc585_1040Yr_V3 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_4070Yr_V3 <- datOcc585_4070Yr_V3 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_7000Yr_V3 <- datOcc585_7000Yr_V3 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
# Var4
datOccHYr_V4 <- datOccHYr_V4 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_1040Yr_V4 <- datOcc126_1040Yr_V4 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_4070Yr_V4 <- datOcc126_4070Yr_V4 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc126_7000Yr_V4 <- datOcc126_7000Yr_V4 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_1040Yr_V4 <- datOcc585_1040Yr_V4 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_4070Yr_V4 <- datOcc585_4070Yr_V4 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)
datOcc585_7000Yr_V4 <- datOcc585_7000Yr_V4 %>%
  gather(key = 'Year', value = 'Occurrences', -c(lon, lat)) %>%
  group_by(Year) %>%
  summarise_at(vars(Occurrences), list(name = mean)) %>%
  arrange(Year)

# Find the difference in time periods
datOcc126_1040Yr_V1$name <- datOcc126_1040Yr_V1$name - datOccHYr_V1$name
datOcc126_4070Yr_V1$name <- datOcc126_4070Yr_V1$name - datOccHYr_V1$name
datOcc126_7000Yr_V1$name <- datOcc126_7000Yr_V1$name - datOccHYr_V1$name
datOcc585_1040Yr_V1$name <- datOcc585_1040Yr_V1$name - datOccHYr_V1$name
datOcc585_4070Yr_V1$name <- datOcc585_4070Yr_V1$name - datOccHYr_V1$name
datOcc585_7000Yr_V1$name <- datOcc585_7000Yr_V1$name - datOccHYr_V1$name

# Find the difference in time periods
datOcc126_1040Yr_V3$name <- datOcc126_1040Yr_V3$name - datOccHYr_V3$name
datOcc126_4070Yr_V3$name <- datOcc126_4070Yr_V3$name - datOccHYr_V3$name
datOcc126_7000Yr_V3$name <- datOcc126_7000Yr_V3$name - datOccHYr_V3$name
datOcc585_1040Yr_V3$name <- datOcc585_1040Yr_V3$name - datOccHYr_V3$name
datOcc585_4070Yr_V3$name <- datOcc585_4070Yr_V3$name - datOccHYr_V3$name
datOcc585_7000Yr_V3$name <- datOcc585_7000Yr_V3$name - datOccHYr_V3$name

# Find the difference in time periods
datOcc126_1040Yr_V4$name <- datOcc126_1040Yr_V4$name - datOccHYr_V4$name
datOcc126_4070Yr_V4$name <- datOcc126_4070Yr_V4$name - datOccHYr_V4$name
datOcc126_7000Yr_V4$name <- datOcc126_7000Yr_V4$name - datOccHYr_V4$name
datOcc585_1040Yr_V4$name <- datOcc585_1040Yr_V4$name - datOccHYr_V4$name
datOcc585_4070Yr_V4$name <- datOcc585_4070Yr_V4$name - datOccHYr_V4$name
datOcc585_7000Yr_V4$name <- datOcc585_7000Yr_V4$name - datOccHYr_V4$name

# Combine
dat <- 
  # cbind(datOccHYr_V1, datOccHYr_V3[,2], datOccHYr_V4[,2], 
  #            TimePeriod = 'Historic', Scenario = 'Historic') %>%
  # rbind(cbind(datOcc126_1040Yr_V1, datOcc126_1040Yr_V3[,2], datOcc126_1040Yr_V4[,2],
  #             TimePeriod = 'Early-Century', Scenario ='SSP126')) %>%
  cbind(datOcc126_1040Yr_V1, datOcc126_1040Yr_V3[,2], datOcc126_1040Yr_V4[,2],
        TimePeriod = 'Early-Century', Scenario ='SSP126') %>%
  rbind(cbind(datOcc126_4070Yr_V1, datOcc126_4070Yr_V3[,2], datOcc126_4070Yr_V4[,2],  
              TimePeriod = 'Mid-Century', Scenario ='SSP126')) %>%
  rbind(cbind(datOcc126_7000Yr_V1, datOcc126_7000Yr_V3[,2], datOcc126_7000Yr_V4[,2],  
              TimePeriod = 'Late-Century', Scenario ='SSP126')) %>%
  rbind(cbind(datOcc585_1040Yr_V1, datOcc585_1040Yr_V3[,2], datOcc585_1040Yr_V4[,2],  
              TimePeriod = 'Early-Century', Scenario ='SSP585')) %>%
  rbind(cbind(datOcc585_4070Yr_V1, datOcc585_4070Yr_V3[,2], datOcc585_4070Yr_V4[,2], 
              TimePeriod = 'Mid-Century', Scenario ='SSP585')) %>%
  rbind(cbind(datOcc585_7000Yr_V1, datOcc585_7000Yr_V3[,2], datOcc585_7000Yr_V4[,2],  
              TimePeriod = 'Late-Century', Scenario ='SSP585'))
colnames(dat) <- c('Year', var[1],var[3],var[4], 'TimePeriod', 'Scenario')  
dat$TimePeriod <- factor(dat$TimePeriod,
                         levels = c('Historic', 'Early-Century',
                                    'Mid-Century','Late-Century'))

# . . 4.2.5 Plotting -----------------------------------------------------------
# Var1 v Var4  SSP126
main <- ggplot(subset(dat, Scenario != 'SSP585'), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position="NULL") +
  scale_color_discrete(name = "Time Period")
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat, Scenario != 'SSP585'), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2)
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat, Scenario != 'SSP585'), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip()
p1 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p1 <- insert_yaxis_grob(p1, denY, grid::unit(.2, "null"), position = "right")

# Var3 v Var4    SSP126
main <- ggplot(subset(dat, Scenario != 'SSP585'), 
               aes(x = pr, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Flash Drought') +
  theme(legend.position='NULL') 
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat, Scenario != 'SSP585'), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2)
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat, Scenario != 'SSP585'), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip()
p2 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p2, denY, grid::unit(.2, "null"), position = "right")

# Var1 v Var3   SSP126
main <- ggplot(subset(dat, Scenario != 'SSP585'), 
               aes(x = pr, y = tasmax, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Heatwaves') +
  theme(legend.position='NULL') 
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat, Scenario != 'SSP585'), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2)
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat, Scenario != 'SSP585'), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip()
p3 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p3 <- insert_yaxis_grob(p3, denY, grid::unit(.2, "null"), position = "right")
# Var1 v Var4  SSP585
main <- ggplot(subset(dat, Scenario != 'SSP126'), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position='NULL') +
  scale_color_discrete(name = "Time Period")
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat, Scenario != 'SSP126'), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2)
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat, Scenario != 'SSP126'), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip()
p4 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p4 <- insert_yaxis_grob(p4, denY, grid::unit(.2, "null"), position = "right")
ggdraw(p4)
# Var3 v Var4    SSP585
main <- ggplot(subset(dat, Scenario != 'SSP126'), 
               aes(x = pr, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Flash Drought') +
  theme(legend.position='NULL') 
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat, Scenario != 'SSP126'), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2)
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat, Scenario != 'SSP126'), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip()
p5 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p5 <- insert_yaxis_grob(p5, denY, grid::unit(.2, "null"), position = "right")

# Var1 v Var3   SSP585
main <- ggplot(subset(dat, Scenario != 'SSP126'), 
               aes(x = pr, y = tasmax, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Heatwaves') +
  theme(legend.position='NULL') 
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat, Scenario != 'SSP126'), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2)
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat, Scenario != 'SSP126'), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip()
p6 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p6 <- insert_yaxis_grob(p6, denY, grid::unit(.2, "null"), position = "right")

main <- ggplot(subset(dat, Scenario != 'SSP585'), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position="bottom") +
  scale_color_discrete(name = "Time Period")
myLegend <- get_legend(main, position = 'bottom') %>% 
  as_ggplot()

F1 <- plot_grid(p1, p4,
                p2, p5,
                p3, p6,
                nrow = 3,
                ncol = 2,
                rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0('Change in extreme events at ',locTitle[location]), fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                rel_heights = c(.05,1, 0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/','OCCYr_SCATT_', timeSpan, 
                             'Region', location, ".tiff"),
       width = 9, height = 6, dpi = 350, bg='white')

# . . 4.2.6 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'lat1', 'lat2','lon1','lon2',
                           'location','locTitle','mFileH','mFile126','mFile585',
                           'var', 'varT', 'baseData', 'timeSpan',
                           'get_legend','as_ggplot','mean_cl_quantile','acf2',
                           'splitMod')])
# . 4.3 Significant Time Series ------------------------------------------------
# . . 4.3.1 Variables Needed ---------------------------------------------------
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')

# . . 4.3.2 Opening Files Needed -----------------------------------------------
#Var 1
Occ_V1 <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var[1],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOccHYr_V1 <- read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                'OCCYr_DAY_', var[1],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V1 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V1 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 3
Occ_V3 <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var[3],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOccHYr_V3 <- read_csv(paste0(fileloc1, loc1[1], 'Results/',
                                'OCCYr_DAY_', var[3],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V3 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V3 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 4
Occ_V4 <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var[4],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOccHYr_V4 <- read_csv(paste0(fileloc1, loc1[1], 'Results/',
                                'OCCYr_DAY_', var[4],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V4 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V4 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# . . 4.3.3 Determine Significance ---------------------------------------------
# Var 1
Occ_V1 <- Occ_V1 %>%
  select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig, 
         SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
Occ_V1$SSP126585 <- 0
Occ_V1$SSP126 <- 0
Occ_V1$SSP585 <- 0
Occ_V1$SSP126585 <- apply(X = Occ_V1[,3:8], MARGIN = 1, FUN = sum)
Occ_V1$SSP126585[Occ_V1$SSP126585 >= 1] <- 1
Occ_V1$SSP126 <- apply(X = Occ_V1[,3:5], MARGIN = 1, FUN = sum)
Occ_V1$SSP126[Occ_V1$SSP126 >= 1] <- 1
Occ_V1$SSP585 <- apply(X = Occ_V1[,6:8], MARGIN = 1, FUN = sum)
Occ_V1$SSP585[Occ_V1$SSP585 >= 1] <- 1
datOccHYr_V1$Sig <- Occ_V1$SSP126585
datOcc126_1040Yr_V1$Sig <- Occ_V1$SSP126
datOcc126_4070Yr_V1$Sig <- Occ_V1$SSP126
datOcc126_7000Yr_V1$Sig <- Occ_V1$SSP126
datOcc585_1040Yr_V1$Sig <- Occ_V1$SSP585
datOcc585_4070Yr_V1$Sig <- Occ_V1$SSP585
datOcc585_7000Yr_V1$Sig <- Occ_V1$SSP585

# Var 3
Occ_V3 <- Occ_V3 %>%
  select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig, 
         SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
Occ_V3$SSP126585 <- 0
Occ_V3$SSP126 <- 0
Occ_V3$SSP585 <- 0
Occ_V3$SSP126585 <- apply(X = Occ_V3[,3:8], MARGIN = 1, FUN = sum)
Occ_V3$SSP126585[Occ_V3$SSP126585 >= 1] <- 1
Occ_V3$SSP126 <- apply(X = Occ_V3[,3:5], MARGIN = 1, FUN = sum)
Occ_V3$SSP126[Occ_V3$SSP126 >= 1] <- 1
Occ_V3$SSP585 <- apply(X = Occ_V3[,6:8], MARGIN = 1, FUN = sum)
Occ_V3$SSP585[Occ_V3$SSP585 >= 1] <- 1
datOccHYr_V3$Sig <- Occ_V3$SSP126585
datOcc126_1040Yr_V3$Sig <- Occ_V3$SSP126
datOcc126_4070Yr_V3$Sig <- Occ_V3$SSP126
datOcc126_7000Yr_V3$Sig <- Occ_V3$SSP126
datOcc585_1040Yr_V3$Sig <- Occ_V3$SSP585
datOcc585_4070Yr_V3$Sig <- Occ_V3$SSP585
datOcc585_7000Yr_V3$Sig <- Occ_V3$SSP585

# Var 4
Occ_V4 <- Occ_V4 %>%
  select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig, 
         SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
Occ_V4$SSP126585 <- 0
Occ_V4$SSP126 <- 0
Occ_V4$SSP585 <- 0
Occ_V4$SSP126585 <- apply(X = Occ_V4[,3:8], MARGIN = 1, FUN = sum)
Occ_V4$SSP126585[Occ_V4$SSP126585 >= 1] <- 1
Occ_V4$SSP126 <- apply(X = Occ_V4[,3:5], MARGIN = 1, FUN = sum)
Occ_V4$SSP126[Occ_V4$SSP126 >= 1] <- 1
Occ_V4$SSP585 <- apply(X = Occ_V4[,6:8], MARGIN = 1, FUN = sum)
Occ_V4$SSP585[Occ_V4$SSP585 >= 1] <- 1
datOccHYr_V4$Sig <- Occ_V4$SSP126585
datOcc126_1040Yr_V4$Sig <- Occ_V4$SSP126
datOcc126_4070Yr_V4$Sig <- Occ_V4$SSP126
datOcc126_7000Yr_V4$Sig <- Occ_V4$SSP126
datOcc585_1040Yr_V4$Sig <- Occ_V4$SSP585
datOcc585_4070Yr_V4$Sig <- Occ_V4$SSP585
datOcc585_7000Yr_V4$Sig <- Occ_V4$SSP585

# . . 4.3.4 Formatting ---------------------------------------------------------
# Var 1
datOccHYr_V1 <- gather(datOccHYr_V1, key = Year, value = Occurrences, 3:33)
datOcc126_1040Yr_V1 <- gather(datOcc126_1040Yr_V1, key = Year, 
                              value =Occurrences, 3:33)
datOcc126_4070Yr_V1 <- gather(datOcc126_4070Yr_V1, key = Year, 
                              value =Occurrences, 3:33)
datOcc126_7000Yr_V1 <- gather(datOcc126_7000Yr_V1, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_1040Yr_V1 <- gather(datOcc585_1040Yr_V1, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_4070Yr_V1 <- gather(datOcc585_4070Yr_V1, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_7000Yr_V1 <- gather(datOcc585_7000Yr_V1, key = Year, 
                              value =Occurrences, 3:33)
datOccHYr_V1$Scenario <- 'Historic'
datOcc126_1040Yr_V1$Scenario <- 'SSP126'
datOcc126_4070Yr_V1$Scenario <- 'SSP126'
datOcc126_7000Yr_V1$Scenario <- 'SSP126'
datOcc585_1040Yr_V1$Scenario <- 'SSP585'
datOcc585_4070Yr_V1$Scenario <- 'SSP585'
datOcc585_7000Yr_V1$Scenario <- 'SSP585'
datOccYr_V1 <- rbind(datOccHYr_V1, datOcc126_1040Yr_V1, datOcc126_4070Yr_V1, 
                     datOcc126_7000Yr_V1, datOcc585_1040Yr_V1,
                     datOcc585_4070Yr_V1, datOcc585_7000Yr_V1)
datOccYr_V1$Year <- as.integer(datOccYr_V1$Year)

# Var 3
datOccHYr_V3 <- gather(datOccHYr_V3, key = Year, value = Occurrences, 3:33)
datOcc126_1040Yr_V3 <- gather(datOcc126_1040Yr_V3, key = Year, 
                              value =Occurrences, 3:33)
datOcc126_4070Yr_V3 <- gather(datOcc126_4070Yr_V3, key = Year, 
                              value =Occurrences, 3:33)
datOcc126_7000Yr_V3 <- gather(datOcc126_7000Yr_V3, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_1040Yr_V3 <- gather(datOcc585_1040Yr_V3, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_4070Yr_V3 <- gather(datOcc585_4070Yr_V3, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_7000Yr_V3 <- gather(datOcc585_7000Yr_V3, key = Year, 
                              value =Occurrences, 3:33)
datOccHYr_V3$Scenario <- 'Historic'
datOcc126_1040Yr_V3$Scenario <- 'SSP126'
datOcc126_4070Yr_V3$Scenario <- 'SSP126'
datOcc126_7000Yr_V3$Scenario <- 'SSP126'
datOcc585_1040Yr_V3$Scenario <- 'SSP585'
datOcc585_4070Yr_V3$Scenario <- 'SSP585'
datOcc585_7000Yr_V3$Scenario <- 'SSP585'
datOccYr_V3 <- rbind(datOccHYr_V3, datOcc126_1040Yr_V3, datOcc126_4070Yr_V3, 
                     datOcc126_7000Yr_V3, datOcc585_1040Yr_V3,
                     datOcc585_4070Yr_V3, datOcc585_7000Yr_V3)
datOccYr_V3$Year <- as.integer(datOccYr_V3$Year)

# Var 4
# Get 2010 data of future
datH_2010 <- cbind(datOccHYr_V4$lon, datOccHYr_V4$lat, datOccHYr_V4$`2010`,
                   Occ_V4$SSP126585)
dat126_2010 <- cbind(datOcc126_1040Yr_V4$lon, datOcc126_1040Yr_V4$lat, 
                     datOcc126_1040Yr_V4$`2010`, Occ_V4$SSP126585)
dat585_2010 <- cbind(datOcc585_1040Yr_V4$lon, datOcc585_1040Yr_V4$lat, 
                     datOcc585_1040Yr_V4$`2010`, Occ_V4$SSP126585)

datOccHYr_V4 <- gather(datOccHYr_V4, key = Year, value = Occurrences, 3:33)
datOcc126_1040Yr_V4 <- gather(datOcc126_1040Yr_V4, key = Year, 
                              value =Occurrences, 3:33)
datOcc126_4070Yr_V4 <- gather(datOcc126_4070Yr_V4, key = Year, 
                              value =Occurrences, 3:33)
datOcc126_7000Yr_V4 <- gather(datOcc126_7000Yr_V4, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_1040Yr_V4 <- gather(datOcc585_1040Yr_V4, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_4070Yr_V4 <- gather(datOcc585_4070Yr_V4, key = Year, 
                              value =Occurrences, 3:33)
datOcc585_7000Yr_V4 <- gather(datOcc585_7000Yr_V4, key = Year, 
                              value =Occurrences, 3:33)
datOccHYr_V4$Scenario <- 'Historic'
datOcc126_1040Yr_V4$Scenario <- 'SSP126'
datOcc126_4070Yr_V4$Scenario <- 'SSP126'
datOcc126_7000Yr_V4$Scenario <- 'SSP126'
datOcc585_1040Yr_V4$Scenario <- 'SSP585'
datOcc585_4070Yr_V4$Scenario <- 'SSP585'
datOcc585_7000Yr_V4$Scenario <- 'SSP585'
datOccYr_V4 <- rbind(datOccHYr_V4, datOcc126_1040Yr_V4, datOcc126_4070Yr_V4, 
                     datOcc126_7000Yr_V4, datOcc585_1040Yr_V4,
                     datOcc585_4070Yr_V4, datOcc585_7000Yr_V4)
datOccYr_V4 <- rbind(datOccYr_V4, 
                     cbind(lon = datH_2010[,1], lat = datH_2010[,2], Sig = datH_2010[,4], 
                           Year = '2010', Occurrences = datH_2010[,3], Scenario = 'SSP126'),
                     cbind(lon = datH_2010[,1], lat = datH_2010[,2], Sig = datH_2010[,4], 
                           Year = '2010', Occurrences = datH_2010[,3], Scenario = 'SSP585'),
                     cbind(lon = dat126_2010[,1], lat = dat126_2010[,2], Sig = dat126_2010[,4],
                           Year = '2010',Occurrences = dat126_2010[,3], Scenario = 'Historic'),
                     cbind(lon = dat126_2010[,1], lat = dat126_2010[,2], Sig = dat126_2010[,4], 
                           Year = '2010', Occurrences = dat126_2010[,3], Scenario = 'SSP585'),
                     cbind(lon = dat585_2010[,1], lat = dat585_2010[,2], Sig = dat585_2010[,4], 
                           Year = '2010', Occurrences = dat585_2010[,3], Scenario = 'Historic'),
                     cbind(lon = dat585_2010[,1], lat = dat585_2010[,2], Sig = dat585_2010[,4],
                           Year = '2010', Occurrences = dat585_2010[,3], Scenario = 'SSP126'))
datOccYr_V4$Year <- as.integer(datOccYr_V4$Year)
datOccYr_V4$Occurrences <- as.numeric(datOccYr_V4$Occurrences)

# . . 4.3.5 Plotting time series -----------------------------------------------
a <- paste0('Occurrences of ', varT[1])
p1 <- ggplot(data = datOccYr_V1[datOccYr_V1$Sig == 1,], 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.5) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='darkgreen','SSP126'='orange',
                               'SSP585' = 'red'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="bottom")

a <- paste0('Occurrences of ', varT[3])
p2 <- ggplot(data = datOccYr_V3[datOccYr_V3$Sig == 1,], 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.5) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='darkgreen','SSP126'='orange',
                               'SSP585' = 'red'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

a <- paste0('Occurrences of ', varT[4])
p3 <- ggplot(data = datOccYr_V4[datOccYr_V4$Sig == 1,], 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.5) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='darkgreen','SSP126'='orange',
                               'SSP585' = 'red'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

myLegend <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1 <- plot_grid(p1,
                p2,
                p3,
                nrow = 3,
                rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0('Global Significant Change'), fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                rel_heights = c(.05,1, 0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/','OCCYr_TIMESER_', timeSpan, 
                             'AllSig', ".tiff"),
       width = 9, height = 6, dpi = 350, bg='white')


# . . 4.3.6 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'lat1', 'lat2','lon1','lon2',
                           'location','locTitle','mFileH','mFile126','mFile585',
                           'var', 'varT', 'baseData', 'timeSpan',
                           'get_legend','as_ggplot','mean_cl_quantile','acf2')])
# . 4.4 Moving average Time Series ---------------------------------------------
# . . 4.4.1 Variables Needed ---------------------------------------------------
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')
a <-strsplit(loc2,'/') %>% unlist()
yr <- 1980:2100

datOcc126_V1 <- matrix(data = 0, nrow = 8, ncol = 121)
colnames(datOcc126_V1) <- yr
datOcc585_V1 <- datOcc126_V1
datOcc126_V3 <- datOcc126_V1
datOcc585_V3 <- datOcc126_V1
datOcc126_V4 <- datOcc126_V1
datOcc585_V4 <- datOcc126_V1

# . . 4.4.2 Opening Files Needed -----------------------------------------------
#Var 1
Occ_V1 <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var[1],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOcc126Yr_V1 <- cbind(read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[1],'_Hist_8010', 
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP126_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP126_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP126_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
datOcc585Yr_V1 <- cbind(read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[1],'_Hist_8010',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP585_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP585_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP585_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))

#Var 3
Occ_V3 <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var[3],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOcc126Yr_V3 <- cbind(read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[3],'_Hist_8010', 
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP126_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP126_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP126_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
datOcc585Yr_V3 <- cbind(read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[3],'_Hist_8010',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP585_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP585_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP585_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
#Var 4
Occ_V4 <- read_csv(paste0(fileloc1,'Results/','OCC_CHANG_',var[4],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOcc126Yr_V4 <- cbind(read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[4],'_Hist_8010', 
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP126_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP126_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP126_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
datOcc585Yr_V4 <- cbind(read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[4],'_Hist_8010',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))

# . . 4.4.3 Significance ---------------------------------------------------------
# Var 1
Occ_V1 <- Occ_V1 %>%
  select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig, 
         SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
Occ_V1$SSP126 <- 0
Occ_V1$SSP585 <- 0
Occ_V1$SSP126 <- apply(X = Occ_V1[,3:5], MARGIN = 1, FUN = sum)
Occ_V1$SSP126[Occ_V1$SSP126 >= 1] <- 1
Occ_V1$SSP585 <- apply(X = Occ_V1[,6:8], MARGIN = 1, FUN = sum)
Occ_V1$SSP585[Occ_V1$SSP585 >= 1] <- 1
Occ_V1$SSP126[Occ_V1$SSP126 == 0] <- NA
Occ_V1$SSP585[Occ_V1$SSP585 == 0] <- NA
datOcc126Yr_V1 <- cbind(datOcc126Yr_V1$lon, datOcc126Yr_V1$lat, 
                        datOcc126Yr_V1[,3:dim(datOcc126Yr_V1)[2]] * Occ_V1$SSP126)
datOcc126Yr_V1 <- na.omit(datOcc126Yr_V1)
datOcc585Yr_V1 <- cbind(datOcc585Yr_V1$lon, datOcc585Yr_V1$lat, 
                        datOcc585Yr_V1[,3:dim(datOcc585Yr_V1)[2]] * Occ_V1$SSP585)
datOcc585Yr_V1 <- na.omit(datOcc585Yr_V1)

# Var 3
Occ_V3 <- Occ_V3 %>%
  select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig, 
         SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
Occ_V3$SSP126 <- 0
Occ_V3$SSP585 <- 0
Occ_V3$SSP126 <- apply(X = Occ_V3[,3:5], MARGIN = 1, FUN = sum)
Occ_V3$SSP126[Occ_V3$SSP126 >= 1] <- 1
Occ_V3$SSP585 <- apply(X = Occ_V3[,6:8], MARGIN = 1, FUN = sum)
Occ_V3$SSP585[Occ_V3$SSP585 >= 1] <- 1
Occ_V3$SSP126[Occ_V3$SSP126 == 0] <- NA
Occ_V3$SSP585[Occ_V3$SSP585 == 0] <- NA
datOcc126Yr_V3 <- cbind(datOcc126Yr_V3$lon, datOcc126Yr_V3$lat, 
                        datOcc126Yr_V3[,3:dim(datOcc126Yr_V3)[2]] * Occ_V3$SSP126)
datOcc126Yr_V3 <- na.omit(datOcc126Yr_V3)
datOcc585Yr_V3 <- cbind(datOcc585Yr_V3$lon, datOcc585Yr_V3$lat, 
                        datOcc585Yr_V3[,3:dim(datOcc585Yr_V3)[2]] * Occ_V3$SSP585)
datOcc585Yr_V3 <- na.omit(datOcc585Yr_V3)

# Var 4
Occ_V4 <- Occ_V4 %>%
  select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig, 
         SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
Occ_V4$SSP126 <- 0
Occ_V4$SSP585 <- 0
Occ_V4$SSP126 <- apply(X = Occ_V4[,3:5], MARGIN = 1, FUN = sum)
Occ_V4$SSP126[Occ_V4$SSP126 >= 1] <- 1
Occ_V4$SSP585 <- apply(X = Occ_V4[,6:8], MARGIN = 1, FUN = sum)
Occ_V4$SSP585[Occ_V4$SSP585 >= 1] <- 1
Occ_V4$SSP126[Occ_V4$SSP126 == 0] <- NA
Occ_V4$SSP585[Occ_V4$SSP585 == 0] <- NA
datOcc126Yr_V4 <- cbind(datOcc126Yr_V4$lon, datOcc126Yr_V4$lat, 
                        datOcc126Yr_V4[,3:dim(datOcc126Yr_V4)[2]] * Occ_V4$SSP126)
datOcc126Yr_V4 <- na.omit(datOcc126Yr_V4)
datOcc585Yr_V4 <- cbind(datOcc585Yr_V4$lon, datOcc585Yr_V4$lat, 
                        datOcc585Yr_V4[,3:dim(datOcc585Yr_V4)[2]] * Occ_V4$SSP585)
datOcc585Yr_V4 <- na.omit(datOcc585Yr_V4)

# . . 4.4.4 Statistics ---------------------------------------------------------
# . . . 4.4.4.1 Model Mu ---
for (i in 1:length(loc2)){
  # Var1
  dat <- select(datOcc126Yr_V1, c(paste0(a[i], '_', yr)))
  datOcc126_V1[i,] <- apply(dat, MARGIN = 2, FUN = mean) %>% t() 
  dat <- select(datOcc585Yr_V1, c(paste0(a[i], '_', yr)))
  datOcc585_V1[i,] <- apply(dat, MARGIN = 2, FUN = mean) %>% t() 
  
  # Var3
  dat <- select(datOcc126Yr_V3, c(paste0(a[i], '_', yr)))
  datOcc126_V3[i,] <- apply(dat, MARGIN = 2, FUN = mean) %>% t() 
  dat <- select(datOcc585Yr_V3, c(paste0(a[i], '_', yr)))
  datOcc585_V3[i,] <- apply(dat, MARGIN = 2, FUN = mean) %>% t() 
  
  # Var4
  dat <- select(datOcc126Yr_V4, c(paste0(a[i], '_', yr)))
  datOcc126_V4[i,] <- apply(dat, MARGIN = 2, FUN = mean) %>% t() 
  dat <- select(datOcc585Yr_V4, c(paste0(a[i], '_', yr)))
  datOcc585_V4[i,] <- apply(dat, MARGIN = 2, FUN = mean) %>% t() 
  
}

# . . . 4.4.4.1 Model Moving average ---
# Var 1
datOcc126_V1 <- apply(X = datOcc126_V1, MARGIN = 1, FUN=zoo::rollmean,
                      k = 10, align = 'center', fill = NA) %>%
  t()
datOcc585_V1 <- apply(X = datOcc585_V1, MARGIN = 1, FUN=zoo::rollmean,
                      k = 10, align = 'center', fill = NA) %>%
  t()
# Var 3
datOcc126_V3 <- apply(X = datOcc126_V3, MARGIN = 1, FUN=zoo::rollmean,
                      k = 10, align = 'center', fill = NA) %>%
  t()
datOcc585_V3 <- apply(X = datOcc585_V3, MARGIN = 1, FUN=zoo::rollmean,
                      k = 10, align = 'center', fill = NA) %>%
  t()
# Var 4
datOcc126_V4 <- apply(X = datOcc126_V4, MARGIN = 1, FUN=zoo::rollmean,
                      k = 10, align = 'center', fill = NA) %>%
  t()
datOcc585_V4 <- apply(X = datOcc585_V4, MARGIN = 1, FUN=zoo::rollmean,
                      k = 10, align = 'center', fill = NA) %>%
  t()


# . . . 4.4.4.3 Standard deviation & Overall Mu ---
# Var1
sd <- apply(X = datOcc126_V1, MARGIN = 2, FUN = sd)
mu <- apply(X = datOcc126_V1, MARGIN = 2, FUN = mean)
datOcc_V1 <- tibble('Year' = yr, 'Mean' = mu, 'SdPlus' = mu + sd, 'SdNeg' = mu - sd,
                    'Scenario' = 'SSP126')
sd <- apply(X = datOcc585_V1, MARGIN = 2, FUN = sd)
mu <- apply(X = datOcc585_V1, MARGIN = 2, FUN = mean)
dat <- tibble('Year' = yr, 'Mean' = mu, 'SdPlus' = mu + sd, 'SdNeg' = mu - sd,
              'Scenario' = 'SSP585')
datOcc_V1 <- datOcc_V1 %>%
  rbind(dat)

# Var3
sd <- apply(X = datOcc126_V3, MARGIN = 2, FUN = sd)
mu <- apply(X = datOcc126_V3, MARGIN = 2, FUN = mean)
datOcc_V3 <- tibble('Year' = yr, 'Mean' = mu, 'SdPlus' = mu + sd, 'SdNeg' = mu - sd,
                    'Scenario' = 'SSP126')
sd <- apply(X = datOcc585_V3, MARGIN = 2, FUN = sd)
mu <- apply(X = datOcc585_V3, MARGIN = 2, FUN = mean)
dat <- tibble('Year' = yr, 'Mean' = mu, 'SdPlus' = mu + sd, 'SdNeg' = mu - sd,
              'Scenario' = 'SSP585')
datOcc_V3 <- datOcc_V3 %>%
  rbind(dat)

# Var4
sd <- apply(X = datOcc126_V4, MARGIN = 2, FUN = sd)
mu <- apply(X = datOcc126_V4, MARGIN = 2, FUN = mean)
datOcc_V4 <- tibble('Year' = yr, 'Mean' = mu, 'SdPlus' = mu + sd, 'SdNeg' = mu - sd,
                    'Scenario' = 'SSP126')
sd <- apply(X = datOcc585_V4, MARGIN = 2, FUN = sd)
mu <- apply(X = datOcc585_V4, MARGIN = 2, FUN = mean)
dat <- tibble('Year' = yr, 'Mean' = mu, 'SdPlus' = mu + sd, 'SdNeg' = mu - sd,
              'Scenario' = 'SSP585')
datOcc_V4 <- datOcc_V4 %>%
  rbind(dat)

# . . 4.4.5 Plotting -----------------------------------------------------------
ggplot(data = datOcc_V1, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, fill = factor(Scenario))) +
  geom_line() +
  geom_ribbon(alpha=0.3)
ggplot(data = datOcc_V3, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, fill = factor(Scenario))) +
  geom_line() +
  geom_ribbon(alpha=0.3)
ggplot(data = datOcc_V4, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, fill = factor(Scenario))) +
  geom_line() +
  geom_ribbon(alpha=0.3)

a <- paste0('Occurrences of ', varT[1])
p1 <- ggplot(data = datOcc_V1, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, 
                                   fill = factor(Scenario))) +
  theme_bw() +
  geom_ribbon(alpha=0.3) +
  geom_line(aes(color =  factor(Scenario))) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                                'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="bottom")

a <- paste0('Occurrences of ', varT[3])
p2 <- ggplot(data = datOcc_V3, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, 
                                   fill = factor(Scenario))) +
  theme_bw() +
  geom_ribbon(alpha=0.3) +
  geom_line(aes(color =  factor(Scenario))) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                                'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

a <- paste0('Occurrences of ', varT[4])
p3 <- ggplot(data = datOcc_V4, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, 
                                   fill = factor(Scenario))) +
  theme_bw() +
  geom_ribbon(alpha=0.3) +
  geom_line(aes(color =  factor(Scenario))) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                                'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

myLegend <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1 <- plot_grid(p1,
                p2,
                p3,
                nrow = 3,
                rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0('Significant Global Change '), fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                rel_heights = c(.05,1, 0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/FD/','OCCYr_TIMESER_', timeSpan, 
                             'Significant', ".tiff"),
       width = 9, height = 6, dpi = 350, bg='white')



# Part V Length of Occurances ##################################################
# . 5.1 Variables Needed -------------------------------------------------------
varNum <- 6
var <- c('tasmax','tasmin','pr','mrsos','SIM14','SEQ14','SEQ41','SIM13','SEQ13',
         'SEQ31','SEQ34','SEQ43')[varNum]
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')[varNum]

datLen <- tibble(
  'lon' = numeric(length = 12476),
  'lat' = numeric(length = 12476),
  'Historical_Mu' = numeric(length = 12476),
  'Historical_Var' = numeric(length = 12476),
  'SSP126_1040_Mu' = numeric(length = 12476),
  'SSP126_1040_Var' = numeric(length = 12476),
  'SSP126_1040_Delta' = numeric(length = 12476),
  'SSP126_1040_Sig' = numeric(length = 12476),
  'SSP126_4070_Mu' = numeric(length = 12476),
  'SSP126_4070_Var' = numeric(length = 12476),
  'SSP126_4070_Delta' = numeric(length = 12476),
  'SSP126_4070_Sig' = numeric(length = 12476),
  'SSP126_7000_Mu' = numeric(length = 12476),
  'SSP126_7000_Var' = numeric(length = 12476),
  'SSP126_7000_Delta' = numeric(length = 12476),
  'SSP126_7000_Sig' = numeric(length = 12476),
  
  'SSP585_1040_Mu' = numeric(length = 12476),
  'SSP585_1040_Var' = numeric(length = 12476),
  'SSP585_1040_Delta' = numeric(length = 12476),
  'SSP585_1040_Sig' = numeric(length = 12476),
  'SSP585_4070_Mu' = numeric(length = 12476),
  'SSP585_4070_Var' = numeric(length = 12476),
  'SSP585_4070_Delta' = numeric(length = 12476),
  'SSP585_4070_Sig' = numeric(length = 12476),
  'SSP585_7000_Mu' = numeric(length = 12476),
  'SSP585_7000_Var' = numeric(length = 12476),
  'SSP585_7000_Delta' = numeric(length = 12476),
  'SSP585_7000_Sig' = numeric(length = 12476)
) 
# Mu : average/mean   
# Var : variance
# Delta : difference with historic period  
# Sig : Significance
dat <- tibble(
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
  'Mu' = numeric(length = 12476),
  "Positive" = numeric(length = 12476),
  "Negative" = numeric(length = 12476)
)
# . 5.2 Opening Files ----------------------------------------------------------
datLenH <- read_csv(paste0(fileloc1, loc1[1], 'Results/', 
                           'OCC_DAY_', var, '_Hist_8010','_length.csv'),
                    col_names = TRUE, cols(.default = col_double()))
datLen126_1040 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                  'OCC_DAY_', var, '_SSP126_1040','_length.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLen126_4070 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                  'OCC_DAY_', var, '_SSP126_4070','_length.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLen126_7000 <- read_csv(paste0(fileloc1, loc1[2], 'Results/',
                                  'OCC_DAY_', var, '_SSP126_7000','_length.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLen585_1040 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                  'OCC_DAY_', var, '_SSP585_1040','_length.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLen585_4070 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                  'OCC_DAY_', var, '_SSP585_4070','_length.csv'),
                           col_names = TRUE, cols(.default = col_double()))
datLen585_7000 <- read_csv(paste0(fileloc1, loc1[3], 'Results/',
                                  'OCC_DAY_', var, '_SSP585_7000','_length.csv'),
                           col_names = TRUE, cols(.default = col_double()))

# . . 5.2.1 Calculate the Significance of Change -------------------------------
# . . . 5.2.1.1 Historical - - - - - - - - - - - - - - - - - - - - - - - - - - -
datLen$lon <- datLenH$lon
datLen$lat <- datLenH$lat
datLen$Historical_Mu <- datLenH$Mu
datLen$Historical_Var <- apply(datLenH[,3:10], MARGIN = 1, FUN=stats::var)
# . . . 5.2.1.2 SSP126_1040 - - - - - - - - - - - - - - - - - - - - - - - - -
# . . . . 5.2.1.2.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datLen126_1040[,2+i] - datLenH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
datLen$SSP126_1040_Delta <- dat$Mu
datLen$SSP126_1040_Mu <- datLen126_1040$Mu
datLen$SSP126_1040_Var <- apply(datLen126_1040[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 5.2.1.2.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
datLen$SSP126_1040_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 5.2.1.3 SSP126_4070 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 5.2.1.3.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datLen126_4070[,2+i] - datLenH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
datLen$SSP126_4070_Delta <- dat$Mu
datLen$SSP126_4070_Mu <- datLen126_4070$Mu
datLen$SSP126_4070_Var <- apply(datLen126_4070[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 5.2.1.3.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
datLen$SSP126_4070_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1
# . . . 5.2.1.4 SSP126_7000 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 5.2.1.4.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datLen126_7000[,2+i] - datLenH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
datLen$SSP126_7000_Delta <- dat$Mu
datLen$SSP126_7000_Mu <- datLen126_7000$Mu
datLen$SSP126_7000_Var <- apply(datLen126_7000[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 5.2.1.4.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
datLen$SSP126_7000_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 5.2.1.5 SSP585_1040 - - - - - - - - - - - - - - - - - - - - - - - - -
# . . . . 5.2.1.5.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datLen585_1040[,2+i] - datLenH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
datLen$SSP585_1040_Delta <- dat$Mu
datLen$SSP585_1040_Mu <- datLen585_1040$Mu
datLen$SSP585_1040_Var <- apply(datLen585_1040[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 5.2.1.5.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
datLen$SSP585_1040_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 5.2.1.6 SSP585_4070 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 5.2.1.6.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datLen585_4070[,2+i] - datLenH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
datLen$SSP585_4070_Delta <- dat$Mu
datLen$SSP585_4070_Mu <- datLen585_4070$Mu
datLen$SSP585_4070_Var <- apply(datLen585_4070[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 6.2.1.6.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
datLen$SSP585_4070_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . . 5.2.1.7 SSP585_7000 - - - - - - - - - - - - - - - - - - - - - - - - - - 
# . . . . 5.2.1.7.a Difference ---
for (i in 1:length(loc2)){
  dat[,2+i] <- datLen585_7000[,2+i] - datLenH[,2+i]
}
dat$Mu <- apply(dat[,3:10], MARGIN = 1, function(x){
  sum(x)/length(x)
})
datLen$SSP585_7000_Delta <- dat$Mu
datLen$SSP585_7000_Mu <- datLen585_7000$Mu
datLen$SSP585_7000_Var <- apply(datLen585_7000[,3:10], MARGIN = 1, FUN=stats::var)
# . . . . 5.2.1.7.b Significance ---
dat$Positive <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x > 0))
})
dat$Negative <- apply(dat[,3:10], MARGIN = 1, function(x){
  length(which(x < 0))
})
datLen$SSP585_7000_Sig[dat$Positive >= 6 | dat$Negative >= 6] <- 1

# . . 5.2.1.8 Saving - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
write.csv(datLen,file=paste0(fileloc1,'Results/','OCC_CHANG_',var,'_length.csv'),
          row.names = FALSE)


# END ##########################################################################