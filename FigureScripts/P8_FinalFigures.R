# P8_FinalFigures.R
# About: This script will create plots for the publication.
# 
# Inputs:
# Outputs: Figures and Supplemental Figures
#
# T. A. Schillerberg
#               Nov. 2023
#      Updated: Nov. 2023
#
#
# Office Computer
setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/'
# Mac
# setwd("~/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Code2")
# fileloc1 <- "~/OneDrive - Auburn University/Research/FEMAResearch/Data/"

# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)

# Part 1 Variables ####################################################################
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')
timeSpan <- c('DAY_', 'WEEK_', 'WEEK_FD_','WEEK_D_', 
              'MONTH_', 'MONTH_FD_','MONTH_D_')[1]

var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
varT <- c('Heatwaves','Coldwaves','Extreme Precipitation','Flash Drought')
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')
compT <- c('Simultanious Heat & Flash Drought','Sequential Heat & Flash Drought',
           'Sequential Flash Drought & Heat','Simultanious Heat & Extreme Precip',
           'Sequential Heat & Extreme Precip','Sequential Precipitation & Heat',
           'Sequential Extreme Precip & Flash Drought',
            'Sequential Flash Drought & Extreme Precip')

locT <- c('Oceania', 'South America', 'China', 'North America', 'Europe')
lon1 <- c( 93, -82,  94, -125, -10)
lon2 <- c(153, -34, 124,  -66,  50)
lat1 <- c( 10,  13,  45,   55,  56)
lat2 <- c(-10, -35,  20,   23,  36)
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")
options(show.error.locations = TRUE)
yr <- 1980:2100

# Part 2 Functions #############################################################
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

# Part 3 - Figure 1 & Sup 1 ####################################################
# . 3.1 Variables Needed -------------------------------------------------------
relV1 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'.csv'), 
                    col_names = TRUE, cols(.default = col_double()))
relV3 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[3],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV4 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'.csv'), 
                     col_names = TRUE, cols(.default = col_double()))

# . 3.2 Plotting limits --------------------------------------------------------
maxLimitV1 <- rbind(relV1$SSP126_7000_Delta, relV1$SSP585_7000_Delta) %>%
  max()
minLimitV1 <- rbind(relV1$SSP126_7000_Delta, relV1$SSP585_7000_Delta) %>%
  min()
maxLimitV3 <- rbind(relV3$SSP126_7000_Delta, relV3$SSP585_7000_Delta) %>%
  max()
minLimitV3 <- rbind(relV3$SSP126_7000_Delta, relV3$SSP585_7000_Delta) %>%
  min()
maxLimitV4 <- rbind(relV4$SSP126_7000_Delta, relV4$SSP585_7000_Delta) %>%
  max()
minLimitV4 <- rbind(relV4$SSP126_7000_Delta, relV4$SSP585_7000_Delta) %>%
  min()

# . 3.3 Plotting Change 7000 ---------------------------------------------------
a <- "SSP126 Late-Century - Historical"
p1 <- ggplot(data = relV1, aes(x=lon, y=lat, fill=SSP126_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV1, maxLimitV1), option = "rocket",
                       direction = -1, name = '') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP126_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'A', x = '', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm"))

p2 <- ggplot(data = relV3, aes(x=lon, y=lat, fill=SSP126_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV3, maxLimitV3), option = "rocket",
                       direction = -1, name = '') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP126_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'B', x = '', y = '') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm"))

p3 <- ggplot(data = relV4, aes(x=lon, y=lat, fill=SSP126_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV4, maxLimitV4), option = "rocket",
                       direction = -1, name = '') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP126_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'C', x = '', y = '') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.05, b = 0.0, l = 0.0, unit="cm"))

a <- "SSP585 Late-Century - Historical"
p4 <- ggplot(data = relV1, aes(x=lon, y=lat, fill=SSP585_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV1, maxLimitV1), option = "rocket",
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP585_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'D', x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin = margin(t = 0.0, r = 0.1, b = 0.0, l = 0.0, unit="cm"))

p5 <- ggplot(data = relV3, aes(x=lon, y=lat, fill=SSP585_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV3, maxLimitV3), option = "rocket",
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP585_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'E', x = 'Longitude', y = '') +
  theme(legend.position="NULL") +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm"))

p6 <- ggplot(data = relV4, aes(x=lon, y=lat, fill=SSP585_7000_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV4, maxLimitM), option = "rocket",
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP585_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'F', x = 'Longitude', y = '') +
  theme(legend.position="NULL") +
  theme(plot.margin = margin(t = 0.0, r = 0.05, b = 0.0, l = 0.0, unit="cm"))

myLegendT <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
myLegendP <- get_legend(p2, position = 'bottom') %>% 
  as_ggplot()
p2 <- p2 + theme(legend.position = "NULL")
myLegendM <- get_legend(p3, position = 'bottom') %>% 
  as_ggplot()
p3 <- p3 + theme(legend.position = "NULL")

F1 <- plot_grid(p1, p2, p3,
                p4, p5, p6, 
                myLegendT, myLegendP, myLegendM,
                nrow = 3,
                # labels = c('A','B','C','D', 'E', 'F', NULL, NULL, NULL),
                rel_widths = c(1, 1, 1),
                rel_heights = c(1, 1, .3))
title <- ggdraw() + draw_label(paste0("Change of ", varT), fontface='bold')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_1', ".tiff"),
       width = 14, height = 6.1, dpi = 375, bg='white')

# . 3.4 Supp Plotting Limits ---------------------------------------------------
maxLimitV1 <- rbind(relV1$SSP126_4070_Delta, relV1$SSP585_4070_Delta,
                   relV1$SSP126_7000_Delta, relV1$SSP585_7000_Delta) %>%
  max()
minLimitV1 <- rbind(relV1$SSP126_4070_Delta, relV1$SSP585_4070_Delta,
                   relV1$SSP126_7000_Delta, relV1$SSP585_7000_Delta) %>%
  min()
maxLimitV3 <- rbind(relV3$SSP126_4070_Delta, relV3$SSP585_4070_Delta,
                   relV3$SSP126_7000_Delta, relV3$SSP585_7000_Delta) %>%
  max()
minLimitV3 <- rbind(relV3$SSP126_4070_Delta, relV3$SSP585_4070_Delta,
                   relV3$SSP126_7000_Delta, relV3$SSP585_7000_Delta) %>%
  min()
maxLimitM <- rbind(relV4$SSP126_4070_Delta, relV4$SSP585_4070_Delta,
                   relV4$SSP126_7000_Delta, relV4$SSP585_7000_Delta) %>%
  max()
minLimitV4 <- rbind(relV4$SSP126_4070_Delta, relV4$SSP585_4070_Delta,
                   relV4$SSP126_7000_Delta, relV4$SSP585_7000_Delta) %>%
  min()

# . 3.5 Supp Plotting Change 4070 ----------------------------------------------
a <- "SSP126 Mid-Century - Historical"
p1 <- ggplot(data = relV1, aes(x=lon, y=lat, fill=SSP126_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV1, maxLimitV1), option = "rocket",
                       direction = -1, name = '') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'A', x = '', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm"))

p2 <- ggplot(data = relV3, aes(x=lon, y=lat, fill=SSP126_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV3, maxLimitV3), option = "rocket",
                       direction = -1, name = '') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'B', x = '', y = '') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm"))

p3 <- ggplot(data = relV4, aes(x=lon, y=lat, fill=SSP126_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV4, maxLimitM), option = "rocket",
                       direction = -1, name = '') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'C', x = '', y = '') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.05, b = 0.0, l = 0.0, unit="cm"))

a <- "SSP585 Mid-Century - Historical"
p4 <- ggplot(data = relV1, aes(x=lon, y=lat, fill=SSP585_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV1, maxLimitV1), option = "rocket",
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'D', x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin = margin(t = 0.0, r = 0.1, b = 0.0, l = 0.0, unit="cm"))

p5 <- ggplot(data = relV3, aes(x=lon, y=lat, fill=SSP585_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV3, maxLimitV3), option = "rocket",
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'E', x = 'Longitude', y = '') +
  theme(legend.position="NULL") +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm"))

p6 <- ggplot(data = relV4, aes(x=lon, y=lat, fill=SSP585_4070_Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimitV4, maxLimitM), option = "rocket",
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 0.5, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = 'F', x = 'Longitude', y = '') +
  theme(legend.position="NULL") +
  theme(plot.margin = margin(t = 0.0, r = 0.05, b = 0.0, l = 0.0, unit="cm"))

myLegendT <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")
myLegendP <- get_legend(p2, position = 'bottom') %>% 
  as_ggplot()
p2 <- p2 + theme(legend.position = "NULL")
myLegendM <- get_legend(p3, position = 'bottom') %>% 
  as_ggplot()
p3 <- p3 + theme(legend.position = "NULL")

F1 <- plot_grid(p1, p2, p3,
                p4, p5, p6, 
                myLegendT, myLegendP, myLegendM,
                nrow = 3,
                # labels = c('A','B','C','D', 'E', 'F', NULL, NULL, NULL),
                rel_widths = c(1, 1, 1),
                rel_heights = c(1, 1, .3))
title <- ggdraw() + draw_label(paste0("Change of ", varT), fontface='bold')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/Supplement/',
                             'SUPP_FIG_1', ".tiff"),
       width = 14, height = 6.1, dpi = 375, bg='white')

# . 3.6 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'timeSpan', 'var', 'varT', 
                           'comp', 'compT', 'locT', 'lon1', 'lon2', 
                           'lat1', 'lat2', 'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile'
                           )])

# Part 4 - Figure 2 ############################################################
# . 4.1 Variables Needed -------------------------------------------------------
#Var 1
Occ_V1 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOccHYr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                'OCCYr_DAY_', var[1],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 3
Occ_V3 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[3],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOccHYr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/',
                                'OCCYr_DAY_', var[3],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 4
Occ_V4 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOccHYr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/',
                                'OCCYr_DAY_', var[4],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# . 4.2 Determine Significance -------------------------------------------------
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

# . 4.3 Formatting -------------------------------------------------------------
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
datH_2010 <- cbind(datOccHYr_V3$lon, datOccHYr_V3$lat, datOccHYr_V3$`2010`,
                   Occ_V3$SSP126585)
dat126_2010 <- cbind(datOcc126_1040Yr_V3$lon, datOcc126_1040Yr_V3$lat, 
                     datOcc126_1040Yr_V3$`2010`, Occ_V3$SSP126585)
dat585_2010 <- cbind(datOcc585_1040Yr_V3$lon, datOcc585_1040Yr_V3$lat, 
                     datOcc585_1040Yr_V3$`2010`, Occ_V3$SSP126585)

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
datOccYr_V3 <- rbind(datOccYr_V3, 
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
datOccYr_V3$Year <- as.integer(datOccYr_V3$Year)
datOccYr_V3$Occurrences <- as.numeric(datOccYr_V3$Occurrences)

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

# . 4.4 Plotting time series -----------------------------------------------
a <- paste0('Occurrences of ', varT[1])
p1 <- ggplot(data = datOccYr_V1[datOccYr_V1$Sig == 1,], 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.2, 
               aes(color = Scenario)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = 'A', x = NULL, y = 'Occurrences') +
  theme(legend.position="bottom")

a <- paste0('Occurrences of ', varT[3])
p2 <- ggplot(data = datOccYr_V3[datOccYr_V3$Sig == 1,], 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.2,
               aes(color = Scenario)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                              'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = 'B', x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

a <- paste0('Occurrences of ', varT[4])
p3 <- ggplot(data = datOccYr_V4[datOccYr_V4$Sig == 1,], 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.2, aes(color = Scenario)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                                'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = 'C', x = NULL, y = 'Occurrences') +
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
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_2', ".tiff"),
       width = 9, height = 6, dpi = 350, bg='white')

# . 4.5 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'timeSpan', 'var', 'varT', 
                           'comp', 'compT', 'locT', 'lon1', 'lon2', 
                           'lat1', 'lat2', 'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile'
)])

# Part 5 - Figure 2 Opt2 #######################################################
# . 5.1 Variables Needed -------------------------------------------------------
#Var 1
# Occ_V1 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'.csv'),
#                    col_names = TRUE, cols(.default = col_double()))
datOccHYr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                'OCCYr_DAY_', var[1],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V1 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[1],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 3
# Occ_V3 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[3],'.csv'),
#                    col_names = TRUE, cols(.default = col_double()))
datOccHYr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/',
                                'OCCYr_DAY_', var[3],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V3 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[3],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# Var 4
# Occ_V4 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'.csv'),
#                    col_names = TRUE, cols(.default = col_double()))
datOccHYr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/',
                                'OCCYr_DAY_', var[4],'_Hist_8010', '.csv'),
                         col_names = TRUE, cols(.default = col_double()))
datOcc126_1040Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP126_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_4070Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc126_7000Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP126_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_1040Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/', 
                                       'OCCYr_DAY_', var[4],'_SSP585_1040', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_4070Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_4070', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
datOcc585_7000Yr_V4 <- read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                       'OCCYr_DAY_', var[4],'_SSP585_7000', '.csv'),
                                col_names = TRUE, cols(.default = col_double()))
# # . 4.2 Determine Significance -------------------------------------------------
# # Var 1
# Occ_V1 <- Occ_V1 %>%
#   select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig,
#          SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
# Occ_V1$SSP126585 <- 0
# Occ_V1$SSP126 <- 0
# Occ_V1$SSP585 <- 0
# Occ_V1$SSP126585 <- apply(X = Occ_V1[,3:8], MARGIN = 1, FUN = sum)
# Occ_V1$SSP126585[Occ_V1$SSP126585 >= 1] <- 1
# Occ_V1$SSP126 <- apply(X = Occ_V1[,3:5], MARGIN = 1, FUN = sum)
# Occ_V1$SSP126[Occ_V1$SSP126 >= 1] <- 1
# Occ_V1$SSP585 <- apply(X = Occ_V1[,6:8], MARGIN = 1, FUN = sum)
# Occ_V1$SSP585[Occ_V1$SSP585 >= 1] <- 1
# datOccHYr_V1$Sig <- Occ_V1$SSP126585
# datOcc126_1040Yr_V1$Sig <- Occ_V1$SSP126
# datOcc126_4070Yr_V1$Sig <- Occ_V1$SSP126
# datOcc126_7000Yr_V1$Sig <- Occ_V1$SSP126
# datOcc585_1040Yr_V1$Sig <- Occ_V1$SSP585
# datOcc585_4070Yr_V1$Sig <- Occ_V1$SSP585
# datOcc585_7000Yr_V1$Sig <- Occ_V1$SSP585
# 
# # Var 3
# Occ_V3 <- Occ_V3 %>%
#   select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig,
#          SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
# Occ_V3$SSP126585 <- 0
# Occ_V3$SSP126 <- 0
# Occ_V3$SSP585 <- 0
# Occ_V3$SSP126585 <- apply(X = Occ_V3[,3:8], MARGIN = 1, FUN = sum)
# Occ_V3$SSP126585[Occ_V3$SSP126585 >= 1] <- 1
# Occ_V3$SSP126 <- apply(X = Occ_V3[,3:5], MARGIN = 1, FUN = sum)
# Occ_V3$SSP126[Occ_V3$SSP126 >= 1] <- 1
# Occ_V3$SSP585 <- apply(X = Occ_V3[,6:8], MARGIN = 1, FUN = sum)
# Occ_V3$SSP585[Occ_V3$SSP585 >= 1] <- 1
# datOccHYr_V3$Sig <- Occ_V3$SSP126585
# datOcc126_1040Yr_V3$Sig <- Occ_V3$SSP126
# datOcc126_4070Yr_V3$Sig <- Occ_V3$SSP126
# datOcc126_7000Yr_V3$Sig <- Occ_V3$SSP126
# datOcc585_1040Yr_V3$Sig <- Occ_V3$SSP585
# datOcc585_4070Yr_V3$Sig <- Occ_V3$SSP585
# datOcc585_7000Yr_V3$Sig <- Occ_V3$SSP585
# 
# # Var 4
# Occ_V4 <- Occ_V4 %>%
#   select(lon, lat, SSP126_1040_Sig, SSP126_4070_Sig, SSP126_7000_Sig,
#          SSP585_1040_Sig, SSP585_4070_Sig, SSP585_7000_Sig)
# Occ_V4$SSP126585 <- 0
# Occ_V4$SSP126 <- 0
# Occ_V4$SSP585 <- 0
# Occ_V4$SSP126585 <- apply(X = Occ_V4[,3:8], MARGIN = 1, FUN = sum)
# Occ_V4$SSP126585[Occ_V4$SSP126585 >= 1] <- 1
# Occ_V4$SSP126 <- apply(X = Occ_V4[,3:5], MARGIN = 1, FUN = sum)
# Occ_V4$SSP126[Occ_V4$SSP126 >= 1] <- 1
# Occ_V4$SSP585 <- apply(X = Occ_V4[,6:8], MARGIN = 1, FUN = sum)
# Occ_V4$SSP585[Occ_V4$SSP585 >= 1] <- 1
# datOccHYr_V4$Sig <- Occ_V4$SSP126585
# datOcc126_1040Yr_V4$Sig <- Occ_V4$SSP126
# datOcc126_4070Yr_V4$Sig <- Occ_V4$SSP126
# datOcc126_7000Yr_V4$Sig <- Occ_V4$SSP126
# datOcc585_1040Yr_V4$Sig <- Occ_V4$SSP585
# datOcc585_4070Yr_V4$Sig <- Occ_V4$SSP585
# datOcc585_7000Yr_V4$Sig <- Occ_V4$SSP585

# . 3.3 Formatting -------------------------------------------------------------
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
datH_2010 <- cbind(datOccHYr_V3$lon, datOccHYr_V3$lat, datOccHYr_V3$`2010`)
dat126_2010 <- cbind(datOcc126_1040Yr_V3$lon, datOcc126_1040Yr_V3$lat,
                     datOcc126_1040Yr_V3$`2010`)
dat585_2010 <- cbind(datOcc585_1040Yr_V3$lon, datOcc585_1040Yr_V3$lat,
                     datOcc585_1040Yr_V3$`2010`)

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
datOccYr_V3 <- rbind(datOccYr_V3, 
                     cbind(lon = datH_2010[,1], lat = datH_2010[,2], 
                           Year = '2010', Occurrences = datH_2010[,3], Scenario = 'SSP126'),
                     cbind(lon = datH_2010[,1], lat = datH_2010[,2], 
                           Year = '2010', Occurrences = datH_2010[,3], Scenario = 'SSP585'),
                     cbind(lon = dat126_2010[,1], lat = dat126_2010[,2],
                           Year = '2010',Occurrences = dat126_2010[,3], Scenario = 'Historic'),
                     cbind(lon = dat126_2010[,1], lat = dat126_2010[,2], 
                           Year = '2010', Occurrences = dat126_2010[,3], Scenario = 'SSP585'),
                     cbind(lon = dat585_2010[,1], lat = dat585_2010[,2], 
                           Year = '2010', Occurrences = dat585_2010[,3], Scenario = 'Historic'),
                     cbind(lon = dat585_2010[,1], lat = dat585_2010[,2],
                           Year = '2010', Occurrences = dat585_2010[,3], Scenario = 'SSP126'))
datOccYr_V3$Year <- as.integer(datOccYr_V3$Year)
datOccYr_V3$Occurrences <- as.numeric(datOccYr_V3$Occurrences)

# Var 4
# Get 2010 data of future
datH_2010 <- cbind(datOccHYr_V4$lon, datOccHYr_V4$lat, datOccHYr_V4$`2010`)
dat126_2010 <- cbind(datOcc126_1040Yr_V4$lon, datOcc126_1040Yr_V4$lat, 
                     datOcc126_1040Yr_V4$`2010`)
dat585_2010 <- cbind(datOcc585_1040Yr_V4$lon, datOcc585_1040Yr_V4$lat, 
                     datOcc585_1040Yr_V4$`2010`)

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
                     cbind(lon = datH_2010[,1], lat = datH_2010[,2], 
                           Year = '2010', Occurrences = datH_2010[,3], Scenario = 'SSP126'),
                     cbind(lon = datH_2010[,1], lat = datH_2010[,2], 
                           Year = '2010', Occurrences = datH_2010[,3], Scenario = 'SSP585'),
                     cbind(lon = dat126_2010[,1], lat = dat126_2010[,2],
                           Year = '2010',Occurrences = dat126_2010[,3], Scenario = 'Historic'),
                     cbind(lon = dat126_2010[,1], lat = dat126_2010[,2], 
                           Year = '2010', Occurrences = dat126_2010[,3], Scenario = 'SSP585'),
                     cbind(lon = dat585_2010[,1], lat = dat585_2010[,2],
                           Year = '2010', Occurrences = dat585_2010[,3], Scenario = 'Historic'),
                     cbind(lon = dat585_2010[,1], lat = dat585_2010[,2], 
                           Year = '2010', Occurrences = dat585_2010[,3], Scenario = 'SSP126'))
datOccYr_V4$Year <- as.integer(datOccYr_V4$Year)
datOccYr_V4$Occurrences <- as.numeric(datOccYr_V4$Occurrences)

# . 3.4 Plotting time series -----------------------------------------------
a <- paste0('Occurrences of ', varT[1])
p1 <- ggplot(data = datOccYr_V1, 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.2, 
               aes(color = Scenario)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                                'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = 'A', x = NULL, y = 'Occurrences') +
  theme(legend.position="bottom")

a <- paste0('Occurrences of ', varT[3])
p2 <- ggplot(data = datOccYr_V3, 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.2,
               aes(color = Scenario)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                                'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = 'B', x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

a <- paste0('Occurrences of ', varT[4])
p3 <- ggplot(data = datOccYr_V4, 
             aes(x = Year, y = Occurrences, fill = factor(Scenario))) +
  theme_bw() +
  stat_summary(geom = "line", fun = mean, linewidth = 0.2, aes(color = Scenario)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  scale_fill_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                               'SSP585' = '#951b1e'), name = 'Scenario') +
  scale_color_manual(values = c('Historic'='#004f00','SSP126'='#173c66',
                                'SSP585' = '#951b1e'), name = 'Scenario') +
  geom_vline(aes(xintercept = 2010)) +
  geom_vline(aes(xintercept = 2040)) +
  geom_vline(aes(xintercept = 2070)) +
  labs(title = 'C', x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

myLegend <- get_legend(p1, position = 'bottom') %>% 
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1 <- plot_grid(p1,
                p2,
                p3,
                nrow = 3,
                rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0('Global Change'), fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                rel_heights = c(.05,1, 0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_2_OPT2', ".tiff"),
       width = 9, height = 6, dpi = 350, bg='white')

# . 3.5 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'timeSpan', 'var', 'varT', 
                           'comp', 'compT', 'locT', 'lon1', 'lon2', 
                           'lat1', 'lat2', 'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile'
)])

# Part 6 - Figure 3 ############################################################
# . 6.1 Variables Needed -------------------------------------------------------
a <-strsplit(loc2,'/') %>% unlist()
yr <- 1980:2100

datOcc126_V1 <- matrix(data = 0, nrow = 121, ncol = 9)
colnames(datOcc126_V1) <- c('Year', a)
datOcc126_V1[,1] <- yr
datOcc585_V1 <- datOcc126_V1
datOcc126_V3 <- datOcc126_V1
datOcc585_V3 <- datOcc126_V1
datOcc126_V4 <- datOcc126_V1
datOcc585_V4 <- datOcc126_V1

# . 6.2 Opening Files ----------------------------------------------------------
#Var 1
Occ_V1 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOcc126Yr_V1 <- cbind(read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[1],'_Hist_8010', 
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP126_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP126_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP126_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
datOcc585Yr_V1 <- cbind(read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[1],'_Hist_8010',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP585_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP585_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[1],'_SSP585_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))

#Var 3
Occ_V3 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[3],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOcc126Yr_V3 <- cbind(read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[3],'_Hist_8010', 
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP126_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP126_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP126_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
datOcc585Yr_V3 <- cbind(read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[3],'_Hist_8010',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP585_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP585_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[3],'_SSP585_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
#Var 4
Occ_V4 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datOcc126Yr_V4 <- cbind(read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[4],'_Hist_8010', 
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP126_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP126_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[2], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP126_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
datOcc585Yr_V4 <- cbind(read_csv(paste0(fileloc1, 'Data/', loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[4],'_Hist_8010',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/', loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))
# . 6.3 Significance -----------------------------------------------------------
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

# . 6.4 Formatting -------------------------------------------------------------
# . . Model Mu - - -
for (i in 1:length(loc2)){
  # Var1
  dat <- select(datOcc126Yr_V1, c(paste0(a[i], '_', yr)))
  datOcc126_V1[,(1+i)] <- apply(dat, MARGIN = 2, FUN = mean)
  dat <- select(datOcc585Yr_V1, c(paste0(a[i], '_', yr)))
  datOcc585_V1[,(1+i)] <- apply(dat, MARGIN = 2, FUN = mean)
  
  # Var3
  dat <- select(datOcc126Yr_V3, c(paste0(a[i], '_', yr)))
  datOcc126_V3[,(1+i)] <- apply(dat, MARGIN = 2, FUN = mean)
  dat <- select(datOcc585Yr_V3, c(paste0(a[i], '_', yr)))
  datOcc585_V3[,(1+i)] <- apply(dat, MARGIN = 2, FUN = mean)

  # Var4
  dat <- select(datOcc126Yr_V4, c(paste0(a[i], '_', yr)))
  datOcc126_V4[,(1+i)] <- apply(dat, MARGIN = 2, FUN = mean)
  dat <- select(datOcc585Yr_V4, c(paste0(a[i], '_', yr)))
  datOcc585_V4[,(1+i)] <- apply(dat, MARGIN = 2, FUN = mean)
  
}

# . . Model Difference - - - 
datOcc126Yr_V1 <- matrix(data = 0, nrow = 91, ncol = 9)
colnames(datOcc126Yr_V1) <- c('Year', a)
datOcc126Yr_V1[,1] <- 2010:2100
datOcc585Yr_V1 <- datOcc126Yr_V1
datOcc126Yr_V3 <- datOcc126Yr_V1
datOcc585Yr_V3 <- datOcc126Yr_V1
datOcc126Yr_V4 <- datOcc126Yr_V1
datOcc585Yr_V4 <- datOcc126Yr_V1

for (i in 1:length(loc2)){
  # Var1
  datH <- datOcc126_V1[1:31, (1 + i)]
  datE <- datOcc126_V1[31:61, (1 + i)]
  datM <- datOcc126_V1[61:91, (1 + i)]
  datL <- datOcc126_V1[91:121, (1 + i)]
  datOcc126Yr_V1[1:31, (1+i)] <- datE - datH
  datOcc126Yr_V1[31:61, (1+i)] <- datM - datH
  datOcc126Yr_V1[61:91, (1+i)] <- datL - datH
  datH <- datOcc585_V1[1:31, (1 + i)]
  datE <- datOcc585_V1[31:61, (1 + i)]
  datM <- datOcc585_V1[61:91, (1 + i)]
  datL <- datOcc585_V1[91:121, (1 + i)]
  datOcc585Yr_V1[1:31, (1+i)] <- datE - datH
  datOcc585Yr_V1[31:61, (1+i)] <- datM - datH
  datOcc585Yr_V1[61:91, (1+i)] <- datL - datH
  
  # Var 3 
  datH <- datOcc126_V3[1:31, (1 + i)]
  datE <- datOcc126_V3[31:61, (1 + i)]
  datM <- datOcc126_V3[61:91, (1 + i)]
  datL <- datOcc126_V3[91:121, (1 + i)]
  datOcc126Yr_V3[1:31, (1+i)] <- datE - datH
  datOcc126Yr_V3[31:61, (1+i)] <- datM - datH
  datOcc126Yr_V3[61:91, (1+i)] <- datL - datH
  datH <- datOcc585_V3[1:31, (1 + i)]
  datE <- datOcc585_V3[31:61, (1 + i)]
  datM <- datOcc585_V3[61:91, (1 + i)]
  datL <- datOcc585_V3[91:121, (1 + i)]
  datOcc585Yr_V3[1:31, (1+i)] <- datE - datH
  datOcc585Yr_V3[31:61, (1+i)] <- datM - datH
  datOcc585Yr_V3[61:91, (1+i)] <- datL - datH
  
  # Var 4
  datH <- datOcc126_V4[1:31, (1 + i)]
  datE <- datOcc126_V4[31:61, (1 + i)]
  datM <- datOcc126_V4[61:91, (1 + i)]
  datL <- datOcc126_V4[91:121, (1 + i)]
  datOcc126Yr_V4[1:31, (1+i)] <- datE - datH
  datOcc126Yr_V4[31:61, (1+i)] <- datM - datH
  datOcc126Yr_V4[61:91, (1+i)] <- datL - datH
  datH <- datOcc585_V4[1:31, (1 + i)]
  datE <- datOcc585_V4[31:61, (1 + i)]
  datM <- datOcc585_V4[61:91, (1 + i)]
  datL <- datOcc585_V4[91:121, (1 + i)]
  datOcc585Yr_V4[1:31, (1+i)] <- datE - datH
  datOcc585Yr_V4[31:61, (1+i)] <- datM - datH
  datOcc585Yr_V4[61:91, (1+i)] <- datL - datH
}

# . . Overall Mean - - - 
datOcc126_V1 <- matrix(data = 0, nrow = 91, ncol = 3)
colnames(datOcc126_V1) <- c('Year', 'Occurances','TimePeriod')
datOcc126_V1[,1] <- 2010:2100
datOcc126_V1[1:31,3] <- 'Early-Century'
datOcc126_V1[32:61,3] <- 'Mid-Century'
datOcc126_V1[62:91,3] <- 'Late-Century'
datOcc585_V1 <- datOcc126_V1
datOcc126_V3 <- datOcc126_V1
datOcc585_V3 <- datOcc126_V1
datOcc126_V4 <- datOcc126_V1
datOcc585_V4 <- datOcc126_V1

# Var 1
datOcc126_V1[,2] <- apply(X = datOcc126Yr_V1[,2:9], MARGIN = 1, FUN = mean)
datOcc585_V1[,2] <- apply(X = datOcc585Yr_V1[,2:9], MARGIN = 1, FUN = mean)
# Var 3
datOcc126_V3[,2] <- apply(X = datOcc126Yr_V3[,2:9], MARGIN = 1, FUN = mean)
datOcc585_V3[,2] <- apply(X = datOcc585Yr_V3[,2:9], MARGIN = 1, FUN = mean)
# Var 4
datOcc126_V4[,2] <- apply(X = datOcc126Yr_V4[,2:9], MARGIN = 1, FUN = mean)
datOcc585_V4[,2] <- apply(X = datOcc585Yr_V4[,2:9], MARGIN = 1, FUN = mean)

# . . Formatting - - - 
dat126 <- tibble(
  'Year' = 2010:2100, 
  'tasmax' = as.numeric(datOcc126_V1[,2]),
  'pr' = as.numeric(datOcc126_V3[,2]), 
  'mrsos' = as.numeric(datOcc126_V4[,2]),
  'TimePeriod' = datOcc126_V1[,3])
dat585 <- tibble(
  'Year' = 2010:2100, 
  'tasmax' = as.numeric(datOcc585_V1[,2]),
  'pr' = as.numeric(datOcc585_V3[,2]), 
  'mrsos' = as.numeric(datOcc585_V4[,2]),
  'TimePeriod' = datOcc585_V1[,3])

dat585$TimePeriod <- factor(dat585$TimePeriod, 
                            levels = c('Early-Century', 'Mid-Century',
                                       'Late-Century'))
dat126$TimePeriod <- factor(dat126$TimePeriod, 
                            levels = c('Early-Century', 'Mid-Century',
                                       'Late-Century'))
# . 5.5 Plotting Scatter Plot --------------------------------------------------
# Var1 v Var4  SSP126
main <- ggplot(subset(dat585), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position="NULL") +
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat585), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat585), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip() +
  scale_fill_manual(name = "Time Period", 
                    values =c('Early-Century'='gold',
                              'Mid-Century'='darkorange1',
                              'Late-Century'='red3'))
p1 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p1 <- insert_yaxis_grob(p1, denY, grid::unit(.2, "null"), position = "right")

# Var3 v Var4    SSP126
main <- ggplot(subset(dat585), 
               aes(x = pr, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Flash Drought') +
  theme(legend.position='NULL') +
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat585), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat585), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip() +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
p2 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p2, denY, grid::unit(.2, "null"), position = "right")

# Var1 v Var3   SSP126
main <- ggplot(subset(dat585), 
               aes(x = pr, y = tasmax, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Heatwaves') +
  theme(legend.position='NULL') +
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat585), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat585), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip() +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
p3 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p3 <- insert_yaxis_grob(p3, denY, grid::unit(.2, "null"), position = "right")

main <- ggplot(subset(dat585), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position="bottom") +
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
myLegend <- get_legend(main, position = 'bottom') %>% 
  as_ggplot()

F1 <- plot_grid(p1, 
                p2,
                p3,
                nrow = 3,
                ncol = 1,
                rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0('Significant Change in extreme events - SSP585'), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                rel_heights = c(.05,1, 0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_3', ".tiff"),
       width = 5, height = 6, dpi = 350, bg='white')

# . 5.6 Sup 2 ------------------------------------------------------------------
# Var1 v Var4  SSP126
main <- ggplot(subset(dat126), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position="NULL") +
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat126), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat126), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip() +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
p1 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p1 <- insert_yaxis_grob(p1, denY, grid::unit(.2, "null"), position = "right")

# Var3 v Var4    SSP126
main <- ggplot(subset(dat126), 
               aes(x = pr, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Flash Drought') +
  theme(legend.position='NULL') +
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat126), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat126), 
               aes(x = mrsos, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip() +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
p2 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p2, denY, grid::unit(.2, "null"), position = "right")

# Var1 v Var3   SSP126
main <- ggplot(subset(dat126), 
               aes(x = pr, y = tasmax, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Extreme Precipitation', y = 'Heatwaves') +
  theme(legend.position='NULL') +
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denX <- axis_canvas(main, axis = "x") +
  geom_density(data = subset(dat126), 
               aes(x = pr, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
denY <- axis_canvas(main, axis = "y", coord_flip = TRUE) +
  geom_density(data = subset(dat126), 
               aes(x = tasmax, fill = TimePeriod),
               alpha = 0.7, size = 0.2) +
  coord_flip() +
  scale_fill_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
p3 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p3 <- insert_yaxis_grob(p3, denY, grid::unit(.2, "null"), position = "right")

main <- ggplot(subset(dat126), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position="bottom")
  scale_color_manual(name = "Time Period", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
myLegend <- get_legend(main, position = 'bottom') %>% 
  as_ggplot()

F1 <- plot_grid(p1, 
                p2,
                p3,
                nrow = 3,
                ncol = 1,
                rel_widths = c(1,1,1))
title <- ggdraw() + draw_label(paste0('Significant Change in extreme events - SSP126'),
                               fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                rel_heights = c(.05,1, 0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/Supplement/','SUPP_FIG_2', ".tiff"),
       width = 5, height = 6, dpi = 350, bg='white')

# . 6.# Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'timeSpan', 'var', 'varT', 
                           'comp', 'compT', 'locT', 'lon1', 'lon2', 
                           'lat1', 'lat2', 'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile'
)])


































# . 4.# Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'timeSpan', 'var', 'varT', 
                           'comp', 'compT', 'locT', 'lon1', 'lon2', 
                           'lat1', 'lat2', 'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile'
)])



















# END ##########################################################################