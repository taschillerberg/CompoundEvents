# P7_Exposure_Figures.R
# About: This program will open the exceedance file for the selected 
#       variable and calculate a 'wave' of when the exceedance occurs.
#
# Inputs: COMP
# Outputs: COMPOUND Figures
#
# T. A. Schillerberg
#               Feb. 2022
#      Updated: Oct. 2023

fileloc1 <- 'C:/Research/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)

# Part I Variables To Change ###################################################
compNum  <- 2
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')[compNum]
compTitle <- c('Simultanious Heat & Flash Drought','Sequential Heat & Flash Drought',
               'Sequential Flash Drought & Heat','Simultanious Heat & Extreme Precip',
               'Sequential Heat & Extreme Precip','Sequential Precipitation & Heat',
               'Sequential Extreme Precip & Flash Drought',
               'Sequential Flash Drought & Extreme Precip')[compNum]

loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')
timeStep <- c('DAY_','WEEK_FD_','WEEK_D_','MONTH_FD_','MONTH_D_')[1]

locTitle <- c('Oceania', 'N. South America', 'China', 'North America', 'Europe')
lon1 <- c(93, -81, 94, -102, -10) 
lon2 <- c(153, -63, 124, -66, 50)
lat1 <- c(10, 10, 40, 52, 56)
lat2 <- c(-10, -14, 20, 26, 37)
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")

print(paste0('Compound Event ',comp, ' - ', compTitle))
print('Rscript: P7_Exposure_Figures.R')
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
# Part III -- Global Change in Exposure ########################################
# . 3.1 Change in Population ---------------------------------------------------
# . . 3.1.1 Opening Files ------------------------------------------------------
datPop <- read_csv(paste0(fileloc1,'Results/','EXPOSURE_POP_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . . . 3.1.1.1 Limits & Formating ---
maxLimt <- cbind(datPop$SSP126_1040_delta, datPop$SSP126_4070_delta, 
                 datPop$SSP126_7000_delta, datPop$SSP585_1040_delta,
                 datPop$SSP585_4070_delta, datPop$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datPop$SSP126_1040_delta, datPop$SSP126_4070_delta, 
                 datPop$SSP126_7000_delta, datPop$SSP585_1040_delta,
                 datPop$SSP585_4070_delta, datPop$SSP585_7000_delta) %>%
  min()
names <- colnames(datPop)
datPop <- cbind(datPop, datComp$SSP126_1040_Sig, datComp$SSP126_4070_Sig, 
                datComp$SSP126_7000_Sig, datComp$SSP585_1040_Sig,
                datComp$SSP585_4070_Sig, datComp$SSP585_7000_Sig)
colnames(datPop) <- c(names, 'SSP126_1040_Sig', 'SSP126_4070_Sig', 
                      'SSP126_7000_Sig', 'SSP585_1040_Sig',
                      'SSP585_4070_Sig', 'SSP585_7000_Sig')
# . . 3.1.2 Plotting -----------------------------------------------------------
a <- "SSP126 Early-Century - Historical"
p1 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP126_1040_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Mid-Century - Historical"
p2 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Late-Century - Historical"
p3 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
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

a <- "SSP585 Early-Century - Historical"
p4 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP585_1040_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Mid-Century - Historical"
p5 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Late-Century - Historical"
p6 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_7000_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
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
title <- ggdraw() + draw_label(paste0("Change in population exposure due to ",
                                      compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_CHANGE_POP_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 6.5, dpi = 350, bg='white')

# . . 3.1.3 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('as_ggplot','get_legend','fileloc1', 'timeStep',
                           'loc1', 'loc2', 'mFileH', 'mFile126', 'mFile585', 
                           'compNum', 'comp', 'compTitle', 'baseData')])
# . 3.2 Change in Agriculture Exposure -----------------------------------------
# . . 3.2.1 Opening Files ------------------------------------------------------
datAg <- read_csv(paste0(fileloc1,'Results/','EXPOSURE_AG_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . . . 3.2.1.1 Limit & Formating ---
maxLimt <- cbind(datAg$SSP126_1040_delta, datAg$SSP126_4070_delta, 
                 datAg$SSP126_7000_delta, datAg$SSP585_1040_delta,
                 datAg$SSP585_4070_delta, datAg$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datAg$SSP126_1040_delta, datAg$SSP126_4070_delta, 
                 datAg$SSP126_7000_delta, datAg$SSP585_1040_delta,
                 datAg$SSP585_4070_delta, datAg$SSP585_7000_delta) %>%
  min()
names <- colnames(datAg)
datAg <- cbind(datAg, datComp$SSP126_1040_Sig, datComp$SSP126_4070_Sig, 
               datComp$SSP126_7000_Sig, datComp$SSP585_1040_Sig,
               datComp$SSP585_4070_Sig, datComp$SSP585_7000_Sig)
colnames(datAg) <- c(names, 'SSP126_1040_Sig', 'SSP126_4070_Sig', 
                     'SSP126_7000_Sig', 'SSP585_1040_Sig',
                     'SSP585_4070_Sig', 'SSP585_7000_Sig')

# . . 3.2 Plotting ---------------------------------------------------------------
a <- "SSP126 Early-Century - Historical"
p1 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP126_1040_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Mid-Century - Historical"
p2 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
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

a <- "SSP126 Late-Century - Historical"
p3 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
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

a <- "SSP585 Early-Century - Historical"
p4 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP585_1040_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Mid-Century - Historical"
p5 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Late-Century - Historical"
p6 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
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

title <- ggdraw() + draw_label(paste0("Change in Agriculture Land (km2) exposure due to ",
                                      compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_CHANGE_AG_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 6.5, dpi = 350, bg='white')

# . . 3.2.3 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('as_ggplot','get_legend','fileloc1', 'timeStep',
                           'loc1', 'loc2', 'mFileH', 'mFile126', 'mFile585', 
                           'compNum', 'comp', 'compTitle', 'baseData')])
# . 3.3 Change in Forest Exposure ----------------------------------------------
# . . 3.3.1 Opening File -------------------------------------------------------
datFor <- read_csv(paste0(fileloc1,'Results/','EXPOSURE_FOR_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_COMP_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . . 3.3.1.1 Limits & Formating ---
maxLimt <- cbind(datFor$SSP126_1040_delta, datFor$SSP126_4070_delta, 
                 datFor$SSP126_7000_delta, datFor$SSP585_1040_delta,
                 datFor$SSP585_4070_delta, datFor$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datFor$SSP126_1040_delta, datFor$SSP126_4070_delta, 
                 datFor$SSP126_7000_delta, datFor$SSP585_1040_delta,
                 datFor$SSP585_4070_delta, datFor$SSP585_7000_delta) %>%
  min()
names <- colnames(datFor)
datFor <- cbind(datFor, datComp$SSP126_1040_Sig, datComp$SSP126_4070_Sig, 
                datComp$SSP126_7000_Sig, datComp$SSP585_1040_Sig,
                datComp$SSP585_4070_Sig, datComp$SSP585_7000_Sig)
colnames(datFor) <- c(names, 'SSP126_1040_Sig', 'SSP126_4070_Sig', 
                      'SSP126_7000_Sig', 'SSP585_1040_Sig',
                      'SSP585_4070_Sig', 'SSP585_7000_Sig')

# . . 3.3 Plotting ---------------------------------------------------------------
a <- "SSP126 Early-Century - Historical"
p1 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP126_1040_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP126 Mid-Century - Historical"
p2 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
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

a <- "SSP126 Late-Century - Historical"
p3 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
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

a <- "SSP585 Early-Century - Historical"
p4 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP585_1040_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Mid-Century - Historical"
p5 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP585_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = a, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = 'NULL') +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

a <- "SSP585 Late-Century - Historical"
p6 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", 
                       na.value = 'lightblue', # trans = "log",
                       direction = -1, name = 'Difference of Exposure') +
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

title <- ggdraw() + draw_label(paste0("Change in Forest Land (km2) exposure due to ",
                                      compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_CHANGE_FOR_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 6.5, dpi = 350, bg='white')
# . . 3.3.3 Remove -------------------------------------------------------------
rm(list=ls()[! ls() %in% c('as_ggplot','get_legend','fileloc1', 'timeStep',
                           'loc1', 'loc2', 'mFileH', 'mFile126', 'mFile585', 
                           'compNum', 'comp', 'compTitle', 'baseData')])

# Part IV -- Lolli Change ######################################################
# . 4.1 Variables Needed -------------------------------------------------------
datChang <- tibble(
  'Exposure' = numeric(length = 15),
  'Region' = numeric(length = 15),
  'SSP126_1040' = numeric(length = 15),
  'SSP126_4070' = numeric(length = 15),
  'SSP126_7000' = numeric(length = 15),
  'SSP585_1040' = numeric(length = 15),
  'SSP585_4070' = numeric(length = 15),
  'SSP585_7000' = numeric(length = 15)
)

# . 4.2 Opening Files ----------------------------------------------------------
datPop <- read_csv(paste0(fileloc1,'Results/','EXPOSURE_POP_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datAg <- read_csv(paste0(fileloc1,'Results/','EXPOSURE_AG_',comp,'.csv'),
                  col_names = TRUE, cols(.default = col_double()))
datFor <- read_csv(paste0(fileloc1,'Results/','EXPOSURE_FOR_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))

# . 4.3 Pre-processing Regions -------------------------------------------------
for (i in 1:length(locTitle)){
  lonA <- lon1[i]
  lonB <- lon2[i]
  latA <- lat1[i]
  latB <- lat2[i]
  datP <- datPop
  datP$lon[datP$lon < lonA | datP$lon > lonB] <- NA
  datP$lat[datP$lat > latA | datP$lat < latB] <- NA
  datP <- na.omit(datP)
  datA <- datAg
  datA$lon[datA$lon < lonA | datA$lon > lonB] <- NA
  datA$lat[datA$lat > latA | datA$lat < latB] <- NA
  datA <- na.omit(datA)
  datF <- datFor
  datF$lon[datF$lon < lonA | datF$lon > lonB] <- NA
  datF$lat[datF$lat > latA | datF$lat < latB] <- NA
  datF <- na.omit(datF)
  
  datChang$Region[(3 * (i-1)) + 1] <- locTitle[i]
  datChang$Exposure[(3 * (i-1)) + 1] <- 'Population'
  datChang$SSP126_1040[(3 * (i-1)) + 1] <- mean(datP$SSP126_1040_delta)
  datChang$SSP126_4070[(3 * (i-1)) + 1] <- mean(datP$SSP126_4070_delta)
  datChang$SSP126_7000[(3 * (i-1)) + 1] <- mean(datP$SSP126_7000_delta)
  datChang$SSP585_1040[(3 * (i-1)) + 1] <- mean(datP$SSP585_1040_delta)
  datChang$SSP585_4070[(3 * (i-1)) + 1] <- mean(datP$SSP585_4070_delta)
  datChang$SSP585_7000[(3 * (i-1)) + 1] <- mean(datP$SSP585_7000_delta)
  
  datChang$Region[(3 * (i-1)) + 2] <- locTitle[i]
  datChang$Exposure[(3 * (i-1)) + 2] <- 'Agriculture'
  datChang$SSP126_1040[(3 * (i-1)) + 2] <- mean(datA$SSP126_1040_delta)
  datChang$SSP126_4070[(3 * (i-1)) + 2] <- mean(datA$SSP126_4070_delta)
  datChang$SSP126_7000[(3 * (i-1)) + 2] <- mean(datA$SSP126_7000_delta)
  datChang$SSP585_1040[(3 * (i-1)) + 2] <- mean(datA$SSP585_1040_delta)
  datChang$SSP585_4070[(3 * (i-1)) + 2] <- mean(datA$SSP585_4070_delta)
  datChang$SSP585_7000[(3 * (i-1)) + 2] <- mean(datA$SSP585_7000_delta)
  
  datChang$Region[(3 * (i-1)) + 3] <- locTitle[i]
  datChang$Exposure[(3 * (i-1)) + 3] <- 'Forestry'
  datChang$SSP126_1040[(3 * (i-1)) + 3] <- mean(datF$SSP126_1040_delta)
  datChang$SSP126_4070[(3 * (i-1)) + 3] <- mean(datF$SSP126_4070_delta)
  datChang$SSP126_7000[(3 * (i-1)) + 3] <- mean(datF$SSP126_7000_delta)
  datChang$SSP585_1040[(3 * (i-1)) + 3] <- mean(datF$SSP585_1040_delta)
  datChang$SSP585_4070[(3 * (i-1)) + 3] <- mean(datF$SSP585_4070_delta)
  datChang$SSP585_7000[(3 * (i-1)) + 3] <- mean(datF$SSP585_7000_delta)
}
datChang$Region <- factor(datChang$Region)

# . 4.4 Plotting ---------------------------------------------------------------
dat <- matrix(0, nrow = 6, ncol = 3) %>%
  tibble()
colnames(dat) <- c('Scenario','XX1','YY2')
dat[,2] <- c(2,3,4,5,3,2)
dat[,3] <- c(3,4,5,4,3,2)
dat$Scenario <- factor(colnames(datChang[3:8]))
colnames(dat) <- c('Scenario','XX1','YY2')
myLegend <- ggplot(dat, aes(x=XX1 ,y=YY2, color = Scenario)) +
  theme_bw() +
  geom_point() + 
  scale_color_manual(values = c('SSP126_1040'='yellow1',
                                'SSP126_4070'="gold",
                                'SSP126_7000'="darkgoldenrod2",
                                'SSP585_1040'="darkorange1",
                                'SSP585_4070'='orangered2',
                                'SSP585_7000'='red3'))
myLegend <- get_legend(myLegend, position = 'bottom') %>% 
  as_ggplot()

dat <- datChang[datChang$Exposure == 'Population',]
a <- 'Population Change'
p1 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_1040, yend=SSP126_4070), 
                color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_4070, yend=SSP126_7000), 
                color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_7000, yend=SSP585_1040), 
                color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_1040, yend=SSP585_4070), 
                color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_4070, yend=SSP585_7000), 
                color="grey") +
  geom_point(aes(x=Region, y=SSP126_1040), color='yellow1', size=3, alpha = 0.5) +
  geom_point(aes(x=Region, y=SSP126_4070), color='gold', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_7000), color='darkgoldenrod2', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_1040), color='darkorange1', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_4070), color='orangered2', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_7000), color='red3', size=3, alpha = 0.5 ) +
  coord_flip() +
  labs(x = "", y = "", title = a)

dat <- datChang[datChang$Exposure == 'Agriculture',]
a <- 'Agriculture exposure change from historical (km2)'
p2 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_1040, yend=SSP126_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_4070, yend=SSP126_7000), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_7000, yend=SSP585_1040), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_1040, yend=SSP585_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_4070, yend=SSP585_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP126_1040), color='yellow1', size=3, alpha = 0.5) +
  geom_point(aes(x=Region, y=SSP126_4070), color='gold', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_7000), color='darkgoldenrod2', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_1040), color='darkorange1', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_4070), color='orangered2', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_7000), color='red3', size=3, alpha = 0.5 ) +
  coord_flip() +
  labs(x = "", y = "", title = a)

dat <- datChang[datChang$Exposure == 'Forestry',]
a <- 'Forestry exposure change from historical (km2)'
p3 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_1040, yend=SSP126_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_4070, yend=SSP126_7000), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_7000, yend=SSP585_1040), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_1040, yend=SSP585_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_4070, yend=SSP585_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP126_1040), color='yellow1', size=3, alpha = 0.5) +
  geom_point(aes(x=Region, y=SSP126_4070), color='gold', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_7000), color='darkgoldenrod2', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_1040), color='darkorange1', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_4070), color='orangered2', size=3, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_7000), color='red3', size=3, alpha = 0.5 ) +
  coord_flip() +
  labs(x = "", y = "", title = a)

F1A <- plot_grid(p1,
                 p2, 
                 p3, 
                 myLegend,
                 nrow = 4,
                 rel_heights = c(1,1,1,0.2))

title <- ggdraw() + draw_label(paste0("Exposure to ", compTitle), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                rel_heights = c(.05,1),
                nrow = 2)

ggsave(F1, filename = paste(fileloc1,'Results/','EXPOSURE_CHANGE_', comp, ".tiff", sep=''),
       width = 6.5, height = 6.5, dpi = 350, bg='white')


# END ##########################################################################