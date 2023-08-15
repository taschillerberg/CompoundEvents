# P7_Figures.R
# About: This program will open the exceedance file for the selected 
#       variable and calculate a 'wave' of when the exceedance occurs.
#
# Inputs: COMP
# Outputs: COMPOUND Figures
#
# T. A. Schillerberg
#               Feb. 2022
#      Updated: Jul. 2023

# Mac

# Office Computer
setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
fileloc <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/'
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# setwd("/Users/taschillerberg/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Code2")
# fileloc1 <- "/Users/taschillerberg/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Data/"

# HPC
# fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)
library(colorspace)

# Part I Variables To Change ##############################################
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/')
timeStep <- c('DAY_','WEEK_FD_','WEEK_D_','MONTH_FD_','MONTH_D_')[1]
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')
compTitle <- c('Simultanious Heatwave & Drought','Sequential Heatwave & Drought',
               'Sequential Drought & Heatwave','Simultanious Heat & Extreme Precip',
               'Sequential Heatwave & Extreme Precip','Sequential Precipitation & Heatwave',
               'Sequential Extreme Precip & Drought',
               'Sequential Drought & Extreme Precip')
baseData <- map_data('world')

print(paste0('Model ',loc2))
print('Rscript: P8a_Compound_Month_Testing.R')
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

# Part III Figure 1 -- Compound Events Relative Change #########################
# . 3.1 Variables to change ----------------------------------------------------
compNum  <- 1
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')[compNum]
compTitle <- c('Simultanious Heat & Flash Drought','Sequential Heat & Flash Drought',
               'Sequential Flash Drought & Heat','Simultanious Heat & Extreme Precip',
               'Sequential Heat & Extreme Precip','Sequential Precipitation & Heat',
               'Sequential Extreme Precip & Flash Drought',
               'Sequential Flash Drought & Extreme Precip')[compNum]
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")
# . 3.2 Opening File -----------------------------------------------------------
dat <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_',comp,'.csv'),
                col_names = TRUE, cols(.default = col_double()))
# . . 3.2.1 Formatting ---------------------------------------------------------
maxLimt <- cbind(dat$SSP126_4070_delta, dat$SSP126_7000_delta, 
                 dat$SSP585_4070_delta, dat$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(dat$SSP126_4070_delta, dat$SSP126_7000_delta, 
                 dat$SSP585_4070_delta, dat$SSP585_7000_delta) %>%
  min()

# . 3.3 Plotting ---------------------------------------------------------------
p1 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
  theme_bw() +
  geom_tile() +
  # scale_fill_continuous_divergingx(limits = c(minLimt,maxLimt),
  #                                  palette = 'BrBG', mid = 0, rev = TRUE,
  #                                  name = 'Difference of Events') +
  # scale_fill_distiller(limits = c(minLimt,maxLimt), palette = "PuOr", direction = -1)
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p4 <- ggplot(data = dat, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_point(alpha = 1, shape = 47,
             aes(size=ifelse(SSP126_4070_Sig == 0,'dot', 'no_dot'))) +
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

F1A <- plot_grid(p1, p2,
                 p3, p4, 
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))

title <- ggdraw() + draw_label(paste0("Relative change of ", compTitle), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc,'Results/','FIG1_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9.5, dpi = 350, bg='white')




# Part IV Figure 3 -- Population Exposure ######################################
# . 4.1 Variables to change ----------------------------------------------------
compNum  <- 8
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')[compNum]
compTitle <- c('Simultanious Heat & Flash Drought','Sequential Heat & Flash Drought',
               'Sequential Flash Drought & Heat','Simultanious Heat & Extreme Precip',
               'Sequential Heat & Extreme Precip','Sequential Precipitation & Heat',
               'Sequential Extreme Precip & Flash Drought',
               'Sequential Flash Drought & Extreme Precip')[compNum]
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")
# . 4.2 Opening File -----------------------------------------------------------
datPOP <- read_csv(paste0(fileloc1,'Results/','EXPPOP_',comp,'.csv'),
                col_names = TRUE, cols(.default = col_double()))
# . . 4.2.1 Formatting ---------------------------------------------------------
maxLimt <- cbind(datPOP$SSP126_4070_delta, datPOP$SSP126_7000_delta, 
                 datPOP$SSP585_4070_delta, datPOP$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datPOP$SSP126_4070_delta, datPOP$SSP126_7000_delta, 
                 datPOP$SSP585_4070_delta, datPOP$SSP585_7000_delta) %>%
  min()
# . 4.3 Plotting ---------------------------------------------------------------
p1 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p4 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
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

F1A <- plot_grid(p1, p2,
                 p3, p4, 
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))

title <- ggdraw() + draw_label(paste0("Relative exposure change of Population and ", compTitle), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc,'Results/','FIG3_POP_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9, dpi = 350, bg='white')

# Part V Figure 5 -- Agriculture Exposure ######################################
# . 5.1 Variables to change ----------------------------------------------------
compNum  <- 8
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')[compNum]
compTitle <- c('Simultanious Heat & Flash Drought','Sequential Heat & Flash Drought',
               'Sequential Flash Drought & Heat','Simultanious Heat & Extreme Precip',
               'Sequential Heat & Extreme Precip','Sequential Precipitation & Heat',
               'Sequential Extreme Precip & Flash Drought',
               'Sequential Flash Drought & Extreme Precip')[compNum]
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")
# . 5.2 Opening File -----------------------------------------------------------
datAG <- read_csv(paste0(fileloc1,'Results/','EXPAG_', comp, ".tiff"),
                   col_names = TRUE, cols(.default = col_double()))
# . . 4.2.1 Formatting ---------------------------------------------------------
maxLimt <- cbind(datAG$SSP126_4070_delta, datAG$SSP126_7000_delta, 
                 datAG$SSP585_4070_delta, datAG$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datAG$SSP126_4070_delta, datAG$SSP126_7000_delta, 
                 datAG$SSP585_4070_delta, datAG$SSP585_7000_delta) %>%
  min()
# . 4.3 Plotting ---------------------------------------------------------------
p1 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-84,90), expand = FALSE) +
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p4 <- ggplot(data = datPOP, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis_c(limits = c(minLimt,maxLimt), option = "rocket", na.value = 'lightblue',
                       direction = -1, name = 'Difference of Events') +
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

F1A <- plot_grid(p1, p2,
                 p3, p4, 
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))

title <- ggdraw() + draw_label(paste0("Relative exposure change of Population and ", compTitle), fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.05),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc,'Results/','FIG5_POP_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9, dpi = 350, bg='white')

















# END ##########################################################################

