# P7_Exposure_Figures.R
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
# setwd("/Users/taschillerberg/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Code2")
# fileloc1 <- "/Users/taschillerberg/Library/CloudStorage/OneDrive-AuburnUniversity/Research/FEMAResearch/Data/"

# Office Computer
setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

options(show.error.locations = TRUE)
# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)

# Part I Variables To Change ###################################################
compNum  <- 8
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
# Part III -- Change in Population Exposure ####################################
# . 3.1 Opening File -----------------------------------------------------------
datPop <- read_csv(paste0(fileloc1,'Results/','EXPPOP_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . . 3.1.2 Formating ---
maxLimt <- cbind(datPop$SSP126_4070_delta, datPop$SSP126_7000_delta, 
                 datPop$SSP585_4070_delta, datPop$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datPop$SSP126_4070_delta, datPop$SSP126_7000_delta, 
                 datPop$SSP585_4070_delta, datPop$SSP585_7000_delta) %>%
  min()
names <- colnames(datPop)
datPop <- cbind(datPop, datComp$SSP126_4070_Sig, datComp$SSP126_7000_Sig,
                datComp$SSP585_4070_Sig, datComp$SSP585_7000_Sig)
colnames(datPop) <- c(names, 'SSP126_4070_Sig', 'SSP126_7000_Sig', 
                      'SSP585_4070_Sig', 'SSP585_7000_Sig')

# . 3.2 Plotting ---------------------------------------------------------------
p1 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
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
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
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
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
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

p4 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
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

F1A <- plot_grid(p1, p2,
                 p3, p4, 
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))

title <- ggdraw() + draw_label(paste0("Change in population exposure due to ",
                                      compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','Exp_Pop_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9.5, dpi = 350, bg='white')

# Part IV -- Change in Agriculture Exposure ####################################
# . 3.1 Opening File -----------------------------------------------------------
datAg <- read_csv(paste0(fileloc1,'Results/','EXPAg_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . . 3.1.2 Formating ---
maxLimt <- cbind(datAg$SSP126_4070_delta, datAg$SSP126_7000_delta, 
                 datAg$SSP585_4070_delta, datAg$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datAg$SSP126_4070_delta, datAg$SSP126_7000_delta, 
                 datAg$SSP585_4070_delta, datAg$SSP585_7000_delta) %>%
  min()
names <- colnames(datAg)
datAg <- cbind(datAg, datComp$SSP126_4070_Sig, datComp$SSP126_7000_Sig,
                datComp$SSP585_4070_Sig, datComp$SSP585_7000_Sig)
colnames(datAg) <- c(names, 'SSP126_4070_Sig', 'SSP126_7000_Sig', 
                      'SSP585_4070_Sig', 'SSP585_7000_Sig')

# . 3.2 Plotting ---------------------------------------------------------------
p1 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
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
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
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
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
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

p4 <- ggplot(data = datAg, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
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

F1A <- plot_grid(p1, p2,
                 p3, p4, 
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))

title <- ggdraw() + draw_label(paste0("Change in Agriculture Land exposure due to ",
                                      compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','Exp_Ag_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9.5, dpi = 350, bg='white')

# Part IV -- Change in Forest Exposure #########################################
# . 3.1 Opening File -----------------------------------------------------------
datFor <- read_csv(paste0(fileloc1,'Results/','EXPFor_',comp,'.csv'),
                   col_names = TRUE, cols(.default = col_double()))
datComp <- read_csv(paste0(fileloc1,'Results/','MU_CHANG_',comp,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))

# . . 3.1.2 Formating ---
maxLimt <- cbind(datFor$SSP126_4070_delta, datFor$SSP126_7000_delta, 
                 datFor$SSP585_4070_delta, datFor$SSP585_7000_delta) %>%
  max()
minLimt <- cbind(datFor$SSP126_4070_delta, datFor$SSP126_7000_delta, 
                 datFor$SSP585_4070_delta, datFor$SSP585_7000_delta) %>%
  min()
names <- colnames(datFor)
datFor <- cbind(datFor, datComp$SSP126_4070_Sig, datComp$SSP126_7000_Sig,
                datComp$SSP585_4070_Sig, datComp$SSP585_7000_Sig)
colnames(datFor) <- c(names, 'SSP126_4070_Sig', 'SSP126_7000_Sig', 
                      'SSP585_4070_Sig', 'SSP585_7000_Sig')

# . 3.2 Plotting ---------------------------------------------------------------
p1 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP126_4070_delta)) +
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
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p2 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP126_7000_delta)) +
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
  labs(title = NULL, 
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "NULL") +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))

p3 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP585_4070_delta)) +
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

p4 <- ggplot(data = datFor, aes(x=lon, y=lat, fill=SSP585_7000_delta)) +
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

F1A <- plot_grid(p1, p2,
                 p3, p4, 
                 nrow = 2,
                 # labels = c('A','B','C','D'),
                 rel_widths = c(1,1))

title <- ggdraw() + draw_label(paste0("Change in Forest Land Cover exposure due to ",
                                      compTitle), 
                               fontface='bold')
F1 <- plot_grid(title,
                F1A,
                myLegend,
                rel_heights = c(.05,1,0.07),
                # rel_heights = c(0.05,1),
                nrow = 3)

ggsave(F1, filename = paste(fileloc1,'Results/','Exp_For_COMP_',comp, ".tiff", sep=''),
       width = 14, height = 9.5, dpi = 350, bg='white')

# END ##########################################################################