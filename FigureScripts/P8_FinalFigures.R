# P8_FinalFigures.R
# About: This script will create plots for the publication.
# 
# Inputs:
# Outputs: Figures and Supplemental Figures
#
# T. A. Schillerberg
#               Nov. 2023
#      Updated: Jul. 2024

# Computer
setwd("Source File Location") 
fileloc1 <- 'Main project folder' 

# Libraries ####################################################################
library(tidyverse)
library(cowplot)
require(gridExtra)
require(scales)

# Part 1 Variables ####################################################################
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/',
          'LUH2/', 'NASA_SEDAC_population/', 'Historical/')
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/',
          'ERA5/')
yr <- 1980:2100
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
varT <- c('Heatwave','Coldwave','Extreme Precipitation','Flash Drought')
comp <- c('SIM14','SEQ14','SEQ41','SIM13','SEQ13',
          'SEQ31','SEQ34','SEQ43')
compT <- c('Sim. Heatwave & Flash Drought','Seq. Heatwave & Flash Drought',
           'Seq. Flash Drought & Heatwave','Sim. Heatwave & Extreme Precip',
           'Seq. Heatwave & Extreme Precipitation',
           'Seq. Extreme Precip & Heatwave',
           'Seq. Extreme Precip & Flash Drought',
           'Seq. Flash Drought & Extreme Precipitation')

locT <- c('Africa','China', 'Europe', 'India ','North America', 
          'Oceania', 'South America')
locTLev <- c('Africa','China', 'Europe', 'India ','North America', 
             'Oceania', 'South America')
lon1 <- c( -17, 94, -10, 73,-125,  94, -82)
lon2 <- c( 50, 124,  50, 90, -66, 153, -34)
lat1 <- c( 30,  45,  56, 36,  55,  10,  13)
lat2 <- c(-35,  20,  36, 10,  23, -35, -35)
baseData <- map_data('world') %>% 
  filter(region != "Antarctica")

options(show.error.locations = TRUE)
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

# Part 3 - Figure 1 ############################################################
# . 3.1 Opening Files ----------------------------------------------------------
relV1 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV3 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[3],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV4 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))

# Length
# relV1L <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'_length.csv'), 
#                    col_names = TRUE, cols(.default = col_double()))
# relV4L <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'_length.csv'), 
#                    col_names = TRUE, cols(.default = col_double()))
# relSEQ14L <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_','SEQ14','_length.csv'), 
#                    col_names = TRUE, cols(.default = col_double()))

# . 3.2 Limits -----------------------------------------------------------------
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

show_col(viridis_pal(alpha=1, option='F')(6))
show_col(viridis_pal(alpha=1, option='D')(6))
show_col(viridis_pal(alpha=1, option='G')(6))
# For use if can get geom_signHatch to work
# relV1$SSP126_7000_Sig <- as.logical(relV1$SSP126_7000_Sig)
# relV1$SSP585_7000_Sig <- as.logical(relV1$SSP126_7000_Sig)
# relV3$SSP126_7000_Sig <- as.logical(relV3$SSP126_7000_Sig)
# relV3$SSP585_7000_Sig <- as.logical(relV3$SSP585_7000_Sig)
# relV4$SSP126_7000_Sig <- as.logical(relV4$SSP126_7000_Sig)
# relV4$SSP585_7000_Sig <- as.logical(relV4$SSP585_7000_Sig)

# . 3.3 Formatting -------------------------------------------------------------
datV1 <- tibble(lat = relV1$lat, lon = relV1$lon, 
                SSP126_7000_Delta = relV1$SSP126_7000_Delta,
                SSP126_7000_Sig = relV1$SSP126_7000_Sig,
                SSP585_7000_Delta = relV1$SSP585_7000_Delta,
                SSP585_7000_Sig = relV1$SSP585_7000_Sig)
datV1 <- datV1 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datV1 <- subset(datV1, select=-c(Scenario2))
datV1$Scenario[datV1$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datV1$Scenario[datV1$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datV1$Delta <- as.integer(datV1$Delta)
# Extreme Precipitation
datV3 <- tibble(lat = relV3$lat, lon = relV3$lon, 
                SSP126_7000_Delta = relV3$SSP126_7000_Delta,
                SSP126_7000_Sig = relV3$SSP126_7000_Sig,
                SSP585_7000_Delta = relV3$SSP585_7000_Delta,
                SSP585_7000_Sig = relV3$SSP585_7000_Sig)
datV3 <- datV3 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datV3 <- subset(datV3, select=-c(Scenario2))
datV3$Scenario[datV3$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datV3$Scenario[datV3$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datV3$Delta <- as.integer(datV3$Delta)
# Flash Drought 
datV4 <- tibble(lat = relV4$lat, lon = relV4$lon, 
                SSP126_7000_Delta = relV4$SSP126_7000_Delta,
                SSP126_7000_Sig = relV4$SSP126_7000_Sig,
                SSP585_7000_Delta = relV4$SSP585_7000_Delta,
                SSP585_7000_Sig = relV4$SSP585_7000_Sig)
datV4 <- datV4 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datV4 <- subset(datV4, select=-c(Scenario2))
datV4$Scenario[datV4$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datV4$Scenario[datV4$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datV4$Delta <- as.integer(datV4$Delta)

# . 3.4 Plotting Change --------------------------------------------------------
a <- paste0(varT[1],' Occurances')
p1 <- ggplot(data = datV1, aes(x=lon, y=lat, fill=Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = c(0,120,240,360,480,600,720),
                    colours =  c( "#faebdd", "#f69c73", "#e83f3f",
                                           '#a11a5b','#4c1dab','#440154'), 
                                           show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_point(alpha = 0.25, shape = 47,
             aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = 'Latitude', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0('Extreme Precip Occurances')
p2 <- ggplot(data = datV3, aes(x=lon, y=lat, fill=Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = c(-100, 0, 100,200, 300, 400, 500),
                    colours =  c( "#fde725", "#7ad151", "#22A884",
                                           '#2A788E', '#414487', '#440154'),
                                           show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_point(alpha = 0.25, shape = 47,
             aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0(varT[4],' Occurances')
p3 <- ggplot(data = datV4, aes(x = lon, y = lat, fill = Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = c(-10, -5, 0, 5, 10, 16),
                    colours =  c( "#DEF5E5", "#60CEAC", "#3497A9",
                                           '#395D9C', '#382A54'),
                                           show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_point(alpha = 0.25, shape = 47,
             aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  facet_grid(vars(Scenario)) +
  theme(strip.text = element_text(size = 7), 
        strip.background = element_blank())

F1 <- plot_grid(p1, p2, p3,
                nrow = 1, rel_heights = c(1),
                ncol = 3, rel_widths = c(.93,.93,1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'FIG_1', ".tiff"),
       width = 9, height = 4, dpi = 375, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'FIG_1_sm', ".tiff"),
       width = 9, height = 4, dpi = 90, bg='white')
# . 3.5 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])
# Part 4 - Figure 2 ############################################################
# . 4.1 Variables Needed ---------------------------------------------------
a <-strsplit(loc2,'/') %>% unlist()

datOcc126_V1 <- matrix(data = 0, nrow = 8, ncol = 121)
colnames(datOcc126_V1) <- yr
datOcc585_V1 <- datOcc126_V1
datOcc126_V3 <- datOcc126_V1
datOcc585_V3 <- datOcc126_V1
datOcc126_V4 <- datOcc126_V1
datOcc585_V4 <- datOcc126_V1

# . 4.2 Opening Files ----------------------------------------------------------
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
datOcc585Yr_V4 <- cbind(read_csv(paste0(fileloc1, 'Data/',loc1[1], 'Results/', 
                                        'OCCYr_DAY_', var[4],'_Hist_8010',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/',loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_1040',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/',loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_4070',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())),
                        read_csv(paste0(fileloc1, 'Data/',loc1[3], 'Results/',
                                        'OCCYr_DAY_', var[4],'_SSP585_7000',
                                        '_full.csv'),
                                 col_names = TRUE, cols(.default = col_double())))

# . 4.3 Significance ---------------------------------------------------------
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

# . 4.4 Statistics ---------------------------------------------------------
# . . . 4.4.1 Model Mu ---
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

# . . . 4.4.2 Model Moving average ---
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

# . . . 4.4.3 Standard deviation & Overall Mu ---
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
datOcc_V1$Event <- 'Heatwave'

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
datOcc_V3$Event <- 'ExPrecip'

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
datOcc_V4$Event <- 'FlashDrought'

# . 4.5 Plotting -----------------------------------------------------------
ggplot(data = datOcc_V1, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, fill = factor(Scenario))) +
  geom_line() +
  geom_ribbon(alpha=0.3)
ggplot(data = datOcc_V3, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, fill = factor(Scenario))) +
  geom_line() +
  geom_ribbon(alpha=0.3)
ggplot(data = datOcc_V4, aes(x = Year, y = Mean, ymin=SdNeg, ymax=SdPlus, fill = factor(Scenario))) +
  geom_line() +
  geom_ribbon(alpha=0.3)

dat <- rbind(datOcc_V1, datOcc_V3, datOcc_V4)
dat$Event <- factor(dat$Event, 
                    levels = c('HeatWave', 'ExPrecip','FlashDrought'),
                    labels = c('Heatwave','Extreme Precipitation', 'Flash Drought'))

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
  xlim(1982, 2098) +
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
  xlim(1982, 2098) +
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
  xlim(1982, 2098) +
  labs(title = a, x = NULL, y = 'Occurrences') +
  theme(legend.position="NULL")

myLegend <- get_legend(p1, position = 'bottom') %>%
  as_ggplot()
p1 <- p1 + theme(legend.position = "NULL")

F1 <- plot_grid(p1,
                p2,
                p3,
                myLegend,
                nrow = 4,
                rel_heights = c(1,1,1,0.09))
title <- ggdraw() + draw_label(paste0('Consistent Global Change '), fontface='bold')

ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_2', ".tiff"),
       width = 9, height = 6, dpi = 350, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_2_sm', ".tiff"),
       width = 9, height = 6, dpi = 75, bg='white')
# . 4.6 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])
# Part 5 - Figure 3 ############################################################
# . 5.1 Variables Needed -------------------------------------------------------
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

# . 5.2 Opening Files ----------------------------------------------------------
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
# . 5.3 Significance -----------------------------------------------------------
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

# . 5.4 Formatting -------------------------------------------------------------
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
  theme(legend.position="bottom") +
  scale_color_manual(name = "", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
# Var1 v Var4  SSP585
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
p4 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p4 <- insert_yaxis_grob(p4, denY, grid::unit(.2, "null"), position = "right")

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
p5 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p5 <- insert_yaxis_grob(p5, denY, grid::unit(.2, "null"), position = "right")

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
p6 <- insert_xaxis_grob(main, denX, grid::unit(.2, "null"), position = "top")
p6 <- insert_yaxis_grob(p6, denY, grid::unit(.2, "null"), position = "right")

main <- ggplot(subset(dat585), 
               aes(x = tasmax, y = mrsos, color = TimePeriod)) +
  theme_bw() +
  # geom_smooth(method = 'lm', se = FALSE)+
  geom_point(alpha = 0.7) +
  labs(title = NULL, x = 'Heatwaves', y = 'Flash Drought') +
  theme(legend.position="bottom") +
  scale_color_manual(name = "", 
                     values =c('Early-Century'='gold',
                               'Mid-Century'='darkorange1',
                               'Late-Century'='red3'))
myLegend <- get_legend(main, position = 'bottom') %>% 
  as_ggplot()

F1 <- plot_grid(p1, p4,
                p2, p5,
                p3, p6,
                nrow = 3, rel_heights = c(1,1,1),
                ncol = 2, rel_widths = c(1,1))

title <- ggdraw() + # draw_label(paste0('Significant Change in extreme events - SSP585'),
  draw_label(paste0('Consistent Change in Climate Extremes'), 
             fontface='bold')
F1 <- plot_grid(title,
                F1,
                myLegend,
                nrow = 3, rel_heights = c(.05,1, 0.05),
                ncol = 1, rel_widths = c(1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_3', ".tiff"),
       width = 6.5, height = 6, dpi = 350, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_3_sm', ".tiff"),
       width = 6.5, height = 6, dpi = 90, bg='white')

# . 5.6 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])
# Part 6 - Figure 4 ############################################################
# . 6.1 Opening Files ----------------------------------------------------------
datSim14 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[1],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[2],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[3],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSim13 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[4],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[8],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))

# . 6.2 Limits -----------------------------------------------------------------
minLimt <- rbind(
  datSim14$SSP585_7000_delta, datSeq14$SSP585_7000_delta, 
  datSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datSeq43$SSP585_7000_delta,
  datSim14$SSP126_7000_delta, datSeq14$SSP126_7000_delta, 
  datSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datSeq43$SSP126_7000_delta) %>%
  min()
maxLimt <- rbind(
  datSim14$SSP585_7000_delta, datSeq14$SSP585_7000_delta, 
  datSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datSeq43$SSP585_7000_delta,
  datSim14$SSP126_7000_delta, datSeq14$SSP126_7000_delta, 
  datSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datSeq43$SSP126_7000_delta) %>%
  max()
# . 6.3 Formatting -------------------------------------------------------------
datC1 <- tibble(lat = datSim14$lat, lon = datSim14$lon, 
                SSP126_7000_Delta = datSim14$SSP126_7000_delta,
                SSP126_7000_Sig = datSim14$SSP126_7000_Sig,
                SSP585_7000_Delta = datSim14$SSP585_7000_delta,
                SSP585_7000_Sig = datSim14$SSP585_7000_Sig)
datC1 <- datC1 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC1 <- subset(datC1, select=-c(Scenario2))
datC1$Scenario[datC1$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC1$Scenario[datC1$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC1$Delta <- as.integer(datC1$Delta)
datC1$Compound <- compT[1]

datC2 <- tibble(lat = datSim14$lat, lon = datSeq14$lon, 
                SSP126_7000_Delta = datSeq14$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq14$SSP126_7000_Sig,
                SSP585_7000_Delta = datSeq14$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq14$SSP585_7000_Sig)
datC2 <- datC2 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC2 <- subset(datC2, select=-c(Scenario2))
datC2$Scenario[datC2$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC2$Scenario[datC2$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC2$Compound <- compT[2]

datC3 <- tibble(lat = datSeq41$lat, lon = datSeq41$lon, 
                SSP126_7000_Delta = datSeq41$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq41$SSP126_7000_Sig,
                SSP585_7000_Delta = datSeq41$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq41$SSP585_7000_Sig)
datC3 <- datC3 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC3 <- subset(datC3, select=-c(Scenario2))
datC3$Scenario[datC3$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC3$Scenario[datC3$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC3$Compound <- compT[3]

datC4 <- tibble(lat = datSeq43$lat, lon = datSeq43$lon, 
                SSP126_7000_Delta = datSeq43$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq43$SSP126_7000_Sig,
                SSP585_7000_Delta = datSeq43$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq43$SSP585_7000_Sig)
datC4 <- datC4 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC4 <- subset(datC4, select=-c(Scenario2))
datC4$Scenario[datC4$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC4$Scenario[datC4$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC4$Compound <- compT[8]

datC <- rbind(datC1, datC2, datC3, datC4)
datC$Compound <- factor(datC$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))

# . 6.4 Plotting ---------------------------------------------------------------
p1 <- ggplot(data = datC, aes(x=lon, y=lat, fill = Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = c(-5, 0, 5, 10, 15, 20, 25), 
                    colours =  c( "#faebdd", "#f69c73", "#e83f3f",
                                           '#a11a5b','#4c1dab','#440154'), 
                                           show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_point(alpha = 0.75, shape = 47,
             aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Compound Event Occurances', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound),
             vars(Scenario)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','FIG_4', ".tiff"),
       width = 12, height = 11, dpi = 350, bg='white')
ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','FIG_4_sm', ".tiff"),
       width = 12, height = 11, dpi = 90, bg='white')
# . 6.5 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])
# Part 7 - Figure 5 ############################################################
# . 7.1 Opening Files ----------------------------------------------------------
datPop <- read_csv(paste0(fileloc1,'Data/',loc1[5],'PopDynamics_hist_future',
                          '.csv'),
                   col_names = TRUE, cols(.default = col_double()))

datH <- read_csv(paste0(fileloc1, 'Data/', loc1[4], 
                        'LUH2_historical_regrid360x180_', 'mod_states','.csv'),
                 col_names = TRUE, cols(.default = col_double()))
dat126_7000 <- read_csv(paste0(fileloc1, 'Data/', loc1[4], 'LUH2_SSP126_2070-2100_',
                               'regrid360x180_', 'mod_states', '.csv'),
                        col_names = TRUE, cols(.default = col_double()))
dat585_7000 <- read_csv(paste0(fileloc1, 'Data/', loc1[4], 'LUH2_SSP585_2070-2100_',
                               'regrid360x180_', 'mod_states', '.csv'),
                        col_names = TRUE, cols(.default = col_double()))

# . 7.2 Pre-processing Land Use Land Change ------------------------------------
datH$Crop <- (datH$c3ann + datH$c3nfx + datH$c3per + datH$c4ann + 
                datH$c4per ) * datH$LandAreakm2
datH$Forest <- (datH$primf + datH$secdf) * datH$LandAreakm2

dat126_7000$Crop <- (dat126_7000$c3ann + dat126_7000$c3nfx + dat126_7000$c3per + 
                       dat126_7000$c4ann + dat126_7000$c4per) * dat126_7000$LandAreakm2
dat126_7000$Forest <- (dat126_7000$primf + dat126_7000$secdf) * dat126_7000$LandAreakm2 

dat585_7000$Crop <- (dat585_7000$c3ann + dat585_7000$c3nfx + dat585_7000$c3per + 
                       dat585_7000$c4ann + dat585_7000$c4per) * dat126_7000$LandAreakm2 
dat585_7000$Forest <- (dat585_7000$primf + dat585_7000$secdf) * dat126_7000$LandAreakm2 

datLand <- tibble('lon' = datH$lon)
datLand$lat <- datH$lat
datLand$Crop126_7000_delta <- dat126_7000$Crop - datH$Crop
datLand$Forest126_7000_delta <- dat126_7000$Forest - datH$Forest
datLand$Crop585_7000_delta <- dat585_7000$Crop - datH$Crop
datLand$Forest585_7000_delta <- dat585_7000$Forest - datH$Forest

# . 7.3 Plotting Limits --------------------------------------------------------
maxPop <- rbind(datPop$SSP126_7000_delta, datPop$SSP585_7000_delta) %>%
  max() %>% 
  signif(digits = 3)
minPop <- rbind(datPop$SSP126_7000_delta, datPop$SSP585_7000_delta) %>%
  min() 
minPop <- floor(minPop / 10000) * 10000
hist(rbind(datPop$SSP585_7000_delta, datPop$SSP126_7000_delta))
breaksPop <- c(-Inf, -5000000, 0, 5000000, 10000000, 15000000, Inf)
colorPop <- c("#e9a3c9", "#fde0ef", "#e6f5d0", "#b8e186", '#7fbc41', '#4d9221')

maxLand <- rbind(datLand$Crop126_7000_delta, datLand$Forest126_7000_delta,
                 datLand$Crop585_7000_delta, datLand$Forest585_7000_delta) %>%
  max() %>% 
  signif(digits = 3)
minLand <- rbind(datLand$Crop126_7000_delta, datLand$Forest126_7000_delta,
                 datLand$Crop585_7000_delta, datLand$Forest585_7000_delta) %>%
  min() 
minLand <- floor(minLand / 100) * 100
hist(rbind(datLand$Crop126_7000_delta, datLand$Forest126_7000_delta,
           datLand$Crop585_7000_delta, datLand$Forest585_7000_delta))
breaksLand <- c(-Inf, -10000, -5000, 0, 5000, 10000, Inf)
colorLand <- c('#762a83',"#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", '#1b7837')

# Formatting
datPop <- datPop %>% 
  pivot_longer(cols = c(SSP126_7000_delta, SSP585_7000_delta),
               names_to = 'Scenario', values_to = 'Delta')
datPop$Scenario[datPop$Scenario == 'SSP126_7000_delta'] <- 'SSP1-2.6'
datPop$Scenario[datPop$Scenario == 'SSP585_7000_delta'] <- 'SSP5-8.5'

datLand <- datLand %>% 
  pivot_longer(cols = c(Crop126_7000_delta, Crop585_7000_delta,
                        Forest126_7000_delta, Forest585_7000_delta),
               names_to = 'Scenario', values_to = 'Delta')
datLand <- datLand %>%
  add_column(Type = 'type')
datLand$Type[datLand$Scenario == 'Crop126_7000_delta'] <- 'Agriculture Change (km2)'
datLand$Type[datLand$Scenario == 'Crop585_7000_delta'] <- 'Agriculture Change (km2)'
datLand$Type[datLand$Scenario == 'Forest126_7000_delta'] <- 'Forest Change (km2)'
datLand$Type[datLand$Scenario == 'Forest585_7000_delta'] <- 'Forest Change (km2)'
datLand$Scenario[datLand$Scenario == 'Crop126_7000_delta'] <- 'SSP1-2.6'
datLand$Scenario[datLand$Scenario == 'Crop585_7000_delta'] <- 'SSP5-8.5'
datLand$Scenario[datLand$Scenario == 'Forest126_7000_delta'] <- 'SSP1-2.6'
datLand$Scenario[datLand$Scenario == 'Forest585_7000_delta'] <- 'SSP5-8.5'

# . 7.3 Plotting Change 7000 ---------------------------------------------------
# . . 7.3.1 Population ----
a <- "SSP126 Late-Century - Historical"
p1 <- ggplot(data = datPop, aes(x=lon, y=lat, fill=Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = breaksPop, colours = colorPop, labels = comma) +
  geom_polygon(data=baseData, aes(x = long, y = lat, group = group),
               colour = "black", fill = "NA", linewidth = 0.1) +
  geom_rect(aes(xmin = lon1[1], xmax = lon2[1], ymin = lat1[1], ymax = lat2[1]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[2], xmax = lon2[2], ymin = lat1[2], ymax = lat2[2]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[3], xmax = lon2[3], ymin = lat1[3], ymax = lat2[3]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[4], xmax = lon2[4], ymin = lat1[4], ymax = lat2[4]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[5], xmax = lon2[5], ymin = lat1[5], ymax = lat2[5]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[6], xmax = lon2[6], ymin = lat1[6], ymax = lat2[6]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[7], xmax = lon2[7], ymin = lat1[7], ymax = lat2[7]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  coord_fixed(ratio = 1, xlim = c(-180,180), ylim = c(-60,90), expand = FALSE) +
  labs(title = 'Population Change (persons)', 
       x = 'Longitude', y = 'Latitude', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.75, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.text=element_text(size = 7)) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  facet_grid(vars(Scenario)) +
  theme(strip.text = element_blank(), 
        strip.background = element_blank())

# . . 7.3.2 Land-Use Land-Change ----
a <- "SSP126 Late-Century - Historical"
p2 <- ggplot(data = datLand, aes(x=lon, y=lat, fill = Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = breaksLand, colours = colorLand, 
                    show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_polygon(data=baseData, aes(x = long, y = lat, group = group),
               colour = "black", fill = "NA", linewidth = 0.1) +
  geom_rect(aes(xmin = lon1[1], xmax = lon2[1], ymin = lat1[1], ymax = lat2[1]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[2], xmax = lon2[2], ymin = lat1[2], ymax = lat2[2]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[3], xmax = lon2[3], ymin = lat1[3], ymax = lat2[3]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[4], xmax = lon2[4], ymin = lat1[4], ymax = lat2[4]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[5], xmax = lon2[5], ymin = lat1[5], ymax = lat2[5]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[6], xmax = lon2[6], ymin = lat1[6], ymax = lat2[6]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  geom_rect(aes(xmin = lon1[7], xmax = lon2[7], ymin = lat1[7], ymax = lat2[7]), 
            color = "maroon",fill = NA, alpha = 0.4) +
  coord_fixed(ratio = 1, xlim = c(-180,180), ylim = c(-60,90), expand = FALSE) +
  labs(
    # title = 'Land Use Change (Km2)', 
    x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(0.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  facet_grid(vars(Scenario),
             vars(Type)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

# . . 7.3.3 Combining ----
F1 <- plot_grid(p1, p2,
                nrow = 1, rel_heights = c(1),
                ncol = 2, rel_widths = c(.5,1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'FIG_5', ".tiff"),
       width = 10, height = 4, dpi = 375, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'FIG_5_sm', ".tiff"),
       width = 10, height = 4, dpi = 100, bg='white')
# . 7.4 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])
# Part 8 - Figure 6 - Figure 10 ################################################
# . 8.1 Opening Files ----------------------------------------------------------
datPopSim14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[1],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datPopSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[2],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datPopSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[3],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datPopSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[8],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))

datAgSim14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[1],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datAgSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[2],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datAgSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[3],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datAgSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[8],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))

datForSim14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[1],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datForSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[2],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datForSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[3],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datForSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[8],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))

datSim14 <- read_csv(paste0(fileloc1, 'Data/Results/','MU_CHANG_COMP_', comp[1], '.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq14 <- read_csv(paste0(fileloc1, 'Data/Results/','MU_CHANG_COMP_', comp[2], '.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq41 <- read_csv(paste0(fileloc1, 'Data/Results/','MU_CHANG_COMP_', comp[3], '.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq43 <- read_csv(paste0(fileloc1, 'Data/Results/','MU_CHANG_COMP_', comp[8], '.csv'),
                     col_names = TRUE, cols(.default = col_double()))
# . 8.2 Pre-processing Regions -------------------------------------------------
# . . 8.3.1 Pre-processing ---
datPopSim14 <- datPopSim14 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datPopSeq14 <- datPopSeq14 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datPopSeq41 <- datPopSeq41 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datPopSeq43 <- datPopSeq43 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)

datAgSim14 <- datAgSim14 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datAgSeq14 <- datAgSeq14 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datAgSeq41 <- datAgSeq41 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datAgSeq43 <- datAgSeq43 %>%
  select(lon, lat, Historic_mu, 
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)

datForSim14 <- datForSim14 %>%
  select(lon, lat, Historic_mu,
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datForSeq14 <- datForSeq14 %>%
  select(lon, lat, Historic_mu,
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datForSeq41 <- datForSeq41 %>%
  select(lon, lat, Historic_mu,
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)
datForSeq43 <- datForSeq43 %>%
  select(lon, lat, Historic_mu,
         SSP126_1040_delta, SSP126_4070_delta, SSP126_7000_delta,
         SSP585_1040_delta, SSP585_4070_delta, SSP585_7000_delta)

# . . 8.3.3 Regions ---
rm(datChang)
for (j in 1:4){
  if (j == 1){
    datPop <- datPopSim14
    datAg <- datAgSim14
    datFor <- datForSim14
    compound <- comp[1]
  } else if (j == 2){
    datPop <- datPopSeq14
    datAg <- datAgSeq14
    datFor <- datForSeq14
    compound <- comp[2]
  } else if (j == 3){
    datPop <- datPopSeq41
    datAg <- datAgSeq41
    datFor <- datForSeq41
    compound <- comp[3]
  } else {
    datPop <- datPopSeq43
    datAg <- datAgSeq43
    datFor <- datForSeq43
    compound <- comp[8]
  }
  datCh <- tibble(
    'Exposure' = numeric(length = 45),
    'Region' = numeric(length = 45),
    'Compound' = numeric(length = 45),
    'Historic_mu' = numeric(length = 45),
    'SSP126_1040' = numeric(length = 45),
    'SSP126_4070' = numeric(length = 45),
    'SSP126_7000' = numeric(length = 45),
    'SSP585_1040' = numeric(length = 45),
    'SSP585_4070' = numeric(length = 45),
    'SSP585_7000' = numeric(length = 45)
  )
  for (i in 1:length(locT)){
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
    
    datCh$Region[(3 * (i-1)) + 1] <- locT[i]
    datCh$Exposure[(3 * (i-1)) + 1] <- 'Population'
    datCh$Compound[(3 * (i-1)) + 1] <- compound
    datCh$Historic_mu[(3 * (i-1)) + 1] <- mean(datP$Historic_mu)
    datCh$SSP126_1040[(3 * (i-1)) + 1] <- mean(datP$SSP126_1040_delta)
    datCh$SSP126_4070[(3 * (i-1)) + 1] <- mean(datP$SSP126_4070_delta)
    datCh$SSP126_7000[(3 * (i-1)) + 1] <- mean(datP$SSP126_7000_delta)
    datCh$SSP585_1040[(3 * (i-1)) + 1] <- mean(datP$SSP585_1040_delta)
    datCh$SSP585_4070[(3 * (i-1)) + 1] <- mean(datP$SSP585_4070_delta)
    datCh$SSP585_7000[(3 * (i-1)) + 1] <- mean(datP$SSP585_7000_delta)
    
    datCh$Region[(3 * (i-1)) + 2] <- locT[i]
    datCh$Exposure[(3 * (i-1)) + 2] <- 'Agriculture'
    datCh$Compound[(3 * (i-1)) + 2] <- compound
    datCh$Historic_mu[(3 * (i-1)) + 2] <- mean(datA$Historic_mu)
    datCh$SSP126_1040[(3 * (i-1)) + 2] <- mean(datA$SSP126_1040_delta)
    datCh$SSP126_4070[(3 * (i-1)) + 2] <- mean(datA$SSP126_4070_delta)
    datCh$SSP126_7000[(3 * (i-1)) + 2] <- mean(datA$SSP126_7000_delta)
    datCh$SSP585_1040[(3 * (i-1)) + 2] <- mean(datA$SSP585_1040_delta)
    datCh$SSP585_4070[(3 * (i-1)) + 2] <- mean(datA$SSP585_4070_delta)
    datCh$SSP585_7000[(3 * (i-1)) + 2] <- mean(datA$SSP585_7000_delta)
    
    datCh$Region[(3 * (i-1)) + 3] <- locT[i]
    datCh$Exposure[(3 * (i-1)) + 3] <- 'Forestry'
    datCh$Compound[(3 * (i-1)) + 3] <- compound
    datCh$Historic_mu[(3 * (i-1)) + 3] <- mean(datF$Historic_mu)
    datCh$SSP126_1040[(3 * (i-1)) + 3] <- mean(datF$SSP126_1040_delta)
    datCh$SSP126_4070[(3 * (i-1)) + 3] <- mean(datF$SSP126_4070_delta)
    datCh$SSP126_7000[(3 * (i-1)) + 3] <- mean(datF$SSP126_7000_delta)
    datCh$SSP585_1040[(3 * (i-1)) + 3] <- mean(datF$SSP585_1040_delta)
    datCh$SSP585_4070[(3 * (i-1)) + 3] <- mean(datF$SSP585_4070_delta)
    datCh$SSP585_7000[(3 * (i-1)) + 3] <- mean(datF$SSP585_7000_delta)
  }
  if (j == 1){
    datChang <- datCh
  } else {
    datChang <- rbind(datChang, datCh)
  }
}
datChang$Compound <- factor(datChang$Compound, 
                            levels = c(comp[1], comp[2], comp[3], comp[8]), 
                            # labels = c(compT[1], compT[2], compT[3], compT[8])
                            labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                       'Seq. FD & HW', 'Seq. FD & EP'))
datChang$Region <- factor(datChang$Region, levels = rev(locTLev))

# . 8.3 Plotting Loli 126 ------------------------------------------------------
dat <- datChang[datChang$Exposure == 'Population',]
a <- 'Population'
p1 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_1040, yend=SSP126_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_4070, yend=SSP126_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP126_1040), color='gold', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_4070), color='darkorange1', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_7000), color='red3', size=4, alpha = 0.5 ) +
  coord_flip() +
  # scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "", y = "person-events", title = a) +
  facet_grid(vars(Compound)) +
  theme(axis.text.x = element_text(size = 7)) +
  theme(strip.text = element_blank(), 
        strip.background = element_blank())

dat <- datChang[datChang$Exposure == 'Agriculture',]
a <- 'Agriculture Land'
p2 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_1040, yend=SSP126_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_4070, yend=SSP126_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP126_1040), color='gold', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_4070), color='darkorange1', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_7000), color='red3', size=4, alpha = 0.5 ) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  # scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "", y = "km2-events", title = a) +
  facet_grid(vars(Compound)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 7)) +
  theme(strip.text = element_blank(), 
        strip.background = element_blank())

dat <- datChang[datChang$Exposure == 'Forestry',]
a <- 'Forestry Land'
p3 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_1040, yend=SSP126_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP126_4070, yend=SSP126_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP126_1040), color='gold', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_4070), color='darkorange1', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP126_7000), color='red3', size=4, alpha = 0.5 ) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  # scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "", y = "km2-events", title = a) +
  facet_grid(vars(Compound)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 7)) +
  theme(strip.text = element_text(size = 7), 
        strip.background = element_blank())

dat <- matrix(0, nrow = 6, ncol = 3) %>%
  tibble()
colnames(dat) <- c('Scenario','XX1','YY2')
dat[,2] <- c(2,3,4,5,3,2)
dat[,3] <- c(3,4,5,4,3,2)
dat$Scenario <- factor(colnames(datChang[3:8]))
colnames(dat) <- c('Scenario','XX1','YY2')
myLegend <- ggplot(dat, aes(x=XX1 ,y=YY2, color = Scenario)) +
  theme_bw() +
  geom_point(size=5, alpha = 0.5) + 
  scale_color_manual(values = c('SSP126_1040'="gold",
                                'SSP126_4070'="darkorange1",
                                'SSP126_7000'='red3'), 
                     labels = c('Early-Century', 'Mid-Century', 'Late-Century'),
                     name = "") 
myLegend <- get_legend(myLegend, position = 'bottom') %>% 
  as_ggplot()

F1A <- plot_grid(p1, p2, p3,
                 nrow = 1, rel_heights = c(1),
                 ncol = 3, rel_widths = c(1, .69, .74))
F1 <- plot_grid(F1A,
                myLegend,
                nrow = 2, rel_heights = c(1, 0.05),
                ncol = 1, rel_widths = c(1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_7', ".tiff"),
       width = 7, height = 5.2, dpi = 350, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_7_sm', ".tiff"),
       width = 7, height = 5.2, dpi = 100, bg='white')
# . 8.4 Plotting Loli 585 ------------------------------------------------------
dat <- datChang[datChang$Exposure == 'Population',]
a <- 'Population'
p1 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_1040, yend=SSP585_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_4070, yend=SSP585_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP585_1040), color='gold', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_4070), color='darkorange1', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_7000), color='red3', size=4, alpha = 0.5 ) +
  coord_flip() +
  # scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "", y = "person-events", title = a) +
  facet_grid(vars(Compound)) +
  theme(strip.text = element_blank(), 
        strip.background = element_blank())

dat <- datChang[datChang$Exposure == 'Agriculture',]
a <- 'Agriculture Land'
p2 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_1040, yend=SSP585_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_4070, yend=SSP585_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP585_1040), color='gold', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_4070), color='darkorange1', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_7000), color='red3', size=4, alpha = 0.5 ) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  # scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "", y = "km2-events", title = a) +
  facet_grid(vars(Compound)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  theme(strip.text = element_blank(), 
        strip.background = element_blank())

dat <- datChang[datChang$Exposure == 'Forestry',]
a <- 'Forestry Land'
p3 <- ggplot(dat) +
  theme_bw() +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_1040, yend=SSP585_4070), 
               color="grey") +
  geom_segment(aes(x=Region, xend=Region, y=SSP585_4070, yend=SSP585_7000), 
               color="grey") +
  geom_point(aes(x=Region, y=SSP585_1040), color='gold', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_4070), color='darkorange1', size=4, alpha = 0.5 ) +
  geom_point(aes(x=Region, y=SSP585_7000), color='red3', size=4, alpha = 0.5 ) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  # scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "", y = "km2-events", title = a) +
  facet_grid(vars(Compound)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  theme(strip.text = element_text(size = 7), 
        strip.background = element_blank())

dat <- matrix(0, nrow = 6, ncol = 3) %>%
  tibble()
colnames(dat) <- c('Scenario','XX1','YY2')
dat[,2] <- c(2,3,4,5,3,2)
dat[,3] <- c(3,4,5,4,3,2)
dat$Scenario <- factor(colnames(datChang[3:8]))
colnames(dat) <- c('Scenario','XX1','YY2')
myLegend <- ggplot(dat, aes(x=XX1 ,y=YY2, color = Scenario)) +
  theme_bw() +
  geom_point(size=5, alpha = 0.5) + 
  scale_color_manual(values = c('SSP126_1040'="gold",
                                'SSP126_4070'="darkorange1",
                                'SSP126_7000'='red3'), 
                     labels = c('Early-Century', 'Mid-Century', 'Late-Century'),
                     name = "") 
myLegend <- get_legend(myLegend, position = 'bottom') %>% 
  as_ggplot()

F1A <- plot_grid(p1, p2, p3,
                 nrow = 1, rel_heights = c(1),
                 ncol = 3, rel_widths = c(1, .69, .74))
F1 <- plot_grid(F1A,
                myLegend,
                nrow = 2, rel_heights = c(1, 0.05),
                ncol = 1, rel_widths = c(1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_8', ".tiff"),
       width = 7, height = 5.2, dpi = 350, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/','FIG_8_sm', ".tiff"),
       width = 7, height = 5.2, dpi = 100, bg='white')

# . 8.5 Pre-processing Limits --------------------------------------------------
# . . 8.5.1 Population ----
minLimt <- rbind(
  datPopSim14$SSP585_7000_delta, datPopSeq14$SSP585_7000_delta, 
  datPopSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datPopSeq43$SSP585_7000_delta,
  datPopSim14$SSP126_7000_delta, datPopSeq14$SSP126_7000_delta, 
  datPopSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datPopSeq43$SSP126_7000_delta) %>%
  min()
maxLimt <- rbind(
  datPopSim14$SSP585_7000_delta, datPopSeq14$SSP585_7000_delta, 
  datPopSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datPopSeq43$SSP585_7000_delta,
  datPopSim14$SSP126_7000_delta, datPopSeq14$SSP126_7000_delta, 
  datPopSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datPopSeq43$SSP126_7000_delta) %>%
  max()

datC1 <- tibble(lat = datPopSim14$lat, lon = datPopSim14$lon, 
                SSP126_7000_Delta = datPopSim14$SSP126_7000_delta,
                SSP126_7000_Sig = datSim14$SSP126_7000_Sig,
                SSP585_7000_Delta = datPopSim14$SSP585_7000_delta,
                SSP585_7000_Sig = datSim14$SSP585_7000_Sig)
datC1 <- datC1 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC1 <- subset(datC1, select=-c(Scenario2))
datC1$Scenario[datC1$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC1$Scenario[datC1$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC1$Delta <- as.integer(datC1$Delta)
datC1$Compound <- compT[1]

datC2 <- tibble(lat = datPopSim14$lat, lon = datPopSeq14$lon, 
                SSP126_7000_Delta = datPopSeq14$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq14$SSP126_7000_Sig,
                SSP585_7000_Delta = datPopSeq14$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq14$SSP585_7000_Sig)
datC2 <- datC2 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC2 <- subset(datC2, select=-c(Scenario2))
datC2$Scenario[datC2$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC2$Scenario[datC2$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC2$Compound <- compT[2]

datC3 <- tibble(lat = datPopSeq41$lat, lon = datPopSeq41$lon, 
                SSP126_7000_Delta = datPopSeq41$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq41$SSP126_7000_Sig,
                SSP585_7000_Delta = datPopSeq41$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq41$SSP585_7000_Sig)
datC3 <- datC3 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC3 <- subset(datC3, select=-c(Scenario2))
datC3$Scenario[datC3$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC3$Scenario[datC3$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC3$Compound <- compT[3]

datC4 <- tibble(lat = datPopSeq43$lat, lon = datPopSeq43$lon, 
                SSP126_7000_Delta = datPopSeq43$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq43$SSP126_7000_Sig,
                SSP585_7000_Delta = datPopSeq43$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq43$SSP585_7000_Sig)
datC4 <- datC4 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC4 <- subset(datC4, select=-c(Scenario2))
datC4$Scenario[datC4$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC4$Scenario[datC4$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC4$Compound <- compT[8]

datP <- rbind(datC1, datC2, datC3, datC4)
datP$Compound <- factor(datP$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))
# . . 8.5.2 Agriculture ----
minLimt <- rbind(
  datAgSim14$SSP585_7000_delta, datAgSeq14 $SSP585_7000_delta, 
  datAgSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datAgSeq43$SSP585_7000_delta,
  datAgSim14$SSP126_7000_delta, datAgSeq14 $SSP126_7000_delta, 
  datAgSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datAgSeq43$SSP126_7000_delta) %>%
  min()
maxLimt <- rbind(
  datAgSim14$SSP585_7000_delta, datAgSeq14 $SSP585_7000_delta, 
  datAgSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datAgSeq43$SSP585_7000_delta,
  datAgSim14$SSP126_7000_delta, datAgSeq14 $SSP126_7000_delta, 
  datAgSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datAgSeq43$SSP126_7000_delta) %>%
  max()

datC1 <- tibble(lat = datAgSim14$lat, lon = datAgSim14$lon, 
                SSP126_7000_Delta = datAgSim14$SSP126_7000_delta,
                SSP126_7000_Sig = datSim14$SSP126_7000_Sig,
                SSP585_7000_Delta = datAgSim14$SSP585_7000_delta,
                SSP585_7000_Sig = datSim14$SSP585_7000_Sig)
datC1 <- datC1 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC1 <- subset(datC1, select=-c(Scenario2))
datC1$Scenario[datC1$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC1$Scenario[datC1$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC1$Delta <- as.integer(datC1$Delta)
datC1$Compound <- compT[1]

datC2 <- tibble(lat = datAgSim14$lat, lon = datAgSeq14$lon, 
                SSP126_7000_Delta = datAgSeq14$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq14$SSP126_7000_Sig,
                SSP585_7000_Delta = datAgSeq14$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq14$SSP585_7000_Sig)
datC2 <- datC2 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC2 <- subset(datC2, select=-c(Scenario2))
datC2$Scenario[datC2$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC2$Scenario[datC2$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC2$Compound <- compT[2]

datC3 <- tibble(lat = datAgSeq41$lat, lon = datAgSeq41$lon, 
                SSP126_7000_Delta = datAgSeq41$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq41$SSP126_7000_Sig,
                SSP585_7000_Delta = datAgSeq41$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq41$SSP585_7000_Sig)
datC3 <- datC3 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC3 <- subset(datC3, select=-c(Scenario2))
datC3$Scenario[datC3$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC3$Scenario[datC3$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC3$Compound <- compT[3]

datC4 <- tibble(lat = datAgSeq43$lat, lon = datAgSeq43$lon, 
                SSP126_7000_Delta = datAgSeq43$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq43$SSP126_7000_Sig,
                SSP585_7000_Delta = datAgSeq43$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq43$SSP585_7000_Sig)
datC4 <- datC4 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC4 <- subset(datC4, select=-c(Scenario2))
datC4$Scenario[datC4$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC4$Scenario[datC4$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC4$Compound <- compT[8]

datA <- rbind(datC1, datC2, datC3, datC4)
datA$Compound <- factor(datA$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))
# . . 8.5.3 Forestry ----
minLimt <- rbind(
  datForSim14$SSP585_7000_delta, datForSeq14$SSP585_7000_delta, 
  datForSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datForSeq43$SSP585_7000_delta,
  datForSim14$SSP126_7000_delta, datForSeq14$SSP126_7000_delta, 
  datForSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datForSeq43$SSP126_7000_delta) %>%
  min()
maxLimt <- rbind(
  datForSim14$SSP585_7000_delta, datForSeq14$SSP585_7000_delta, 
  datForSeq41$SSP585_7000_delta, # datSim13$SSP585_7000_delta,
  datForSeq43$SSP585_7000_delta,
  datForSim14$SSP126_7000_delta, datForSeq14$SSP126_7000_delta, 
  datForSeq41$SSP126_7000_delta, # datSim13$SSP126_7000_delta,
  datForSeq43$SSP126_7000_delta) %>%
  max()

datC1 <- tibble(lat = datForSim14$lat, lon = datForSim14$lon, 
                SSP126_7000_Delta = datForSim14$SSP126_7000_delta,
                SSP126_7000_Sig = datSim14$SSP126_7000_Sig,
                SSP585_7000_Delta = datForSim14$SSP585_7000_delta,
                SSP585_7000_Sig = datSim14$SSP585_7000_Sig)
datC1 <- datC1 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC1 <- subset(datC1, select=-c(Scenario2))
datC1$Scenario[datC1$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC1$Scenario[datC1$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC1$Delta <- as.integer(datC1$Delta)
datC1$Compound <- compT[1]

datC2 <- tibble(lat = datForSim14$lat, lon = datForSeq14$lon, 
                SSP126_7000_Delta = datForSeq14$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq14$SSP126_7000_Sig,
                SSP585_7000_Delta = datForSeq14$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq14$SSP585_7000_Sig)
datC2 <- datC2 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC2 <- subset(datC2, select=-c(Scenario2))
datC2$Scenario[datC2$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC2$Scenario[datC2$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC2$Compound <- compT[2]

datC3 <- tibble(lat = datForSeq41$lat, lon = datForSeq41$lon, 
                SSP126_7000_Delta = datForSeq41$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq41$SSP126_7000_Sig,
                SSP585_7000_Delta = datForSeq41$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq41$SSP585_7000_Sig)
datC3 <- datC3 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC3 <- subset(datC3, select=-c(Scenario2))
datC3$Scenario[datC3$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC3$Scenario[datC3$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC3$Compound <- compT[3]

datC4 <- tibble(lat = datForSeq43$lat, lon = datForSeq43$lon, 
                SSP126_7000_Delta = datForSeq43$SSP126_7000_delta,
                SSP126_7000_Sig = datSeq43$SSP126_7000_Sig,
                SSP585_7000_Delta = datForSeq43$SSP585_7000_delta,
                SSP585_7000_Sig = datSeq43$SSP585_7000_Sig)
datC4 <- datC4 %>% 
  pivot_longer(cols = c(SSP126_7000_Delta, SSP585_7000_Delta),
               names_to = 'Scenario', values_to = 'Delta' ) %>%
  pivot_longer(cols = c(SSP126_7000_Sig, SSP585_7000_Sig),
               names_to = 'Scenario2', values_to = 'Sig') %>%
  filter(substr(Scenario, 1, 6) == substr(Scenario2, 1, 6)) 
datC4 <- subset(datC4, select=-c(Scenario2))
datC4$Scenario[datC4$Scenario == 'SSP126_7000_Delta'] <- 'SSP1-2.6'
datC4$Scenario[datC4$Scenario == 'SSP585_7000_Delta'] <- 'SSP5-8.5'
datC4$Compound <- compT[8]

datF <- rbind(datC1, datC2, datC3, datC4)
datF$Compound <- factor(datF$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))
# . 8.6 Plotting Exposure ------------------------------------------------------
p1 <- ggplot(data = datP, aes(x=lon, y=lat, fill = Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = c(-50000000, 0, 50000000, 100000000, 150000000,
                               200000000, 250000000), 
                    colours =  c( "#fde0ef", "#e6f5d0", "#b8e186",
                                           "#7fbc41", '#4d9221', '#006837'), 
                                           show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_point(alpha = 0.75, shape = 47,
             aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Population Exposure (person-events)', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(2.5, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound),
             vars(Scenario)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','FIG_6', ".tiff"),
       width = 12, height = 11, dpi = 350, bg='white')
ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','FIG_6_sm', ".tiff"),
       width = 12, height = 11, dpi = 90, bg='white')


p2 <- ggplot(data = datA, aes(x=lon, y=lat, fill = Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = c(-Inf, -30000, 0, 30000, 60000, 90000, Inf), 
                    colours =  c( "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", '#1b7837', '#006837'), 
                    show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_point(alpha = 0.75, shape = 47,
             aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Agriculture Exposure (km2-events)', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound),
             vars(Scenario)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p2, filename = paste0(fileloc1,'Results/Paper/','FIG_9', ".tiff"),
       width = 12, height = 11, dpi = 350, bg='white')
ggsave(p2, filename = paste0(fileloc1,'Results/Paper/','FIG_9_sm', ".tiff"),
       width = 12, height = 11, dpi = 90, bg='white')

p3 <- ggplot(data = datF, aes(x=lon, y=lat, fill = Delta)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(breaks = c(-Inf, -60000, 0, 60000, 120000, 1800000, Inf), 
                    colours =  c( "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", '#1b7837', '#006837'), 
                    show.limits = F, oob = oob_keep,
                    labels = comma) +
  geom_point(alpha = 0.75, shape = 47,
             aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Forestry Exposure (km2-events)', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound),
             vars(Scenario)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p3, filename = paste0(fileloc1,'Results/Paper/','FIG_10', ".tiff"),
       width = 12, height = 11, dpi = 350, bg='white')
ggsave(p3, filename = paste0(fileloc1,'Results/Paper/','FIG_10_sm', ".tiff"),
       width = 12, height = 11, dpi = 90, bg='white')


# . 8.7 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])
# Part 9 - Supplemental Figure 1 ###############################################
# . 9.1 Opening Files ----------------------------------------------------------
relV1 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[1],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV3 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[3],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
relV4 <- read_csv(paste0(fileloc1,'Data/Results/','OCC_CHANG_',var[4],'.csv'), 
                  col_names = TRUE, cols(.default = col_double()))

obsV1 <- read_csv(paste0(fileloc1,'Data/',loc1[6],loc2[9],'OCC_DAY_',var[1],
                         '_Hist_ERA5_8010.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
obsV3 <- read_csv(paste0(fileloc1,'Data/',loc1[6],loc2[9],'OCC_DAY_',var[3],
                         '_Hist_ERA5_8010.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
obsV4 <- read_csv(paste0(fileloc1,'Data/',loc1[6],loc2[9],'OCC_DAY_',var[4],
                         '_Hist_ERA5_8010.csv'), 
                  col_names = TRUE, cols(.default = col_double()))
# . 9.2 Limits -----------------------------------------------------------------
maxLimitV1 <- max(relV1$Historical_Mu)
minLimitV1 <- min(relV1$Historical_Mu)
maxLimitV3 <- max(relV3$Historical_Mu)
minLimitV3 <- min(relV3$Historical_Mu)
maxLimitV4 <- max(relV4$Historical_Mu)
minLimitV4 <- min(relV4$Historical_Mu)

maxLimitV1a <- max(obsV1$ERA5)
minLimitV1a <- min(obsV1$ERA5)
maxLimitV3a <- max(obsV3$ERA5)
minLimitV3a <- min(obsV3$ERA5)
maxLimitV4a <- max(obsV4$ERA5)
minLimitV4a <- min(obsV4$ERA5)

obsV1$Dif <- obsV1$ERA5 - relV1$Historical_Mu
obsV3$Dif <- obsV3$ERA5 - relV3$Historical_Mu
obsV4$Dif <- obsV4$ERA5 - relV4$Historical_Mu

show_col(viridis_pal(alpha=1, option='F')(6))
show_col(viridis_pal(alpha=1, option='D')(6))
show_col(viridis_pal(alpha=1, option='G')(6))
# . 9.3 Plotting----------------------------------------------------------------
a <- paste0('Heatwave Occurances')
p1 <- ggplot(data = relV1, aes(x= lon, y= lat, fill= Historical_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(0,120,240,360,480,600,720),
    colours =  c( "#faebdd", "#f69c73", "#e83f3f",
                           '#a11a5b','#4c1dab','#440154'), 
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = 'Latitude', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0('Extreme Precip Occurances')
p2 <- ggplot(data = relV3, aes(x=lon, y=lat, fill=Historical_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-100, 0, 100,200, 300, 400, 500),
    colours =  c( "#fde725", "#7ad151", "#22A884",
                           '#2A788E', '#414487', '#440154'),
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0(varT[4],' Occurances')
p3 <- ggplot(data = relV4, aes(x = lon, y = lat, fill = Historical_Mu)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-10, -5, 0, 5, 10, 16),
    colours =  c( "#DEF5E5", "#60CEAC", "#3497A9",
                           '#395D9C', '#382A54'),
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(vars(Scenario)) +
  theme(strip.text = element_text(size = 7), 
        strip.background = element_blank())

F1 <- plot_grid(p1, p2, p3,
                nrow = 1, rel_heights = c(1),
                ncol = 3, rel_widths = c(.93,.93,1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'SUPFIG_1', ".tiff"),
       width = 9, height = 4, dpi = 375, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'SUPFIG_1_sm', ".tiff"),
       width = 9, height = 4, dpi = 90, bg='white')
# . 9.4 Plotting ERA5 ----------------------------------------------------------
a <- paste0('Heatwave Occurances')
p1 <- ggplot(data = obsV1, aes(x= lon, y= lat, fill= ERA5)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(0,120,240,360,480,600,720),
    colours =  c( "#faebdd", "#f69c73", "#e83f3f",
                           '#a11a5b','#4c1dab','#440154'), 
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = 'Latitude', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0('Extreme Precip Occurances')
p2 <- ggplot(data = obsV3, aes(x=lon, y=lat, fill=ERA5)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-100, 0, 100,200, 300, 400, 500),
    colours =  c( "#fde725", "#7ad151", "#22A884",
                           '#2A788E', '#414487', '#440154'),
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0(varT[4],' Occurances')
p3 <- ggplot(data = obsV4, aes(x = lon, y = lat, fill = ERA5)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-10, -5, 0, 5, 10, 16),
    colours =  c( "#DEF5E5", "#60CEAC", "#3497A9",
                           '#395D9C', '#382A54'),
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(vars(Scenario)) +
  theme(strip.text = element_text(size = 7), 
        strip.background = element_blank())

F1 <- plot_grid(p1, p2, p3,
                nrow = 1, rel_heights = c(1),
                ncol = 3, rel_widths = c(.93,.93,1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'SUPFIG_1a', ".tiff"),
       width = 9, height = 4, dpi = 375, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'SUPFIG_1a_sm', ".tiff"),
       width = 9, height = 4, dpi = 90, bg='white')
# . 9.5 Plotting Difference ----------------------------------------------------
a <- paste0('Heatwave Occurances')
p1 <- ggplot(data = obsV1, aes(x= lon, y= lat, fill= Dif)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(0,120,240,360,480,600,720),
    colours =  c( "#faebdd", "#f69c73", "#e83f3f",
                           '#a11a5b','#4c1dab','#440154'), 
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = 'Latitude', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0('Extreme Precip Occurances')
p2 <- ggplot(data = obsV3, aes(x=lon, y=lat, fill=Dif)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-100, 0, 100,200, 300, 400, 500),
    colours =  c( "#fde725", "#7ad151", "#22A884",
                           '#2A788E', '#414487', '#440154'),
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(Scenario ~ .) +
  theme(strip.text = element_blank(),
        # strip.text = element_text(size = 7), 
        strip.background = element_blank())

a <- paste0(varT[4],' Occurances')
p3 <- ggplot(data = obsV4, aes(x = lon, y = lat, fill = Dif)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-10, -5, 0, 5, 10, 16),
    colours =  c( "#DEF5E5", "#60CEAC", "#3497A9",
                           '#395D9C', '#382A54'),
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.25, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-60,90), expand = FALSE) +
  labs(title = a, x = 'Longitude', y = '', fill = '') +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit="cm")) +
  # facet_grid(vars(Scenario)) +
  theme(strip.text = element_text(size = 7), 
        strip.background = element_blank())

F1 <- plot_grid(p1, p2, p3,
                nrow = 1, rel_heights = c(1),
                ncol = 3, rel_widths = c(.93,.93,1))
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'SUPFIG_1b', ".tiff"),
       width = 9, height = 4, dpi = 375, bg='white')
ggsave(F1, filename = paste0(fileloc1,'Results/Paper/', 'SUPFIG_1b_sm', ".tiff"),
       width = 9, height = 4, dpi = 90, bg='white')
# . 9.4 Remove -----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])
# Part 10 - Supplemental Figure 2 ##############################################
# . 10.1 Opening Files ---------------------------------------------------------
datSim14 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[1],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[2],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[3],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))
datSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','MU_CHANG_COMP_',comp[8],'.csv'),
                     col_names = TRUE, cols(.default = col_double()))

# . 10.2 Limits ----------------------------------------------------------------
minLimt <- rbind(
  datSim14$Historical_mu, datSeq14$Historical_mu, 
  datSeq41$Historical_mu, # datSim13$Historical_mu,
  datSeq43$Historical_mu) %>%
  min()
maxLimt <- rbind(
  datSim14$Historical_mu, datSeq14$Historical_mu, 
  datSeq41$Historical_mu, # datSim13$Historical_mu,
  datSeq43$Historical_mu) %>%
  max()

# . 10.3 Formatting ------------------------------------------------------------
datC1 <- tibble(lat = datSim14$lat, lon = datSim14$lon, 
                Historical = datSim14$Historical_mu,
                Compound = compT[1])
datC1$Historical <- as.integer(datC1$Historical)

datC2 <- tibble(lat = datSeq14$lat, lon = datSeq14$lon, 
                Historical = datSeq14$Historical_mu,
                Compound = compT[2])
datC2$Historical <- as.integer(datC2$Historical)

datC3 <- tibble(lat = datSeq41$lat, lon = datSeq41$lon, 
                Historical = datSeq41$Historical_mu,
                Compound = compT[3])
datC3$Historical <- as.integer(datC3$Historical)

datC4 <- tibble(lat = datSeq43$lat, lon = datSeq43$lon, 
                Historical = datSeq43$Historical_mu,
                Compound = compT[8])
datC4$Historical <- as.integer(datC4$Historical)

datC <- rbind(datC1, datC2, datC3, datC4)
datC$Compound <- factor(datC$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))
# . 10.4 Plotting --------------------------------------------------------------
p1 <- ggplot(data = datC, aes(x=lon, y=lat, fill = Historical)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-5, 0, 5, 10, 15, 20, 25), 
    colours =  c( "#faebdd", "#f69c73", "#e83f3f",
                           '#a11a5b','#4c1dab','#440154'), 
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.75, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Compound Event Occurances', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_2', ".tiff"),
       width = 7.5, height = 11, dpi = 350, bg='white')
ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_2_sm', ".tiff"),
       width = 7.5, height = 11, dpi = 90, bg='white')
# . 10.5 Remove ----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])





# Part 11 - Supplemental Figure 3 - 4 ##########################################
# . 11.1 Opening Files --------------------------------------------------------
datPopSim14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[1],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datPopSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[2],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datPopSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[3],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datPopSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_POP_',comp[8],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))

datAgSim14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[1],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datAgSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[2],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datAgSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[3],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))
datAgSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_AG_',comp[8],'.csv'),
                       col_names = TRUE, cols(.default = col_double()))

datForSim14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[1],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datForSeq14 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[2],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datForSeq41 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[3],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
datForSeq43 <- read_csv(paste0(fileloc1,'Data/Results/','EXPOSURE_FOR_',comp[8],'.csv'),
                        col_names = TRUE, cols(.default = col_double()))
# . 11.2 Pre-processing Limits -------------------------------------------------
# . . 11.2.1 Population ----
minLimt <- rbind(
  datPopSim14$Historic_mu , datPopSeq14$Historic_mu, 
  datPopSeq41$Historic_mu, 
  datPopSeq43$Historic_mu) %>%
  min()
maxLimt <- rbind(
  datPopSim14$Historic_mu, datPopSeq14$Historic_mu, 
  datPopSeq41$Historic_mu, # datSim13$Historic_mu,
  datPopSeq43$Historic_mu) %>%
  max()

datC1 <- tibble(lat = datPopSim14$lat, lon = datPopSim14$lon, 
                Historical = datPopSim14$Historic_mu,
                Compound = compT[1])
datC1$Delta <- as.integer(datC1$Historical)

datC2 <- tibble(lat = datPopSeq14$lat, lon = datPopSeq14$lon, 
                Historical = datPopSeq14$Historic_mu,
                Compound = compT[2])
datC2$Delta <- as.integer(datC2$Historical)

datC3 <- tibble(lat = datPopSeq41$lat, lon = datPopSeq41$lon, 
                Historical = datPopSeq41$Historic_mu,
                Compound = compT[3])
datC3$Delta <- as.integer(datC3$Historical)

datC4 <- tibble(lat = datPopSeq43$lat, lon = datPopSeq43$lon, 
                Historical = datPopSeq43$Historic_mu,
                Compound = compT[8])
datC4$Delta <- as.integer(datC4$Historical)

datP <- rbind(datC1, datC2, datC3, datC4)
datP$Compound <- factor(datP$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))
# . . 11.2.2 Agriculture ----
minLimt <- rbind(
  datAgSim14$Historic_mu , datAgSeq14$Historic_mu, 
  datAgSeq41$Historic_mu, 
  datAgSeq43$Historic_mu) %>%
  min()
maxLimt <- rbind(
  datAgSim14$Historic_mu, datAgSeq14$Historic_mu, 
  datAgSeq41$Historic_mu, # datSim13$Historic_mu,
  datAgSeq43$Historic_mu) %>%
  max()

datC1 <- tibble(lat = datAgSim14$lat, lon = datAgSim14$lon, 
                Historical = datAgSim14$Historic_mu,
                Compound = compT[1])
datC1$Delta <- as.integer(datC1$Historical)

datC2 <- tibble(lat = datAgSeq14$lat, lon = datAgSeq14$lon, 
                Historical = datAgSeq14$Historic_mu,
                Compound = compT[2])
datC2$Delta <- as.integer(datC2$Historical)

datC3 <- tibble(lat = datAgSeq41$lat, lon = datAgSeq41$lon, 
                Historical = datAgSeq41$Historic_mu,
                Compound = compT[3])
datC3$Delta <- as.integer(datC3$Historical)

datC4 <- tibble(lat = datAgSeq43$lat, lon = datAgSeq43$lon, 
                Historical = datAgSeq43$Historic_mu,
                Compound = compT[8])
datC4$Delta <- as.integer(datC4$Historical)

datA <- rbind(datC1, datC2, datC3, datC4)
datA$Compound <- factor(datA$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))
# . . 11.2.3 Forestry ----
minLimt <- rbind(
  datForSim14$Historic_mu , datForSeq14$Historic_mu, 
  datForSeq41$Historic_mu, 
  datForSeq43$Historic_mu) %>%
  min()
maxLimt <- rbind(
  datForSim14$Historic_mu, datForSeq14$Historic_mu, 
  datForSeq41$Historic_mu, # datSim13$Historic_mu,
  datForSeq43$Historic_mu) %>%
  max()

datC1 <- tibble(lat = datForSim14$lat, lon = datForSim14$lon, 
                Historical = datForSim14$Historic_mu,
                Compound = compT[1])
datC1$Delta <- as.integer(datC1$Historical)

datC2 <- tibble(lat = datForSeq14$lat, lon = datForSeq14$lon, 
                Historical = datForSeq14$Historic_mu,
                Compound = compT[2])
datC2$Delta <- as.integer(datC2$Historical)

datC3 <- tibble(lat = datForSeq41$lat, lon = datForSeq41$lon, 
                Historical = datForSeq41$Historic_mu,
                Compound = compT[3])
datC3$Delta <- as.integer(datC3$Historical)

datC4 <- tibble(lat = datForSeq43$lat, lon = datForSeq43$lon, 
                Historical = datForSeq43$Historic_mu,
                Compound = compT[8])
datC4$Delta <- as.integer(datC4$Historical)
datF <- rbind(datC1, datC2, datC3, datC4)
datF$Compound <- factor(datF$Compound, 
                        levels = c(compT[1], compT[2], compT[3], compT[8]), 
                        labels = c('Sim. HW & FD', 'Seq. HW & FD', 
                                   'Seq. FD & HW', 'Seq. FD & EP'))
# . 11.6 Plotting Exposure ------------------------------------------------------
p1 <- ggplot(data = datP, aes(x=lon, y=lat, fill = Historical)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(#breaks = c(-50000000, 0, 50000000, 100000000, 150000000,
    #           200000000, 250000000), 
    colours =  c( "#fde0ef", "#e6f5d0", "#b8e186",
                           "#7fbc41", '#4d9221', '#006837'), 
                           show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.75, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Population Exposure (person-events)', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(2.5, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_3', ".tiff"),
       width = 7.5, height = 11, dpi = 350, bg='white')
ggsave(p1, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_3_sm', ".tiff"),
       width = 7.5, height = 11, dpi = 90, bg='white')


p2 <- ggplot(data = datA, aes(x=lon, y=lat, fill = Historical)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(# breaks = c(-Inf, -30000, 0, 30000, 60000, 90000, Inf), 
    colours =  c( "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", '#1b7837', '#006837'), 
    show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.75, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Agriculture Exposure (km2-events)', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p2, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_4', ".tiff"),
       width = 7.5, height = 11, dpi = 350, bg='white')
ggsave(p2, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_4_sm', ".tiff"),
       width = 7.5, height = 11, dpi = 90, bg='white')

p3 <- ggplot(data = datF, aes(x=lon, y=lat, fill = Historical)) +
  theme_bw() +
  geom_tile() +
  scale_fill_stepsn(#breaks = c(-Inf, -60000, 0, 60000, 120000, 1800000, Inf), 
    colours =  c( "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", '#1b7837', '#006837'), 
    show.limits = F, oob = oob_keep,
    labels = comma) +
  # geom_point(alpha = 0.75, shape = 47,
  #            aes(size=ifelse(Sig == 1,'dot', 'no_dot'))) +
  # scale_size_manual(values=c(dot=0.5, no_dot=NA), guide="none") +
  geom_polygon(data=baseData, aes(x=long, y=lat, group=group),
               colour="black", fill="NA", linewidth=0.5) +
  coord_fixed(ratio=1, xlim=c(-180,180), ylim=c(-65,90), expand = FALSE) +
  labs(title = 'Forestry Exposure (km2-events)', fill = '',
       x = 'Longitude', y = 'Latitude') +
  theme(legend.position="bottom",
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(.5, 'cm')) +
  theme(plot.margin=margin(t=0.5, r=0.5, unit="cm"))  +
  facet_grid(vars(Compound)) +
  theme(strip.text = element_text(size = 12), 
        strip.background = element_blank())

ggsave(p3, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_5', ".tiff"),
       width = 7.5, height = 11, dpi = 350, bg='white')
ggsave(p3, filename = paste0(fileloc1,'Results/Paper/','SUPFIG_5_sm', ".tiff"),
       width = 7.5, height = 11, dpi = 90, bg='white')


# . 11.7 Remove ----------------------------------------------------------------
rm(list=ls()[! ls() %in% c('fileloc1', 'loc1', 'loc2', 'yr', 
                           'var', 'varT', 'comp', 'compT', 
                           'locT','locTLev', 'lon1', 'lon2', 'lat1', 'lat2', 
                           'baseData', 
                           'get_legend','as_ggplot','mean_cl_quantile')])

# END ##########################################################################