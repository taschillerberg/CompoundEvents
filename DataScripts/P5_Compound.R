# P6_Compound_Day.R
# About: This program will open the exceedance file for the selected 
#       variable and calculate a 'wave' of when the exceedance occurs.
#       Most recent run of day historical (B) took 30 hr, 13GB; SSP126 (C & D)
#       took 32h, 13GB; SSP585(E & F) took h, 14GB 
#       Most recent run of week historical (B) took 5h, 3GB; SSP126 (C & D)
#       took 6h, 3GB; SSP585(E & F) took 7h, 3GB
#       Most recent run of month historical (B) took 2h, 1GB; SSP126 (C & D)
#       took 2h, 1GB; SSP585(E & F) took 2h, 1GB
#
# Inputs: EXCEED & WAVES
# Outputs: COMPOUND
#
# T. A. Schillerberg
#               Oct. 2022
#      Updated: Jun. 2023

# Mac

# Office Computer
# setwd("C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Code2")
# fileloc1 <- 'C:/Users/tas0053/OneDrive - Auburn University/Research/FEMAResearch/Data/'

# HPC
fileloc1 <- '~/CompoundEvents/Data/'

options(show.error.locations = TRUE)
# Libraries ###############################################################
library(tidyverse)
library(CoinCalc)

# Part I Variables To Change ##############################################
mNum <- as.numeric('model_num') # Bash script
# wavesTime <- c('WAVES_DAY','WAVES_WEEK','WAVES_MONTH')[as.numeric('waves_time')]
# mNum <- 1 # Select a model (1-4)
wavesTime <- c('WAVES_DAY_','WAVES_WEEK_','WAVES_MONTH_')[1]
wavesTime2 <- c('WAVES_DAY_','WAVES_WEEK_FD_','WAVES_WEEK_D_',
               'WAVES_MONTH_FD_','WAVES_MONTH_D_')[1]
mFile <- c('_day_CMCC-ESM2_historical_r1i1p1f1_gn_',
           '_day_EC-Earth3_historical_r1i1p1f1_gr_',
           '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
           '_day_INM-CM4-8_historical_r1i1p1f1_gr1_',
           '_day_INM-CM5-0_historical_r1i1p1f1_gr1_',
           '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
           '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_',
           '_day_NorESM2-MM_historical_r1i1p1f1_gn_') [mNum]
# mFile <- c('_day_CMCC-ESM2_ssp126_r1i1p1f1_gn_',
#            '_day_EC-Earth3_ssp126_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp126_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_ssp126_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_ssp126_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp126_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp126_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_ssp126_r1i1p1f1_gn_')[mNum]
# mFile <- c('_day_CMCC-ESM2_ssp585_r1i1p1f1_gn_',
#            '_day_EC-Earth3_ssp585_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM4-8_ssp585_r1i1p1f1_gr1_',
#            '_day_INM-CM5-0_ssp585_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_',
#            '_day_NorESM2-MM_ssp585_r1i1p1f1_gn_')[mNum]
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')[1]
loc2 <- c('CMCC-ESM2/', 'EC-Earth3/',
          'GFDL-ESM4/', 'INM-CM4-8/',
          'INM-CM5-0/', 'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/', 'NorESM2-MM/') [mNum]
var <- c('tasmax', 'tasmin','pr','mrsos')
startyr <- 1980
endyr <- 2010

print(paste0('Model: ',loc2))
print(paste0('Temporal Resolution: ', wavesTime))
print('Rscript: P4_Compound.R')
print(paste0('Scenario: ', loc1, ' For the time period: ', startyr, '-', endyr))
# Functions ####################################################################
CC.eca.ts2 <- function (seriesA, seriesB, alpha = 0.05, delT = 0, sym = FALSE, 
                        tau = 0, sigtest = "poisson", reps = 1000) 
{
  if (length(table(seriesA)) > 2) {
    print("|-------------    ERROR #1    -------------|")
    print("|       Time seriesA is not binary         |")
    print("|      (or the number of events=0)!        |")
    print("| Use CC.binarize() to preprocess seriesA  |")
    print("|------------------------------------------|")
    return()
  }
  if (length(table(seriesB)) > 2) {
    print("|-------------    ERROR #1    -------------|")
    print("|       Time seriesB is not binary         |")
    print("|      (or the number of events=0)!        |")
    print("| Use CC.binarize() to preprocess seriesA  |")
    print("|------------------------------------------|")
    return()
  }
  if (tau < 0 || delT < 0) {
    print("|-------------    ERROR #2    -------------|")
    print("|    The offset (tau) or delta T (delT)    |")
    print("|              is negative.                |")
    print("|------------------------------------------|")
    return()
  }
  if (length(seriesA) != length(seriesB)) {
    print("|-------------    ERROR #4    -------------|")
    print("|    lengths of series A and B differ      |")
    print("|------------------------------------------|")
    return()
  }
  if (!is.vector(seriesA) && !is.matrix(seriesA)) {
    print("|-------------    ERROR #5    -------------|")
    print("| series A is neither vector nor matrix    |")
    print("|------------------------------------------|")
    return()
  }
  if (!is.vector(seriesB) && !is.matrix(seriesB)) {
    print("|-------------    ERROR #5    -------------|")
    print("| series B is neither vector nor matrix    |")
    print("|------------------------------------------|")
    return()
  }
  if (any(is.na(seriesA)) && sigtest == "surrogate") {
    print("|-------------    ERROR #6    -------------|")
    print("|          seriesA contains NAs.           |")
    print("| surrogate test not allowed, use poisson  |")
    print("|------------------------------------------|")
    return()
  }
  if (any(is.na(seriesB)) && sigtest == "surrogate") {
    print("|-------------    ERROR #6    -------------|")
    print("|          seriesB contains NAs.           |")
    print("| surrogate test not allowed, use poisson  |")
    print("|------------------------------------------|")
    return()
  }
  if (any(is.na(seriesB)) && sigtest == "shuffle") {
    print("|-------------    ERROR #6    -------------|")
    print("|    seriesB or seriesA contains NAs.      |")
    print("| shuffle test not allowed, use poisson    |")
    print("|------------------------------------------|")
    return()
  }
  if (sum(is.na(seriesA)) == length(seriesA) || sum(is.na(seriesB)) == 
      length(seriesB)) {
    CA_out = list(NA, NA, NA, NA, NA, NA)
    names(CA_out) = c("NH precursor", "NH trigger", "p-value precursor", 
                      "p-value trigger", "precursor coincidence rate", 
                      "trigger coincidence rate")
    return(CA_out)
  }
  seriesA[is.na(seriesB)] = NA
  seriesB[is.na(seriesA)] = NA
  bindata = matrix(NA, 2, length(seriesA))
  bindata[1, ] = seriesA
  bindata[2, ] = seriesB
  rownames(bindata) = c("seriesA", "seriesB")
  Tlen = length(bindata[1, !is.na(bindata[1, ])])
  steplen = length(seriesA)
  N_A = as.numeric(Tlen - table(bindata[1, ] == 1)[1])
  N_B = as.numeric(Tlen - table(bindata[2, ] == 1)[1])
  K_prec = 0
  for (step in 1:steplen) {
    if (is.na(bindata[1, step])) {
      next
    }
    if (bindata[1, step] == 1) {
      start = step - tau - (delT)
      if (sym == TRUE) {
        end = step - tau + (delT)
      }
      if (sym == FALSE) {
        end = step - tau
      }
      if (start < 1 & end >= 1) {
        start = 1
      }
      if (start < 1 & end < 1) {
        next
      }
      if (start > steplen) {
        next
      }
      if (end > steplen) {
        end = steplen
      }
      if (is.element(1, bindata[2, start:end])) {
        K_prec = K_prec + 1
      }
    }
  }
  CRprec = K_prec/N_A
  K_trigg = 0
  for (step in 1:steplen) {
    if (is.na(bindata[2, step])) {
      next
    }
    if (bindata[2, step] == 1) {
      if (sym == TRUE) {
        start = step + tau - (delT)
      }
      if (sym == FALSE) {
        start = step + tau
      }
      end = step + tau + (delT)
      if (start < 1 & end >= 1) {
        start = 1
      }
      if (start < 1 & end < 1) {
        next
      }
      if (start > steplen) {
        next
      }
      if (end > steplen) {
        end = steplen
      }
      if (is.element(1, bindata[1, start:end])) {
        K_trigg = K_trigg + 1
      }
    }
  }
  CRtrigg = K_trigg/N_B
  if (sigtest == "poisson") {
    if (sym == FALSE) {
      Pprec = 0
      for (Ktmp in K_prec:N_A) {
        Ptmp = choose(N_A, Ktmp) * ((1 - ((1 - ((delT + 
                                                   1)/(Tlen - tau)))^N_B))^Ktmp) * (((1 - ((delT + 
                                                                                              1)/(Tlen - tau)))^N_B)^(N_A - Ktmp))
        Ptmp <- ifelse(is.na(Ptmp),0,Ptmp) #TAS11/30/22
        Pprec = Pprec + Ptmp
      }
      Ptrigg = 0
      for (Ktmp in K_trigg:N_B) {
        Ptmp = choose(N_B, Ktmp) * ((1 - ((1 - ((delT + 
                                                   1)/(Tlen - tau)))^N_A))^Ktmp) * (((1 - ((delT + 
                                                                                              1)/(Tlen - tau)))^N_A)^(N_B - Ktmp))
        Ptmp <- ifelse(is.na(Ptmp),0,Ptmp) #TAS11/30/22
        Ptrigg = Ptrigg + Ptmp
      }
    }
    if (sym == TRUE) {
      Pprec = 0
      delTsym = delT + delT + 1
      for (Ktmp in K_prec:N_A) {
        Ptmp = choose(N_A, Ktmp) * ((1 - ((1 - (delTsym/(Tlen)))^N_B))^Ktmp) * 
          (((1 - (delTsym/(Tlen)))^N_B)^(N_A - Ktmp))
        Pprec = Pprec + Ptmp
      }
      Ptrigg = 0
      for (Ktmp in K_trigg:N_B) {
        Ptmp = choose(N_B, Ktmp) * ((1 - ((1 - (delTsym/(Tlen)))^N_A))^Ktmp) * 
          (((1 - (delTsym/(Tlen)))^N_A)^(N_B - Ktmp))
        Ptrigg = Ptrigg + Ptmp
      }
    }
  }
  if (sigtest == "wt.surrogate") {
    if (delT == 0) {
      delT = 1
    }
    seriesA = CC.ts2es(seriesA)
    seriesB = CC.ts2es(seriesB)
    span = (max(seriesA$span[1], seriesB$span[1]):min(seriesA$span[2], 
                                                      seriesB$span[2]))
    Tlen = length(span)
    seriesA_sort = sort(seriesA$es)
    wtA = rep(0, N_A)
    wtA[1] = seriesA_sort[1] - seriesA$span[1]
    for (i in 2:N_A) {
      wtA[i] = seriesA_sort[i] - seriesA_sort[i - 1]
    }
    seriesB_sort = sort(seriesB$es)
    wtB = rep(0, N_B)
    wtB[1] = seriesB_sort[1] - seriesB$span[1]
    for (i in 2:N_B) {
      wtB[i] = seriesB_sort[i] - seriesB_sort[i - 1]
    }
    surdist = matrix(NA, 2, reps)
    for (surno in 1:reps) {
      surA = sample(wtA, size = 1)
      for (i in 2:length(seriesA$es)) {
        tmp = surA[i - 1] + sample(wtA, size = 1)
        if (tmp > span[Tlen]) {
          break
        }
        else {
          surA[i] = tmp
        }
      }
      surB = sample(wtB, size = 1)
      for (i in 2:length(seriesB$es)) {
        tmp = surB[i - 1] + sample(wtB, size = 1)
        if (tmp > span[Tlen]) {
          break
        }
        else {
          surB[i] = tmp
        }
      }
      K_prec_sur = 0
      for (step_a in 1:N_A) {
        if (!is.element(surA[step_a], span)) {
          next
        }
        for (step_b in 1:N_B) {
          if (!is.element(surB[step_b], span)) {
            next
          }
          if (sym == FALSE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(0:delT))) {
              K_prec_sur = K_prec_sur + 1
              break
            }
          }
          if (sym == TRUE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(-delT:delT))) {
              K_prec_sur = K_prec_sur + 1
              break
            }
          }
        }
      }
      surdist[1, surno] = K_prec_sur/N_A
      K_trigg_sur = 0
      for (step_b in 1:N_B) {
        if (!is.element(surB[step_b], span)) {
          next
        }
        for (step_a in 1:N_A) {
          if (!is.element(surA[step_a], span)) {
            next
          }
          if (sym == FALSE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(0:delT))) {
              K_trigg_sur = K_trigg_sur + 1
              break
            }
          }
          if (sym == TRUE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(-delT:delT))) {
              K_trigg_sur = K_trigg_sur + 1
              break
            }
          }
        }
      }
      surdist[2, surno] = K_trigg_sur/N_B
    }
    Pprec = 1 - ecdf(surdist[1, ])(CRprec)
    Ptrigg = 1 - ecdf(surdist[2, ])(CRtrigg)
  }
  if (sigtest == "shuffle.surrogate") {
    surdist = matrix(NA, 2, reps)
    span = seq(1:steplen)
    for (surno in 1:reps) {
      surA = sample(span, size = N_A)
      surB = sample(span, size = N_B)
      K_prec_sur = 0
      for (step_a in 1:N_A) {
        for (step_b in 1:N_B) {
          if (sym == FALSE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(0:delT))) {
              K_prec_sur = K_prec_sur + 1
              break
            }
          }
          if (sym == TRUE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(-delT:delT))) {
              K_prec_sur = K_prec_sur + 1
              break
            }
          }
        }
      }
      surdist[1, surno] = K_prec_sur/N_A
      K_trigg_sur = 0
      for (step_b in 1:N_B) {
        for (step_a in 1:N_A) {
          if (sym == FALSE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(0:delT))) {
              K_trigg_sur = K_trigg_sur + 1
              break
            }
          }
          if (sym == TRUE) {
            if (is.element(((surA[step_a] - tau) - surB[step_b]), 
                           c(-delT:delT))) {
              K_trigg_sur = K_trigg_sur + 1
              break
            }
          }
        }
      }
      surdist[2, surno] = K_trigg_sur/N_B
    }
    Pprec = 1 - ecdf(surdist[1, ])(CRprec)
    Ptrigg = 1 - ecdf(surdist[2, ])(CRtrigg)
  }
  sig_testprec = logical
  if (Pprec >= alpha) {
    sig_testprec = TRUE
  } else {
    sig_testprec = FALSE
  }
  sig_testtrigg = logical
  if (Ptrigg >= alpha) {
    sig_testtrigg = TRUE
  }  else {
    sig_testtrigg = FALSE
  }
  CA_out = list(sig_testprec,  Pprec,  CRprec,  K_prec,  N_A,
                sig_testtrigg, Ptrigg, CRtrigg, K_trigg, N_B)
  names(CA_out) = c("NH precursor", "p-value precursor", "precursor coincidence rate", 
                    "precursor events", "seriesA events",
                    "NH trigger",   "p-value trigger",   "trigger coincidence rate",  
                    "trigger events", "seriesB events")
  return(CA_out)
}
lineCC <- function(dat, days){
  # This function will take the data given (combined binary time series), split 
  #      into two series and then send the data into the CoinCalc::CC.eca.ts 
  #      function using the days(delT) specified by the user. The data will then
  #      be taken out of the function results list and put into a data frame 
  #      before being returned to the call function.
  
  # dat : matrix containing the combined binary time series
  # days : delT how much of a delay allowed for sequential events
  require(CoinCalc)
  
  # Variables ------------------------------------------------------------------
  # print(dat[1])
  # dat <- dat[2:length(dat)]
  dat <- dat[1:length(dat)]
  seriesA <- dat[1:(length(dat)/2)]
  seriesB <- dat[(length(dat)/2 + 1): length(dat)]
  # need to make sure its a matrix
  days <- days
  
  # Calculations ---------------------------------------------------------------
  # Check to make sure they are the same dimentions
  if (length(seriesA) == length(seriesB)){
    #  Correlation
    if (days == 0){
      correl <- cor.test(seriesA, seriesB, method = 'pearson')
    } else {
      # # Day
      # correl1 <- cor.test(seriesA[1:(length(seriesA)-1)], seriesB[2:length(seriesB)], method = 'pearson')
      # correl2 <- cor.test(seriesA[1:(length(seriesA)-2)], seriesB[3:length(seriesB)], method = 'pearson')
      # correl3 <- cor.test(seriesA[1:(length(seriesA)-3)], seriesB[4:length(seriesB)], method = 'pearson')
      # correl4 <- cor.test(seriesA[1:(length(seriesA)-4)], seriesB[5:length(seriesB)], method = 'pearson')
      # correl5 <- cor.test(seriesA[1:(length(seriesA)-5)], seriesB[6:length(seriesB)], method = 'pearson')
      # correl6 <- cor.test(seriesA[1:(length(seriesA)-6)], seriesB[7:length(seriesB)], method = 'pearson')
      # correl7 <- cor.test(seriesA[1:(length(seriesA)-7)], seriesB[8:length(seriesB)], method = 'pearson')
      # correl <- max(correl1,correl2,correl3,correl4,correl5,correl6,correl7)
      # Month and Week
      correl <- cor.test(seriesA[1:(length(seriesA)-1)], seriesB[2:length(seriesB)], method = 'pearson')
    }
    #  CoinCalc
    # need to make sure its a matrix & class(seriesA)[1] == 'matrix' & class(seriesB)[1] == 'matrix'
    result <- CC.eca.ts2(seriesA=seriesA, seriesB=seriesB, delT = days)
    result <- cbind(result[["NH precursor"]],result[["p-value precursor"]],
                    result[["precursor coincidence rate"]],
                    result[["precursor events"]],result[["seriesA events"]],
                    result[["NH trigger"]],  result[["p-value trigger"]],  
                    result[["trigger coincidence rate"]],
                    result[["trigger events"]],result[["seriesB events"]],
                    correl$estimate)
    # Result c(Accept the null hypothesis precursor: T(1)/F(0), p-value precursor,
    #          rate in time frame precursor, number of combined events, 
    #          number of events in series A,
    #          Accept the null hypothesis trigger: T(1)/F(0),p-value trigger, 
    #          rate in time frame trigger, number of combined events, 
    #          number of events in series B
    #          )
  } else {
    result <- c('ERROR','ERROR','ERROR','ERROR','ERROR','ERROR','ERROR',
                'ERROR','ERROR','ERROR','ERROR')
  }
  return(result)
}
# Part II Opening files & Formatting ###########################################
A <- Sys.time()
print(paste0('Starting to open the required file at: ',A))
# Tmax
series1 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime, var[1],
                           mFile, startyr,'-',endyr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
lonlat <- series1[,1:2]
ID <- rowid_to_column(series1); ID <- ID[,1]; ID <- as.matrix(ID) # For testing
series1 <- series1[,3:ncol(series1)]
series1[series1 > 0] <- 1     # Needs to be converted to binary (0,1)
series1 <- as.matrix(series1)

# Pr
if (wavesTime == 'WAVES_DAY_'){
  series3 <- read_csv(paste0(fileloc1, loc1, loc2, 'EXCEED_DAY_', var[3],
                           mFile, startyr,'-',endyr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
} else {
  series3 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime, var[3],
                             mFile, startyr,'-',endyr,'.csv'),
                      col_names = TRUE, cols(.default = col_double()))
}
series3 <- series3[,3:ncol(series3)]
series3[series3 > 0] <- 1     # Needs to be converted to binary (0,1)
series3 <- as.matrix(series3)

# mrsos
if (wavesTime == 'WAVES_DAY_'){
  series4 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime, var[4],
                             mFile, startyr,'-',endyr,'.csv'),
                      col_names = TRUE, cols(.default = col_double()))
} else {
  series4 <- read_csv(paste0(fileloc1, loc1, loc2, wavesTime2, var[4],
                             mFile, startyr,'-',endyr,'.csv'),
                      col_names = TRUE, cols(.default = col_double()))
}
series4 <- series4[,3:ncol(series4)]
series4[series4 > 0.0357] <- 0 # 28 days exceeded 0.0357
series4[series4 > 0] <- 1     # Needs to be converted to binary (0,1)
series4 <- as.matrix(series4)

if (wavesTime == 'WAVES_DAY_'){
  a <-strsplit(wavesTime,'_') %>% unlist()
  a <- a[2]
  seqTime <- 7
} else if (wavesTime == 'WAVES_WEEK_'){
  a <-strsplit(wavesTime2,'_') %>% unlist()
  a <- paste0(a[2],'_',a[3])
  seqTime <- 1
} else if (wavesTime == 'WAVES_MONTH_'){
  a <-strsplit(wavesTime2,'_') %>% unlist()
  a <- paste0(a[2],'_',a[3])
  seqTime <- 1
}else {
  print( 'Variable wavesTime not reconized.')
}

# Part III Heatwave & Drought ##################################################
# . 3.1 Sim. Heatwave & Drought ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Heatwave & Drought at: ',B))

if (dim(series1)[1] == dim(series4)[1] | dim(series1)[2] == dim(series4)[2]){
  seriesC <- cbind(series1, series4)
  # seriesC <- cbind(ID, series1, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = 0) %>%
    t() 
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events', 
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SIM14',
                                mFile,startyr,'-',endyr,'.csv'), 
            row.names = FALSE)
}
B <- Sys.time()
print(paste0('Finished calculated the simultanious Heatwave & Drought at: ',B))

# . 3.2 Seq. Heatwave & Drought ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Heatwave & Drought at: ',B))

if (dim(series1)[1] == dim(series4)[1] | dim(series1)[2] == dim(series4)[2]){
  seriesC <- cbind(series1, series4)
  # seriesC <- cbind(ID, series1, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = seqTime) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events',
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ14',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the sequential Heatwave & Drought at: ',B))

# . 3.3 Seq. Drought & Heatwave ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Drought & Heatwave at: ',B))

if (dim(series4)[1] == dim(series1)[1] | dim(series4)[2] == dim(series1)[2]){
  seriesC <- cbind(series4, series1)
  # seriesC <- cbind(ID, series4, series1)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = seqTime) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events',
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ41',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}
B <- Sys.time()
print(paste0('Finished calculating the sequential Drought & Heatwave at: ',B))

# Part IV Extreme Precip. & Heatwave ###########################################
# . 4.1 Sim. Extreme Precip. & Heatwave ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Heatwave & Ex. Precip. at: ',B))

if (dim(series1)[1] == dim(series3)[1] | dim(series1)[2] == dim(series3)[2]){
  seriesC <- cbind(series1, series3)
  # seriesC <- cbind(ID, series1, series3)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = 0) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events',
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SIM13',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the simultanious Heatwave & Ex. Precip. at: ',B))

# . 4.2 Seq. Heatwave -> Extreme Precip. ---------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Heatwave & Ex. Precip. at: ',B))

if (dim(series1)[1] == dim(series3)[1] | dim(series1)[2] == dim(series3)[2]){
  seriesC <- cbind(series1, series3)
  # seriesC <- cbind(ID, series1, series3)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = seqTime) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events',
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ13',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the sequential Heatwave & Ex. Precip. at: ',B))

# . 4.3 Seq. Extreme Precip -> Heatwave ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Ex. Precip. & Heatwave at: ',B))

if (dim(series3)[1] == dim(series1)[1] | dim(series3)[2] == dim(series1)[2]){
  seriesC <- cbind(series3, series1)
  # seriesC <- cbind(ID, series3, series1)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = seqTime) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events',
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ31',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the simultanious Ex. Precip. & Heatwave at: ',B))
# Part V Extreme Precip. & Drought #############################################
# . 5.1 Seq. Extreme Precip. -> Drought ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Ex. Precip. &  Drought at: ',B))

if (dim(series3)[1] == dim(series4)[1] | dim(series3)[2] == dim(series4)[2]){
  seriesC <- cbind(series3, series4)
  # seriesC <- cbind(ID, series3, series4)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = seqTime) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events',
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ34',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finished calculating the sequential Ex. Precip. &  Drought at: ',B))

# . 5.2 Seq. Drought -> Extreme Precip. ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Drought & Ex. Precip. at: ',B))

if (dim(series4)[1] == dim(series3)[1] | dim(series4)[2] == dim(series3)[2]){
  seriesC <- cbind(series4, series3)
  # seriesC <- cbind(ID, series4, series3)
  seriesC <- apply(X=seriesC, MARGIN = 1, FUN=lineCC, days = seqTime) %>%
    t()
  seriesC<- cbind(lonlat, seriesC)
  colnames(seriesC) <- c('lon','lat',
                         'Hypo_precur','Pvalue_precur','Prec_Coin_Rate',
                         'Prec_Events','SeriesA_Events',
                         'Hypo_trig',  'Pvalue_trig', 'Trig_Coin_Rate',
                         'Trig_Events','SeriesB_Events','Correlation')
  write.csv(seriesC,file=paste0(fileloc1,loc1,loc2,'COMP_',a,'_SEQ43',
                                mFile,startyr,'-',endyr,'.csv'),
            row.names = FALSE)
}

B <- Sys.time()
print(paste0('Finsihed calculating the sequential Drought & Ex. Precip. at: ',B))

# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating the compound events.',
             'End time: ',B, 'Total time elapsed: ', B-A))
print("-----------------------------------------------------------------------")