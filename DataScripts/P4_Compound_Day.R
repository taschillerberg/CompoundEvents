# P6_Compound_Day.R
# About: This program will open the exceedance file for the selected 
#       variable and calculate a 'wave' of when the exceedance occurs.
#
# Inputs: EXCEED & WAVES
# Outputs: COMPOUND
#
# T. A. Schillerberg
#               Oct. 2022
#      Updated: Jan. 2023

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
mNum <- 1 # Select a model (1-4)
mFile <- c('_day_EC-Earth3_historical_r1i1p1f1_gr_',
           '_day_GFDL-ESM4_esm-hist_r1i1p1f1_gr1_',
           '_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_',
           '_day_MRI-ESM2-0_historical_r1i1p1f1_gn_')[mNum]
# mFile <- c('_day_EC-Earth3_ssp126_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp126_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp126_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp126_r1i1p1f1_gn_')[mNum]
# mFile <- c('_day_EC-Earth3_ssp585_r1i1p1f1_gr_',
#            '_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_',
#            '_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_',
#            '_day_MRI-ESM2-0_ssp585_r1i1p1f1_gn_')[mNum]
loc1 <- c('CMIP6_historical/','CMIP6_SSP126/','CMIP6_SSP585/')[1]
loc2 <- c('EC-Earth3/',
          'GFDL-ESM4/',
          'MPI-ESM1-2-HR/',
          'MRI-ESM2-0/')[mNum]
var <- c('tasmax', 'tasmin', 'pr', 'mrsos')
startyr <- 1980
endyr <- 2010
core <- 23

print(paste0('Model: ',loc2))
print(paste0('Variable: ', var))
print('Rscript: P6_Compound_Day.R')
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
  CA_out = list(sig_testprec, sig_testtrigg, Pprec, Ptrigg, 
                CRprec, CRtrigg)
  names(CA_out) = c("NH precursor", "NH trigger", "p-value precursor", 
                    "p-value trigger", "precursor coincidence rate", "trigger coincidence rate")
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
  print(ID)
  seriesA <- dat[["seriesA"]][ID,]
  seriesB <- dat[['seriesB']][ID,]
  days <- dat[['days']]
  
  # Calculations ---------------------------------------------------------------
  # Check to make sure they are the same dimentions
  if (length(seriesA) == length(seriesB)){
    # need to make sure its a matrix & class(seriesA)[1] == 'matrix' & class(seriesB)[1] == 'matrix'
    result <- CC.eca.ts2(seriesA=seriesA, seriesB=seriesB, delT = days)
    result <- cbind(result[["NH precursor"]], result[["NH trigger"]],
                    result[["p-value precursor"]], result[["p-value trigger"]],
                    result[["precursor coincidence rate"]], result[["trigger coincidence rate"]])
    # Result c(Accept the null hypothesis precursor: T(1)/F(0), Accept the null hypothesis trigger: T(1)/F(0),
    #          p-value precursor, p-value trigger,
    #          rate in time frame precursor, rate in time frame trigger)
  } else {
    result <- c(ID,'ERROR','ERROR','ERROR','ERROR','ERROR')
  }
  return(result)
}
# Part II Opening files & Formatting ###########################################
A <- Sys.time()
print(paste0('Starting to open the required file at: ',A))
# Tmax
series1 <- read_csv(paste0(fileloc1, loc1, loc2, 'WAVES_DAY_', var[1],
                           mFile, startyr,'-',endyr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
series1$lat[series1$lat <= -60] <- NA     # Removing Antarctica values
series1 <- na.omit(series1)   # Removing Antarctica values
lonlat <- series1[,1:2]
series1 <- series1[,3:ncol(series1)]
series1[series1 > 0] <- 1     # Needs to be converted to binary (0,1)
series1 <- as.matrix(series1)

# Pr
series3 <- read_csv(paste0(fileloc1, loc1, loc2, 'EXCEED_DAY_', var[3],
                           mFile, startyr,'-',endyr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
series3$lat[series3$lat <= -60] <- NA     # Removing Antarctica values
series3 <- na.omit(series3)   # Removing Antarctica values
series3 <- series3[,3:ncol(series3)]
series3[series3 > 0] <- 1     # Needs to be converted to binary (0,1)
series3 <- as.matrix(series3)

# mrsos
series4 <- read_csv(paste0(fileloc1, loc1, loc2, 'WAVES_DAY_', var[4],
                           mFile, startyr,'-',endyr,'.csv'),
                    col_names = TRUE, cols(.default = col_double()))
series4$lat[series4$lat <= -60] <- NA     # Removing Antarctica values
series4 <- na.omit(series4)   # Removing Antarctica values
lonlat <- series4[,1:2]
series4 <- series4[,3:ncol(series4)]
series4[series4 > 0] <- 1     # Needs to be converted to binary (0,1)
series4 <- as.matrix(series4)

# Part III Heatwave & Drought ##################################################
# . 3.1 Sim. Heatwave & Drought ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Heatwave & Drought at: ',B))

ID <- 1 : nrow(series1)
dat <- list(
  'days' = 0,
  'seriesA' = series1,
  'seriesB' = series4
)
datSim <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = 6, byrow = TRUE)
datSim <- cbind(lonlat, datSeq)
colnames(datSim) <- c('lon','lat','Hypo_precur','Hypo_trig',
                        'Pvalue_precur','Pvalue_trig',
                        'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSim,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SIM14_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE)

# . 3.2 Seq. Heatwave & Drought ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Heatwave & Drought at: ',B))

dat <- list(
  'days' = 7,
  'seriesA' = series1,
  'seriesB' = series4
)
datSeq <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = length(month), byrow = TRUE)
datSeq <- cbind(lonlat, datSeq)
colnames(datSeq) <- c('lon','lat','Hypo_precur','Hypo_trig',
                      'Pvalue_precur','Pvalue_trig',
                      'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSeq,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SEQ14_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE)

# . 3.2 Seq. Drought & Heatwave ------------------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Drought & Heatwave at: ',B))

dat <- list(
  'days' = 7,
  'seriesA' = series4,
  'seriesB' = series1
)
datSeq <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = length(month), byrow = TRUE)
datSeq <- cbind(lonlat, datSeq)
colnames(datSeq) <- c('lon','lat','Hypo_precur','Hypo_trig',
                      'Pvalue_precur','Pvalue_trig',
                      'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSeq,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SEQ41_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE)

# Part IV Extreme Precip. & Heatwave ###########################################
# . 4.1 Sim. Extreme Precip. & Heatwave ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Heatwave & Ex. Precip. at: ',B))

dat <- list(
  'days' = 0,
  'seriesA' = series1,
  'seriesB' = series3
)
datSim <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = length(month), byrow = TRUE)
datSim <- cbind(lonlat, datSeq)
colnames(datSim) <- c('lon','lat','Hypo_precur','Hypo_trig',
                      'Pvalue_precur','Pvalue_trig',
                      'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSim,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SIM13_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE)

# . 4.2 Seq. Heatwave -> Extreme Precip. ---------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Heatwave & Ex. Precip. at: ',B))

dat <- list(
  'days' = 7,
  'seriesA' = series1,
  'seriesB' = series3
)
datSeq <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = length(month), byrow = TRUE)
datSeq <- cbind(lonlat, datSeq)
colnames(datSeq) <- c('lon','lat','Hypo_precur','Hypo_trig',
                      'Pvalue_precur','Pvalue_trig',
                      'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSeq,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SEQ13_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE)

# . 4.3 Seq. Extreme Precip -> Heatwave ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the simultanious Ex. Precip. & Heatwave at: ',B))

dat <- list(
  'days' = 7,
  'seriesA' = series3,
  'seriesB' = series1
)
datSeq <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = length(month), byrow = TRUE)
datSeq <- cbind(lonlat, datSeq)
colnames(datSeq) <- c('lon','lat','Hypo_precur','Hypo_trig',
                      'Pvalue_precur','Pvalue_trig',
                      'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSeq,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SEQ31_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE)
# Part V Extreme Precip. & Drought #############################################
# . 5.1 Seq. Extreme Precip. -> Drought ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Ex. Precip. &  Drought at: ',B))

dat <- list(
  'days' = 7,
  'seriesA' = series3,
  'seriesB' = series4
)
datSeq <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = length(month), byrow = TRUE)
datSeq <- cbind(lonlat, datSeq)
colnames(datSeq) <- c('lon','lat','Hypo_precur','Hypo_trig',
                      'Pvalue_precur','Pvalue_trig',
                      'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSeq,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SEQ34_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE)

# . 5.2 Seq. Drought -> Extreme Precip. ----------------------------------------
B <- Sys.time()
print(paste0('Starting to calculate the sequential Drought & Ex. Precip. at: ',B))

dat <- list(
  'days' = 7,
  'seriesA' = series4,
  'seriesB' = series3
)
datSeq <- parallel::mclapply(X=ID, FUN=lineCC, dat=dat, mc.cores=core) %>%
  unlist() %>%
  matrix(ncol = length(month), byrow = TRUE)
datSeq <- cbind(lonlat, datSeq)
colnames(datSeq) <- c('lon','lat','Hypo_precur','Hypo_trig',
                      'Pvalue_precur','Pvalue_trig',
                      'Prec_Coin_Rate','Trig_Coin_Rate')
write.csv(datSeq,file=paste0(fileloc1,loc1,loc2,'COMP_DAY_SEQ43_',
                               model,startyr,'-',endyr,'.csv'), 
          row.names = FALSE) 

# END ##########################################################################
B <- Sys.time()
print(paste0('Finished calculating the compound events.',
             'End time: ',B, 'Total time elapsed: ', B-A))