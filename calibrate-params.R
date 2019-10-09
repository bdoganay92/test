library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")

source(file.path(path.code, "input-utils.R"))
source(file.path(path.code, "datagen-utils.R"))

# -----------------------------------------------------------------------------
# Read and prepare input parameters
# -----------------------------------------------------------------------------

N <- 500  # Total no. of individuals
tot.time <- 12  # Total no. of time points
rand.time <- 4  # Time when second randomization occurred (time is 1-indexed)
cutoff <- 1  # Cutoff in the definition of response status

# input.means contains mean outcome under each treatment sequence
# from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), header = TRUE)

# input.prop.zeros contains proportion of zeros in the outcome under each 
# treatment sequence from time 1 until tot.time
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"), header = TRUE)

# -----------------------------------------------------------------------------
# Begin tasks
# -----------------------------------------------------------------------------

nsim <- 1000
gridx <- as.list(seq(0, 1, 0.1))  # rho
list.empirical.corr.max <- list()
list.empirical.corr.min <- list()
list.empirical.corr.ave <- list()

for(i in 1:length(gridx)){
  rho <- gridx[[i]]
  for(j in 1:nsim){
    source(file.path(path.code, "sim-po-dat.R"))
    empirical.corr <- DTRCorrelationPO(df.potential.Yit)
    
    empirical.corr.max <- c(i=i, rho=rho, sim=j, empirical.corr$rho.star.max)
    empirical.corr.max <- t(data.frame(empirical.corr.max))
    list.empirical.corr.max <- append(list.empirical.corr.max, list(empirical.corr.max))
    
    empirical.corr.min <- c(i=i, rho=rho, sim=j, empirical.corr$rho.star.min)
    empirical.corr.min <- t(data.frame(empirical.corr.min))
    list.empirical.corr.min <- append(list.empirical.corr.min, list(empirical.corr.min))
    
    empirical.corr.ave <- c(i=i, rho=rho, sim=j, empirical.corr$rho.star.ave)
    empirical.corr.ave <- t(data.frame(empirical.corr.ave))
    list.empirical.corr.ave <- append(list.empirical.corr.ave, list(empirical.corr.ave))
  }
}

empirical.corr.max <- do.call(rbind, list.empirical.corr.max)
empirical.corr.min <- do.call(rbind, list.empirical.corr.min)
empirical.corr.ave <- do.call(rbind, list.empirical.corr.ave)

est.corr.max <- empirical.corr.max %>%
  as.data.frame(.) %>% 
  group_by(i, rho) %>% 
  summarise(plusplus = mean(plusplus),
            plusminus = mean(plusminus),
            minusplus = mean(minusplus),
            minusminus = mean(minusminus))

est.corr.min <- empirical.corr.min %>%
  as.data.frame(.) %>% 
  group_by(i, rho) %>% 
  summarise(plusplus = mean(plusplus),
            plusminus = mean(plusminus),
            minusplus = mean(minusplus),
            minusminus = mean(minusminus))

est.corr.ave <- empirical.corr.ave %>%
  as.data.frame(.) %>% 
  group_by(i, rho) %>% 
  summarise(plusplus = mean(plusplus),
            plusminus = mean(plusminus),
            minusplus = mean(minusplus),
            minusminus = mean(minusminus))

