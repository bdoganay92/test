library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)
library(gridExtra)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))
source(file.path(path.code, "geemMod.r"))
environment(geemMod) <- asNamespace("geeM")

###############################################################################
# User-specified design parameters
###############################################################################
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0
input.means <- read.csv(file.path(path.input_data, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"))

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

###############################################################################
# Vary the means
###############################################################################
d <- 1
input.means$time.3[4:5] <- input.means$time.3[4:5] + 0.10*d
input.means$time.4[4:5] <- input.means$time.4[4:5] + 0.50*d
input.means$time.5[4:5] <- input.means$time.5[4:5] + 0.90*d
input.means$time.6[4:5] <- input.means$time.6[4:5] + d

###############################################################################
# Vary the proportion of zeros
###############################################################################
m <- 1

input.prop.zeros$time.3[1:2] <- input.prop.zeros$time.3[1:2] * m
input.prop.zeros$time.4[1:2] <- input.prop.zeros$time.4[1:2] * m
input.prop.zeros$time.5[1:2] <- input.prop.zeros$time.5[1:2] * m
input.prop.zeros$time.6[1:2] <- input.prop.zeros$time.6[1:2] * m

input.prop.zeros$time.3[4:5] <- input.prop.zeros$time.3[4:5] * m
input.prop.zeros$time.4[4:5] <- input.prop.zeros$time.4[4:5] * m
input.prop.zeros$time.5[4:5] <- input.prop.zeros$time.5[4:5] * m
input.prop.zeros$time.6[4:5] <- input.prop.zeros$time.6[4:5] * m

###############################################################################
# Specify L and D matrices for contrasts of interest 
# Below, these are specified for end-of-study means and AUC
###############################################################################
# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in AUCs
# This specification assumes that measurement occasions will be 1-month apart
for(i in 1:input.tot.time){
  if(input.tot.time==2){
    L.AUC <- (1/2)*t(eCol(1,input.tot.time)) + (1/2)*t(eCol(input.tot.time,input.tot.time))
  }else if(input.tot.time>2 & i==1){
    L.AUC <- (1/2)*t(eCol(1,input.tot.time))
  }else if(input.tot.time>2 & i==input.tot.time){
    L.AUC <- L.AUC+(1/2)*t(eCol(input.tot.time,input.tot.time))
  }else{
    L.AUC <- L.AUC+t(eCol(i,input.tot.time))
  }
}
D.AUC <- cbind(L.AUC,-L.AUC)

###############################################################################
# Check truth
###############################################################################
source(file.path(path.code, "calc-truth-beta.R"))
source(file.path(path.code, "calc-truth-contrasts.R"))

print(p)
print(q)
print(diff.eos.means.plusplus.minusplus)
print(diff.AUC.plusplus.minusplus)

