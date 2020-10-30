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
path.output_data <- Sys.getenv("path.output_data")

source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))
source(file.path(path.code, "geemMod.r"))
environment(geemMod) <- asNamespace("geeM")

###############################################################################
# User-specified design parameters
###############################################################################
this.folder <- "sim_study_main/sim_results_alternative"

this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

input.means <- read.csv(file.path(path.output_data, this.folder, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(path.output_data, this.folder, "input_prop_zeros.csv"))

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

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

###############################################################################
# Plot EDTR mean trajectories
###############################################################################
plotdat <- data.frame(month = seq(1,input.tot.time,1),
                      mu.plusplus = u.plusplus,
                      mu.plusminus = u.plusminus,
                      mu.minusplus = u.minusplus,
                      mu.minusminus = u.minusminus)

plot(-1, 
     type="n",
     xlim = c(1,input.tot.time),
     ylim = c(0,1+ceiling(max(plotdat[,2:ncol(plotdat)]))),
     xaxt="n",
     yaxt="n",
     xlab = "Month",
     ylab = "Mean Past-Month No. of Cocaine-Use Days")

axis(1, at = seq(1,input.tot.time,1))
axis(2, at = seq(0, 1+ceiling(max(plotdat[,2:ncol(plotdat)])), 0.5))

title(main = "EDTR Mean Trajectories")

lines(plotdat$month, plotdat$mu.plusplus, type = "o", lwd=3, col="darkgrey", lty=1)
lines(plotdat$month, plotdat$mu.minusplus, type = "o", lwd=3, col="black", lty=2)
lines(plotdat$month, plotdat$mu.plusplus, type = "o", lwd=3, col="goldenrod", lty=3)
lines(plotdat$month, plotdat$mu.minusplus, type = "o", lwd=3, col="lightblue", lty=4)

