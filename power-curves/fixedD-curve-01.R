###############################################################################
# Specify working correlation structure
###############################################################################
use.working.corr <- "ar1"

###############################################################################
# Script begins
###############################################################################
start.time <- Sys.time()

library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

###############################################################################
# Specify inputs
###############################################################################
input.alpha <- 0.05
input.rand.time <- 2
input.tot.time <- 6
list.input.rho <- list(0.1, 0.5, 0.9)
input.cutoff <- 0
names.seq <- matrix(c("plus.r", "plus.nr.plus", "plus.nr.minus", 
                      "minus.r", "minus.nr.plus", "minus.nr.minus"), 
                    ncol=1, dimnames = list(NULL, "seq"))
this.pair <- 2 # Compare DTR plus.plus vs. DTR minus.plus

###############################################################################
# Specify contrasts of interest
###############################################################################
# Create C matrix
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
C.plusplus <- list.C$C.plusplus
C.plusminus <- list.C$C.plusminus
C.minusplus <- list.C$C.minusplus
C.minusminus <- list.C$C.minusminus

# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in change score
L.change.score <- -t(eCol(input.rand.time, input.tot.time)) + t(eCol(input.tot.time, input.tot.time))
D.change.score <- cbind(L.change.score, -L.change.score)

###############################################################################
# Create shortlist.input.means data frames for a fixed choice of 
# difference in end-of-study means or change score between DTRs ++ and -+
# This will be used to calculate power when standardized effect size is fixed
# and N is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- 2.8

shortlist.input.means <- list()
for(i in 1:length(increments)){
  k <- increments[i]
  tmpdat <- dat
  tmpdat[tmpdat$seq=="minus.r","time.6"] <- tmpdat[tmpdat$seq=="minus.r","time.6"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] + k
  shortlist.input.means <- append(shortlist.input.means, list(tmpdat))
}

###############################################################################
# Create input.prop.zeros data frames
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 0.6)
input.prop.zeros <- dat

###############################################################################
# Calculate power: difference in eos means or change score
# =============================================================================
# Standardized effect size is held fixed while N is varied
###############################################################################
list.input.N <- as.list(seq(120, 600, by = 10))
collect.power <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(shortlist.input.means)){
    input.means <- shortlist.input.means[[j]]
    
    for(l in 1:length(list.input.N)){
      input.N <- list.input.N[[l]]
      source(file.path(path.code,"calc-power.R"))
      power.diff.eos.means$idx.input.means <- j
      power.diff.change.score$idx.input.means <- j
      tmp.power <- list(eos.means = power.diff.eos.means[power.diff.eos.means$pair==this.pair,],
                        change.score = power.diff.change.score[power.diff.change.score$pair==this.pair,]
      )
      collect.power <- append(collect.power, list(tmp.power))
    }
  }
}

end.time <- Sys.time()
###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, use.working.corr, "fixedD-curve-01.RData"))

