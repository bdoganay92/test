# In this script, N is fixed while effect size is varied

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
input.N <- 300
input.rand.time <- 2
input.tot.time <- 6
list.input.rho <- list(0.2, 0.5, 0.8)
input.cutoff <- 0
names.seq <- matrix(c("plus.r", "plus.nr.plus", "plus.nr.minus", 
                      "minus.r", "minus.nr.plus", "minus.nr.minus"), 
                    ncol=1, dimnames = list(NULL, "seq"))

###############################################################################
# Create input.means data frames
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- seq(0,5,0.2)

list.input.means <- list()
for(i in 1:length(increments)){
  k <- increments[i]
  tmpdat <- dat
  tmpdat[tmpdat$seq=="minus.r","time.6"] <- tmpdat[tmpdat$seq=="minus.r","time.6"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] + k
  list.input.means <- append(list.input.means, list(tmpdat))
}

###############################################################################
# Create input.prop.zeros data frames
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 0.6)
input.prop.zeros <- dat

# -----------------------------------------------------------------------------
# Use working independence correlation structure
# -----------------------------------------------------------------------------
use.working.corr <- "independence"

###############################################################################
# Calculate power
###############################################################################
this.pair <- 2 # Compare DTR plus.plus vs. DTR minus.plus
collect.power <- list()
collect.delta <- list()
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(list.input.means)){
    input.means <- list.input.means[[j]]
    source(file.path(path.code,"calc-power.R"))
    tmp.power <- power.diff.eos.means[power.diff.eos.means$pair==this.pair,]
    tmp.power$idx.input.means <- j
    collect.power <- append(collect.power, list(tmp.power))
    source(file.path(path.code,"calc-delta.R"))
    tmp.delta <- delta.eos.means[delta.eos.means$pair==this.pair,]
    tmp.delta$idx.input.means <- j
    collect.delta <- append(collect.delta, list(tmp.delta))
    source(file.path(path.code,"calc-correlation.R"))
    empirical.corr$idx.input.means <- j
    collect.correlation <- append(collect.correlation, list(empirical.corr))
  }
}

