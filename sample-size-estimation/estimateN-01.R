###############################################################################
# Specify working correlation structure
###############################################################################
use.working.corr <- "ar1"

###############################################################################
# Specify desired power
###############################################################################
input.power <- 0.80

###############################################################################
# Script begins
###############################################################################

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

load(file.path(path.output_data, use.working.corr, "estimated-covmat-01.RData"))
path.code <- Sys.getenv("path.code")  # Need to change paths back
path.input_data <- Sys.getenv("path.input_data")   # Need to change paths back
path.output_data <- Sys.getenv("path.output_data")   # Need to change paths back


for(i in 1:length(collect.estimates)){
  idx.params <- 2*i - 1 
  idx.est <- 2*i
  
  # Calculate "true" sandwich term
  datagen.params <- collect.estimates[[idx.params]]
  estimates <- collect.estimates[[idx.est]]
  
  # Calculate DeltaQ
  idx.input.means <- datagen.params$input.means
  input.means <- list.input.means[[idx.input.means]]
  source(file.path(path.code, "calc-truth.R"))
  
  # Calculate N.required
  z.eta <- qnorm(1-input.power)
  z.alpha <- qnorm(input.alpha/2)
  const.eos.means <- ((z.eta + z.alpha)/diff.eos.means.plusplus.minusplus)^2
  const.change.score <- ((z.eta + z.alpha)/diff.change.score.plusplus.minusplus)^2
  
  # ADD MORE HERE LATER
  
}

