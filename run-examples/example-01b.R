path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)

source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

# -----------------------------------------------------------------------------
# Inputs from user
# -----------------------------------------------------------------------------
input.tot.time <- 6  # Total no. of measurement occasions
input.rand.time <- 2  # Time when second randomization occurred (time is 1-indexed)
input.tau.max <- 0.1  # Maximum correlation between time-specific outcomes under any DTR
input.alpha <- 0.05  # Type-I error rate
type.ii.error <- 0.20  # Type-II error rate

# Choices of values for this.pair are c(1,2,3,4,5,6) where
#     1: plusplus vs. plusminus
#     2: plusplus vs. minusplus
#     3: plusplus vs. minusminus
#     4: plusminus vs. minusplus
#     5: plusminus vs. minusminus
#     6: minusminus vs. minusplus
this.pair <- 2  # compare DTR plusplus vs. minusplus

# -----------------------------------------------------------------------------
# Read and prepare input data
# -----------------------------------------------------------------------------
# input.means contains mean of time-specific outcomes under each 
# treatment sequence from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), 
                        header = TRUE)
# input.prop.zeros contains proportion of individuals having time-specific outcomes
# equal to zero under each treatment sequence from time 1 until tot.time
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"), 
                             header = TRUE)
# Check whether input data are in the correct format
CheckInputData(input.df=input.means, rand.time=input.rand.time, tot.time=input.tot.time)
CheckInputData(input.df=input.prop.zeros, rand.time=input.rand.time, tot.time=input.tot.time)

# -----------------------------------------------------------------------------
# Calibration step
# -----------------------------------------------------------------------------
source(file.path(path.code, "calibrate-params.R"))

# -----------------------------------------------------------------------------
# Calculate standardized effect size corresponding to rho.star
# -----------------------------------------------------------------------------
source(file.path(path.code, "calc-delta.R"))

# -----------------------------------------------------------------------------
# Estimate sample size
# -----------------------------------------------------------------------------

###############################################################################
# Primary Aim: Compare DTRs based on end-of-study means
###############################################################################
what.quantity <- "eos.means" # choices are "eos.means" or "AUC"
write.csv(delta.eos.means, file.path(path.output_data, "delta.eos.means.csv"), row.names=FALSE)
source(file.path(path.code, "estimate-sample-size.R"))

###############################################################################
# Secondary Aim: Compare DTRs based on AUC
###############################################################################
what.quantity <- "AUC" # choices are "eos.means" or "AUC"
write.csv(delta.AUC, file.path(path.output_data, "delta.AUC.csv"), row.names=FALSE)
source(file.path(path.code, "estimate-sample-size.R"))

###############################################################################
# Secondary Aim: Compare DTRs based on change score
###############################################################################
what.quantity <- "change.score" # choices are "eos.means" or "AUC" or "change.score"
write.csv(delta.AUC, file.path(path.output_data, "delta.change.score.csv"), row.names=FALSE)
source(file.path(path.code, "estimate-sample-size.R"))
