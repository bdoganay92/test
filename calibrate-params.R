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
rho <- 0.7  # Dependence parameter

# input.means contains mean outcome under each treatment sequence
# from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), header = TRUE)

# input.prop.zeros contains proportion of zeros in the outcome under each 
# treatment sequence from time 1 until tot.time
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"), header = TRUE)

# -----------------------------------------------------------------------------
# Begin tasks
# -----------------------------------------------------------------------------

source(file.path(path.code, "sim-po-dat.R"))

