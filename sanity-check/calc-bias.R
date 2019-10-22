library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")

# -----------------------------------------------------------------------------
# Read and prepare input parameters
# -----------------------------------------------------------------------------

nsim <- 7  # Total no. of monte carlo samples
input.N <- 1000  # Total no. of individuals
input.tot.time <- 6  # Total no. of time points
input.rand.time <- 2  # Time when second randomization occurred (time is 1-indexed)
input.cutoff <- 0  # Cutoff in the definition of response status
input.rho <- 0.7  # Dependence parameter

# input.means contains mean of time-specific outcomes under each 
# treatment sequence from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), header = TRUE)

# -----------------------------------------------------------------------------
# Define functions
# -----------------------------------------------------------------------------

e <- function(i,n){
  evec <- rep(0,n)
  evec[i] <- 1
  evec <- as.matrix(evec)
  return(evec)
}

# -----------------------------------------------------------------------------
# Calculate true values of parameters of marginal mean model
# -----------------------------------------------------------------------------

# Using input.means we calculate the value of the parameters gamma_j
# in our model for log(E{Y_t^{(a_1,r,a_2)}})
gamma.vec <- rep(NA,6*input.tot.time-4*input.rand.time-1)
gamma.vec <- as.matrix(gamma.vec)
gamma.vec[1] <- log(input.means[1,"time.1"])

