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

# Difference in AUC
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
# Read in input.mean data frame
###############################################################################
input.means <- read.csv(file.path(path.input_data, "input_means.csv"))
CheckInputData(input.means, rand.time = input.rand.time, tot.time = input.tot.time)

###############################################################################
# Read in input.prop.zeros data frame
###############################################################################
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"))
CheckInputData(input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

###############################################################################
# Calculate estimated covariance of regression model parameters:
# difference in eos means or change score
# =============================================================================
# N is fixed while standardized effect size is varied
###############################################################################
input.N <- 1000
input.n4 <- NA_real_
collect.estimates <- list()

for(idx.i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[idx.i]]
  
  source(file.path(path.code,"calc-covmat.R"))
  datagen.params <- list.df.est.beta[[length(list.df.est.beta)]]$datagen.params
  
  ###########################################################################
  # Get mean of covmat
  ###########################################################################
  
  # First, determine how many simulation runs resulted in convergence
  list.converged.beta <- lapply(list.df.est.beta, function(x){
    converged <- (x$estimates$converged)
    return(converged)
  })
  sum.converged.beta <- reduce(.x = list.converged.beta, .f = `+`)
  
  # Discard list elements corresponding to simulation runs
  # that did not converge
  list.df.est.beta <- discard(list.df.est.beta, function(x){
    return(x$estimates$converged==0)
  })
  
  # Calculate standard error of differences
  list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
  list.stderr.eos.means <- lapply(list.df.est.beta, EstimateStdErrDiffs, D = D.eos.means, list.C = list.C)
  list.stderr.change.score <- lapply(list.df.est.beta, EstimateStdErrDiffs, D = D.change.score, list.C = list.C)
  list.stderr.AUC <- lapply(list.df.est.beta, EstimateStdErrDiffs, D = D.AUC, list.C = list.C)
  
  # Only keep those list elements corresponding to the comparison
  # between DTRs plusplus vs. minusplus
  list.stderr.eos.means <- lapply(list.stderr.eos.means, function(x){
    return(list(x$plusplus.minusplus))
  })
  list.stderr.eos.means <- lapply(list.stderr.eos.means, ReshapeList)
  
  list.stderr.change.score <- lapply(list.stderr.change.score, function(x){
    return(list(x$plusplus.minusplus))
  })
  list.stderr.change.score <- lapply(list.stderr.change.score, ReshapeList)
  
  list.stderr.AUC <- lapply(list.stderr.AUC, function(x){
    return(list(x$plusplus.minusplus))
  })
  list.stderr.AUC <- lapply(list.stderr.AUC, ReshapeList)
  
  # Change from list to data frame and then calculate mean across all Monte Carlo samples
  df.stderr.eos.means <- bind_rows(list.stderr.eos.means)
  df.stderr.change.score <- bind_rows(list.stderr.change.score)
  df.stderr.AUC <- bind_rows(list.stderr.AUC)
  mean.stderr.eos.means <- sum(df.stderr.eos.means$estimates)/sum.converged.beta
  mean.stderr.change.score <- sum(df.stderr.change.score$estimates)/sum.converged.beta
  mean.sandwich.eos.means <- (datagen.params$N) * ((mean.stderr.eos.means)^2)
  mean.sandwich.change.score <- (datagen.params$N) * ((mean.stderr.change.score)^2)
  mean.stderr.AUC <- sum(df.stderr.AUC$estimates)/sum.converged.beta
  mean.sandwich.AUC <- (datagen.params$N) * ((mean.stderr.AUC)^2)
  
  out.df <- cbind(datagen.params,
                  mean.stderr.eos.means,
                  mean.stderr.change.score,
                  mean.stderr.AUC,
                  mean.sandwich.eos.means,
                  mean.sandwich.change.score,
                  mean.sandwich.AUC)
  
  out.df <- as.data.frame(out.df)
  
  collect.estimates <- append(collect.estimates, list(out.df))
  remove(list.df.est.beta, list.converged.beta, list.stderr.eos.means, list.stderr.change.score, list.stderr.AUC)
}

end.time <- Sys.time()
###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, use.working.corr, "estimated-covmat.RData"))


