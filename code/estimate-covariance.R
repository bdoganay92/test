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
list.input.rho <- list(0.5)#list(0.1, 0.5, 0.9)
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
# Create list.input.means data frames where difference in end-of-study means
# or change score between DTRs ++ and -+ is gradually increased. 
# This will be used to calculate power when N is fixed while standardized 
# effect size is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)

dat$time.1 <- c(1.89, 1.89, 1.89, 1.89, 1.89, 1.89)
dat$time.2 <- c(1.59, 1.59, 1.59, 2.30, 2.30, 2.30)
dat$time.3 <- c(2.56, 5.93, 3.25, 0.89, 4.86, 3.63)
dat$time.4 <- c(2.00, 4.71, 3.12, 0.68, 7.71, 5.37)
dat$time.5 <- c(2.24, 3.00, 3.44, 0.75, 7.36, 4.53)
dat$time.6 <- c(1.45, 2.64, 3.44, 0.51, 5.71, 3.58)

increments <- seq(0.5, 2, 0.5)

list.input.means <- list()
for(i in 1:length(increments)){
  k <- increments[i]
  tmpdat <- dat
  
  tmpdat[tmpdat$seq=="minus.r","time.3"] <- tmpdat[tmpdat$seq=="minus.r","time.3"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.3"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.3"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.4"] <- tmpdat[tmpdat$seq=="minus.r","time.4"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.4"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.4"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.5"] <- tmpdat[tmpdat$seq=="minus.r","time.5"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.5"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.5"] + k
  
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

dat$time.1 <- c(0.64, 0.64, 0.64, 0.64, 0.64, 0.64)
dat$time.2 <- c(0.65, 0.65, 0.65, 0.61, 0.61, 0.61)
dat$time.3 <- c(0.71, 0.36, 0.56, 0.68, 0.43, 0.53)
dat$time.4 <- c(0.74, 0.50, 0.62, 0.89, 0.57, 0.63)
dat$time.5 <- c(0.71, 0.50, 0.62, 0.83, 0.64, 0.63)
dat$time.6 <- c(0.70, 0.50, 0.44, 0.83, 0.64, 0.53)

input.prop.zeros <- dat

###############################################################################
# Calculate estimated covariance of regression model parameters:
# difference in eos means or change score
# =============================================================================
# N is fixed while standardized effect size is varied
###############################################################################
input.N <- 5000
input.n4 <- NA_real_
collect.estimates <- list()

for(idx.i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[idx.i]]
  
  for(idx.j in 1:length(list.input.means)){
    input.means <- list.input.means[[idx.j]]
    
    source(file.path(path.code,"calc-covmat.R"))
    datagen.params <- list.df.est.beta[[length(list.df.est.beta)]]$datagen.params
    datagen.params$input.means <- idx.j
    
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
    list.stderr.AUC <- lapply(list.df.est.beta, EstimateStdErrDiffs, D = D.AUC, list.C = list.C)
    
    # Only keep those list elements corresponding to the comparison
    # between DTRs plusplus vs. minusplus
    list.stderr.eos.means <- lapply(list.stderr.eos.means, function(x){
      return(list(x$plusplus.minusplus))
    })
    list.stderr.eos.means <- lapply(list.stderr.eos.means, ReshapeList)
    
    list.stderr.AUC <- lapply(list.stderr.AUC, function(x){
      return(list(x$plusplus.minusplus))
    })
    list.stderr.AUC <- lapply(list.stderr.AUC, ReshapeList)
    
    # Change from list to data frame and then calculate mean across all Monte Carlo samples
    df.stderr.eos.means <- bind_rows(list.stderr.eos.means)
    df.stderr.AUC <- bind_rows(list.stderr.AUC)
    mean.stderr.eos.means <- sum(df.stderr.eos.means$estimates)/sum.converged.beta
    mean.stderr.AUC <- sum(df.stderr.AUC$estimates)/sum.converged.beta
    mean.sandwich.eos.means <- (datagen.params$N) * ((mean.stderr.eos.means)^2)
    mean.sandwich.AUC <- (datagen.params$N) * ((mean.stderr.AUC)^2)
    
    out.df <- cbind(datagen.params,
                    mean.stderr.eos.means,
                    mean.stderr.AUC,
                    mean.sandwich.eos.means,
                    mean.sandwich.AUC)
    
    out.df <- as.data.frame(out.df)
    
    collect.estimates <- append(collect.estimates, list(out.df))
    remove(list.df.est.beta, list.converged.beta, list.stderr.eos.means, list.stderr.AUC)
  }
}

end.time <- Sys.time()
