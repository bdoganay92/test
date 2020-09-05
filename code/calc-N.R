library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)
library(gridExtra)
library(beepr)

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
input.power <- 0.80
input.alpha <- 0.05

this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

input.rho <- 0.60

# Means and proportion of zeros
input.means <- read.csv(file.path(path.input_data, "input_means_d_0.csv"))  # input file: change to appropriate file
input.prop.zeros  <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"))  # input file: change to appropriate file

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
# Other inputs required in simulation (not specified by user)
###############################################################################
input.M <- 6000
input.N <- 700  # This is N_old
input.n4 <- NA_real_
use.working.corr <- "ar1"

###############################################################################
# Calculate covmat
###############################################################################
begin.time <- Sys.time()

source(file.path(path.code,"calc-covmat.R"))

end.time <- Sys.time()

###########################################################################
# Clean up
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

###########################################################################
# Calculate standard error of differences of contrasts of interest
###########################################################################
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
list.stderr.eos.means <- lapply(list.df.est.beta, EstimateStdErrDiffs, D = D.eos.means, list.C = list.C)
list.stderr.AUC <- lapply(list.df.est.beta, EstimateStdErrDiffs, D = D.AUC, list.C = list.C)

###############################################################################
# Determine which pairwise comparison to pick out
###############################################################################
string.this.pair <- case_when(
  this.pair==1 ~ "plusplus.plusminus",
  this.pair==2 ~ "plusplus.minusplus",
  this.pair==3 ~ "plusplus.minusminus",
  this.pair==4 ~ "plusminus.minusplus",
  this.pair==5 ~ "plusminus.minusminus",
  this.pair==6 ~ "minusplus.minusminus",
  TRUE ~ NA_character_
)

assert_that(!is.na(string.this.pair), msg = "invalid option entered")

###############################################################################
# Only keep those list elements corresponding to the pairwise comparison
# of interest
###############################################################################
list.stderr.eos.means <- lapply(list.stderr.eos.means, function(x, which.pair = string.this.pair){return(x[which.pair])})
list.stderr.eos.means <- lapply(list.stderr.eos.means, ReshapeList)
df.stderr.eos.means <- bind_rows(list.stderr.eos.means)

list.stderr.AUC <- lapply(list.stderr.AUC, function(x, which.pair = string.this.pair){return(x[which.pair])})
list.stderr.AUC <- lapply(list.stderr.AUC, ReshapeList)
df.stderr.AUC <- bind_rows(list.stderr.AUC)

###############################################################################
# Now calculate the mean across all Monte Caelo samples
###############################################################################
mean.stderr.eos.means <- sum(df.stderr.eos.means$estimates)/sum.converged.beta
mean.sandwich.eos.means <- (input.N) * ((mean.stderr.eos.means)^2)

mean.stderr.AUC <- sum(df.stderr.AUC$estimates)/sum.converged.beta
mean.sandwich.AUC <- (input.N) * ((mean.stderr.AUC)^2)

###########################################################################
# Calculate DELTA_Q
###########################################################################
source(file.path(path.code, "calc-truth-beta.R"))
source(file.path(path.code, "calc-truth-contrasts.R"))

###############################################################################
# Determine which pairwise comparison to pick out from the outputs of
# calc-truth-conrasts.R
###############################################################################
diff.eos.means <- case_when(
  this.pair==1 ~ diff.eos.means.plusplus.plusminus,
  this.pair==2 ~ diff.eos.means.plusplus.minusplus,
  this.pair==3 ~ diff.eos.means.plusplus.minusminus,
  this.pair==4 ~ diff.eos.means.plusminus.minusplus,
  this.pair==5 ~ diff.eos.means.plusminus.minusminus,
  this.pair==6 ~ diff.eos.means.minusplus.minusminus,
  TRUE ~ NA_real_
)

assert_that(!is.na(diff.eos.means), msg = "invalid option entered")

diff.AUC <- case_when(
  this.pair==1 ~ diff.AUC.plusplus.plusminus,
  this.pair==2 ~ diff.AUC.plusplus.minusplus,
  this.pair==3 ~ diff.AUC.plusplus.minusminus,
  this.pair==4 ~ diff.AUC.plusminus.minusplus,
  this.pair==5 ~ diff.AUC.plusminus.minusminus,
  this.pair==6 ~ diff.AUC.minusplus.minusminus,
  TRUE ~ NA_real_
)

assert_that(!is.na(diff.AUC), msg = "invalid option entered")

###########################################################################
# Calculate N.required
###########################################################################
z.eta <- qnorm(1-input.power)
z.alpha <- qnorm(input.alpha/2)

const.eos.means <- ((z.eta + z.alpha)/diff.eos.means)^2
const.AUC <- ((z.eta + z.alpha)/diff.AUC)^2

N.required.eos.means <- const.eos.means * mean.sandwich.eos.means
N.required.AUC <- const.AUC * mean.sandwich.AUC

###########################################################################
# Display N.required
###########################################################################
print(N.required.eos.means)
print(N.required.AUC)

# Audio notification
beep("mario")

# Save RData
save(N.required.eos.means, N.required.AUC, file = file.path(path.output_data, paste("Nrequired","_rho_",input.rho, ".RData", sep="")))


