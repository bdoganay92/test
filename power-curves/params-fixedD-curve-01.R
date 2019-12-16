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
# Calculate standardized effect size and simulated within-person correlation 
# by DTR
###############################################################################
input.N <- 200000
collect.delta.eos.means <- list()
collect.delta.change.score <- list()
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(shortlist.input.means)){
    input.means <- shortlist.input.means[[j]]
    
    # Simulate potential outcomes for the 5000 individuals for these inputs
    df.list <- GeneratePotentialYit(sim=1, 
                                    N=input.N, 
                                    tot.time=input.tot.time, 
                                    rand.time=input.rand.time, 
                                    cutoff=input.cutoff, 
                                    rho=input.rho, 
                                    input.prop.zeros=input.prop.zeros, 
                                    input.means=input.means)
    
    # Calculate delta
    delta.eos.means <- CalcDeltaj(list.df = df.list, L = L.eos.means)
    delta.eos.means <- ReshapeList(x = delta.eos.means, idx=this.pair)
    delta.eos.means$idx.input.means <- j
    
    delta.change.score <- CalcDeltaj(list.df = df.list, L = L.change.score)
    delta.change.score <- ReshapeList(x = delta.change.score, idx=this.pair)
    delta.change.score$idx.input.means <- j
    
    # Calculate correlation
    this.corr <- DTRCorrelationPO(df.list = df.list)
    this.corr <- ReshapeList(x = list(this.corr), idx=1)
    
    # Append to list
    collect.delta.eos.means <- append(collect.delta.eos.means, list(delta.eos.means))
    collect.delta.change.score <- append(collect.delta.change.score, list(delta.change.score))
    collect.correlation <- append(collect.correlation, list(this.corr))
    
    remove(df.list)
  }
}

end.time <- Sys.time()
###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, "params-fixedD-curve-01.RData"))

