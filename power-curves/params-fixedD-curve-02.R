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
# Create auc.shortlist.input.means data frames for a fixed choice of 
# difference in AUC between DTRs ++ and -+
# This will be used to calculate power when standardized effect size is fixed
# and N is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- 1

auc.shortlist.input.means <- list()
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
  
  auc.shortlist.input.means <- append(auc.shortlist.input.means, list(tmpdat))
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
collect.delta.AUC <- list()
collect.correlation <- list()


for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(auc.shortlist.input.means)){
    input.means <- auc.shortlist.input.means[[j]]
    
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
    delta.AUC <- CalcDeltaj(list.df = df.list, L = L.AUC)
    delta.AUC <- ReshapeList(x = delta.AUC, idx=this.pair)
    delta.AUC$idx.input.means <- j
    
    # Calculate correlation
    this.corr <- DTRCorrelationPO(df.list = df.list)
    this.corr <- ReshapeList(x = list(this.corr), idx=1)
    
    # Append to list
    collect.delta.AUC <- append(collect.delta.AUC, list(delta.AUC))
    collect.correlation <- append(collect.correlation, list(this.corr))
    
    remove(df.list)
  }
}

end.time <- Sys.time()

###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, "params-fixedD-curve-02.RData"))


