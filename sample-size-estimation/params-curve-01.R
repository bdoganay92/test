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
list.input.rho <- as.list(seq(0,0.99,by=0.1))
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
# Create list.input.means data frames where difference in end-of-study means
# or change score between DTRs ++ and -+ is gradually increased. 
# This will be used to calculate power when N is fixed while standardized 
# effect size is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- seq(0.5, 5.5, 0.5)

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

###############################################################################
# Calculate standardized effect size and simulated within-person correlation 
# by DTR
###############################################################################
input.N <- 10000
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(list.input.means)){
    input.means <- list.input.means[[j]]
    
    for(k in 1:5000){
      # Simulate potential outcomes for the 10000 individuals for these inputs
      df.list <- GeneratePotentialYit(sim=k, 
                                      N=input.N, 
                                      tot.time=input.tot.time, 
                                      rand.time=input.rand.time, 
                                      cutoff=input.cutoff, 
                                      rho=input.rho, 
                                      input.prop.zeros=input.prop.zeros, 
                                      input.means=input.means)
      
      # Calculate correlation
      this.corr <- DTRCorrelationPO(df.list = df.list)
      this.corr <- ReshapeList(x = list(this.corr), idx=1)
      this.corr$idx.input.means <- j
      this.corr$sim <- k
      
      # Append to list
      collect.correlation <- append(collect.correlation, list(this.corr))
    }
    
    remove(df.list)
  }
}

end.time <- Sys.time()
###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, "params-curve-01.RData"))

