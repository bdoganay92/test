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
list.input.rho <- as.list(seq(0.1,0.99,by=0.20))
input.cutoff <- 0
names.seq <- matrix(c("plus.r", "plus.nr.plus", "plus.nr.minus", 
                      "minus.r", "minus.nr.plus", "minus.nr.minus"), 
                    ncol=1, dimnames = list(NULL, "seq"))
this.pair <- 2 # Compare DTR plus.plus vs. DTR minus.plus

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
# Calculate standardized effect size and simulated within-person correlation 
# by DTR
###############################################################################
input.N <- 1000
input.n4 <- NA_real_
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(list.input.means)){
    input.means <- list.input.means[[j]]
    
    gridx <- expand.grid(nsim=1:1000, 
                         input.N=input.N,
                         input.rand.time=input.rand.time, 
                         input.tot.time=input.tot.time,
                         input.cutoff=input.cutoff,
                         input.rho=input.rho,
                         input.n4=input.n4)
    
    list.gridx <- apply(gridx, 1, as.list)
    list.gridx <- lapply(list.gridx, function(this.list, 
                                              means=input.means,
                                              prop.zeros=input.prop.zeros){
      this.list$input.means <- input.means
      this.list$input.prop.zeros <- input.prop.zeros
      return(this.list)
    })
    
    ncore <- detectCores()
    cl <- makeCluster(ncore - 1)
    clusterSetRNGStream(cl, 102399)
    clusterExport(cl, c("path.code",
                        "path.input_data",
                        "path.output_data",
                        "list.gridx",
                        "j"))
    clusterEvalQ(cl,
                 {
                   library(dplyr)
                   library(assertthat)
                   library(rootSolve)
                   library(mvtnorm)
                   library(geeM)
                   source(file.path(path.code, "input-utils.R"))
                   source(file.path(path.code, "datagen-utils.R"))
                   source(file.path(path.code, "analysis-utils.R"))
                 })
    
    list.df.potential <- parLapply(cl=cl,
                                   X=list.gridx,
                                   fun=function(this.gridx){
                                     df <- GeneratePotentialYit(sim=this.gridx$nsim, 
                                                                N=this.gridx$input.N, 
                                                                tot.time=this.gridx$input.tot.time, 
                                                                rand.time=this.gridx$input.rand.time, 
                                                                cutoff=this.gridx$input.cutoff, 
                                                                rho=this.gridx$input.rho, 
                                                                input.prop.zeros=this.gridx$input.prop.zeros, 
                                                                input.means=this.gridx$input.means,
                                                                input.n4=this.gridx$input.n4)
                                     return(df)
                                   })
    
    list.corr <- parLapply(cl=cl,
                           X=list.df.potential,
                           fun=function(this.list){
                             this.corr <- DTRCorrelationPO(df.list = this.list)
                             this.corr <- ReshapeList(x = list(this.corr), idx=1)
                             this.corr$idx.input.means <- j
                             return(this.corr)
                           })
    
    stopCluster(cl)
    
    remove(list.df.potential, list.gridx)
    collect.correlation <- append(collect.correlation, list.corr)
  }
}

end.time <- Sys.time()
###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, "params-curve.RData"))

