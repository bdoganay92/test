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
list.input.rho <- as.list(seq(0,0.99,by=0.05))
input.cutoff <- 0
this.pair <- 2 # Compare DTR plus.plus vs. DTR minus.plus

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
# Calculate standardized effect size and simulated within-person correlation 
# by DTR
###############################################################################
input.N <- 1000
input.n4 <- NA_real_
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  gridx <- expand.grid(nsim=1:5000, 
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
                      "list.gridx"))
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
                           return(this.corr)
                         })
  
  stopCluster(cl)
  
  remove(list.df.potential, list.gridx)
  collect.correlation <- append(collect.correlation, list.corr)
}

end.time <- Sys.time()
###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, "params-curve.RData"))

