library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)
library(gridExtra)

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
this.pair <- 2
input.power <- 0.80
input.alpha <- 0.05
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0
input.means <- read.csv(file.path(path.input_data, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"))

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
input.M <- 30
input.N <- 500
input.n4 <- NA_real_
list.input.rho <- as.list(seq(0,1,by=0.30))

###############################################################################
# Calculate correlation
###############################################################################
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  gridx <- expand.grid(nsim=1:input.M, 
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

collect.correlation <- do.call(rbind, collect.correlation)

###############################################################################
# Calculate simulated correlation by taking the average across all
# Monte Carlo samples
###############################################################################
simulated.correlation <- collect.correlation %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(simulated.corr=mean(estimates))

###############################################################################
# Display simulated correlation
###############################################################################
print(simulated.correlation)





