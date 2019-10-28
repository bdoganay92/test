library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

# -----------------------------------------------------------------------------
# Read and prepare input data
# -----------------------------------------------------------------------------
input.tot.time <- 6  # Total no. of measurement occasions
input.rand.time <- 2  # Time when second randomization occurred (time is 1-indexed)

# input.means contains mean of time-specific outcomes under each 
# treatment sequence from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "6-months/input_means.csv"), header = TRUE)
input.prop.zeros <- read.csv(file.path(path.input_data, "6-months/input_prop_zeros.csv"), header = TRUE)
# Check whether input data are in the correct format
CheckInputData(input.df=input.means, rand.time=input.rand.time, tot.time=input.tot.time)
CheckInputData(input.df=input.prop.zeros, rand.time=input.rand.time, tot.time=input.tot.time)

# -----------------------------------------------------------------------------
# Specify contrasts of interest
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Specify data generating parameters
# -----------------------------------------------------------------------------
# Vary inputs here
input.N <- c(300)  # Total no. of individuals
input.rho <- c(0.7)  # Dependence parameter

# Held fixed at all times
idx.nsim <- 1:1000  # Total no. of monte carlo samples
input.cutoff <- 0  # Cutoff in the definition of response status

# Combine all inputs into a grid
gridx <- expand.grid(nsim=idx.nsim, 
                     input.N=input.N,
                     input.rand.time=input.rand.time, 
                     input.tot.time=input.tot.time,
                     input.cutoff=input.cutoff,
                     input.rho=input.rho)

list.gridx <- apply(gridx, 1, as.list)
list.gridx <- lapply(list.gridx, function(this.list, 
                                          means=input.means,
                                          prop.zeros=input.prop.zeros){
  this.list$input.means <- input.means
  this.list$input.prop.zeros <- input.prop.zeros
  return(this.list)
})

# -----------------------------------------------------------------------------
# Calculate delta
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "list.gridx",
                    "L.AUC",
                    "L.eos.means"))
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

list.delta.eos.means <- parLapply(cl=cl, list.df.potential, CalcDeltaj, L=L.eos.means)
list.delta.AUC <- parLapply(cl=cl, list.df.potential, CalcDeltaj, L=L.AUC)
stopCluster(cl)

list.delta.eos.means.plusplus.plusminus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.plusminus"])})
list.delta.eos.means.plusplus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.minusplus"])})
list.delta.eos.means.plusplus.minusminus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.minusminus"])})
list.delta.eos.means.minusplus.plusminus <- lapply(list.delta.eos.means, function(x){return(x["minusplus.plusminus"])})
list.delta.eos.means.minusplus.minusminus <- lapply(list.delta.eos.means, function(x){return(x["minusplus.minusminus"])})
list.delta.eos.means.minusminus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["minusminus.minusplus"])})

list.delta.AUC.plusplus.plusminus <- lapply(list.delta.AUC, function(x){return(x["plusplus.plusminus"])})
list.delta.AUC.plusplus.minusplus <- lapply(list.delta.AUC, function(x){return(x["plusplus.minusplus"])})
list.delta.AUC.plusplus.minusminus <- lapply(list.delta.AUC, function(x){return(x["plusplus.minusminus"])})
list.delta.AUC.minusplus.plusminus <- lapply(list.delta.AUC, function(x){return(x["minusplus.plusminus"])})
list.delta.AUC.minusplus.minusminus <- lapply(list.delta.AUC, function(x){return(x["minusplus.minusminus"])})
list.delta.AUC.minusminus.minusplus <- lapply(list.delta.AUC, function(x){return(x["minusminus.minusplus"])})

delta.eos.means.plusplus.plusminus <- lapply(list.delta.eos.means.plusplus.plusminus, 
                                             function(x){
                                               out <- data.frame(x[[1]]$datagen.params,
                                                                 x[[1]]$estimates)
                                               return(out)
                                             }
                                             )
delta.eos.means.plusplus.plusminus <- bind_rows(delta.eos.means.plusplus.plusminus)

