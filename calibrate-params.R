library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(parallel)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")

source(file.path(path.code, "input-utils.R"))
source(file.path(path.code, "datagen-utils.R"))

# -----------------------------------------------------------------------------
# Read and prepare input parameters
# -----------------------------------------------------------------------------

nsim <- seq(1,1000,1)  # Total no. of monte carlo samples
input.N <- 500  # Total no. of individuals
input.tot.time <- 11  # Total no. of time points
input.rand.time <- 7  # Time when second randomization occurred (time is 1-indexed)
input.cutoff <- 1  # Cutoff in the definition of response status
input.rho <- seq(0,1,0.1)

# input.means contains mean outcome under each treatment sequence
# from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), header = TRUE)

# input.prop.zeros contains proportion of zeros in the outcome under each 
# treatment sequence from time 1 until tot.time
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"), header = TRUE)

# Combine all inputs into a grid
gridx <- expand.grid(nsim=nsim, 
                     input.N=input.N, 
                     input.tot.time=input.tot.time,
                     input.rand.time=input.rand.time,
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
# Begin tasks
# -----------------------------------------------------------------------------

ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("list.gridx","path.code","path.input_data"))
clusterEvalQ(cl,
             {
               library(dplyr)
               library(assertthat)
               library(rootSolve)
               library(mvtnorm)
               library(geeM)
               source(file.path(path.code, "input-utils.R"))
               source(file.path(path.code, "datagen-utils.R"))
               source(file.path(path.code, "wr-utils.R"))
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
                                                            input.means=this.gridx$input.means)
                                 return(df)
                               })

list.empirical.corr <- parLapply(cl = cl, 
                                 X = list.df.potential, 
                                 fun = DTRCorrelationPO)

list.empirical.corr <- parLapply(cl = cl,
                                 X = list.empirical.corr, 
                                 fun = function(x){
                                   df <- data.frame(sim = x$sim,
                                                    rho = x$rho,
                                                    DTR = c("plusplus", 
                                                            "plusminus", 
                                                            "minusplus", 
                                                            "minusminus"),
                                                    rho.star.max = x$rho.star.max,
                                                    rho.star.min = x$rho.star.min,
                                                    rho.star.ave = x$rho.star.ave)
                                   return(df)
                                 })

stopCluster(cl)

# -----------------------------------------------------------------------------
# Evaluate estimates
# -----------------------------------------------------------------------------

empirical.corr <- bind_rows(list.empirical.corr)
corr.hat <- empirical.corr %>% group_by(DTR, rho) %>%
  summarise(rho.star.max = mean(rho.star.max),
            rho.star.min = mean(rho.star.min),
            rho.star.ave = mean(rho.star.ave)) %>%
  arrange(rho, desc(DTR))


