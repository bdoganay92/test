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

input.sim <- seq(1,1000,1)  # Total no. of monte carlo samples
input.N <- 500  # Total no. of individuals
input.tot.time <- 12  # Total no. of time points
input.rand.time <- 4  # Time when second randomization occurred (time is 1-indexed)
input.cutoff <- 1  # Cutoff in the definition of response status
input.rho <- seq(0,1,0.1)

# input.means contains mean outcome under each treatment sequence
# from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), header = TRUE)

# input.prop.zeros contains proportion of zeros in the outcome under each 
# treatment sequence from time 1 until tot.time
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"), header = TRUE)

# -----------------------------------------------------------------------------
# Begin tasks
# -----------------------------------------------------------------------------

ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("input.N",
                    "input.tot.time","input.rand.time",
                    "input.cutoff","input.rho",
                    "input.means","input.prop.zeros",
                    "path.code","path.input_data",
                    "input.sim"))
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

list.df.potential <- clusterMap(cl = cl, 
                                fun = GeneratePotentialYit,
                                sim = input.sim,
                                N = input.N, 
                                tot.time = input.tot.time,
                                rand.time = input.rand.time,
                                cutoff = input.cutoff,
                                rho = input.rho)

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
            rho.star.ave = mean(rho.star.ave))


