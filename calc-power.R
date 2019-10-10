library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")

source(file.path(path.code, "input-utils.R"))
source(file.path(path.code, "datagen-utils.R"))
source(file.path(path.code, "wr-utils.R"))
source(file.path(path.code, "analysis-utils.R"))

# -----------------------------------------------------------------------------
# Read and prepare input parameters
# -----------------------------------------------------------------------------

nsim <- 1000  # Total no. of monte carlo samples
input.N <- 500  # Total no. of individuals
input.tot.time <- 12  # Total no. of time points
input.rand.time <- 4  # Time when second randomization occurred (time is 1-indexed)
input.cutoff <- 1  # Cutoff in the definition of response status
input.rho <- 0.7  # Dependence parameter

# input.means contains mean outcome under each treatment sequence
# from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), header = TRUE)

# input.prop.zeros contains proportion of zeros in the outcome under each 
# treatment sequence from time 1 until tot.time
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"), header = TRUE)

# -----------------------------------------------------------------------------
# Calculate marginal means implied by input parameters
# -----------------------------------------------------------------------------

input.marg.params <- CalcMarginalParams(means = input.means, 
                                        prop.zeros = input.prop.zeros, 
                                        cutoff = input.cutoff, 
                                        tot.time = input.tot.time, 
                                        rand.time = input.rand.time)

idx.time.1 <- which(colnames(true.beta)=="time.1")
idx.tot.time <- input.tot.time + (idx.time.1-1)
idx.rand.time <- input.rand.time + (idx.time.1-1)

true.beta <- input.marg.params$true.beta
true.beta <- c(true.beta[true.beta$DTR=="plusplus", idx.time.1],
               true.beta[true.beta$DTR=="plusplus", (idx.time.1+1):idx.rand.time],
               true.beta[true.beta$DTR=="minusplus", (idx.time.1+1):idx.rand.time],
               true.beta[true.beta$DTR=="plusplus", (idx.rand.time+1):idx.tot.time],
               true.beta[true.beta$DTR=="plusminus", (idx.rand.time+1):idx.tot.time],
               true.beta[true.beta$DTR=="minusplus", (idx.rand.time+1):idx.tot.time],
               true.beta[true.beta$DTR=="minusminus", (idx.rand.time+1):idx.tot.time])

true.beta <- as.matrix(true.beta)

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
                    "nsim"))
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

list.df.potential <- parLapply(cl = cl, 
                               X = as.list(1:nsim), 
                               fun = GeneratePotentialYit,
                               N=input.N, 
                               tot.time=input.tot.time,
                               rand.time=input.rand.time,
                               cutoff=input.cutoff,
                               rho=input.rho)

list.df.observed <- parLapply(cl = cl, 
                              X = list.df.potential, 
                              fun = GenerateObservedYit)

list.df.wr <- parLapply(cl = cl, 
                        X = list.df.observed, 
                        fun = WeightAndReplicate, 
                        tot.time = input.tot.time)

list.df.est <- parLapply(cl = cl, 
                         X = list.df.wr, 
                         fun = AnalyzeData, 
                         tot.time = input.tot.time, 
                         rand.time = input.rand.time)

stopCluster(cl)

# -----------------------------------------------------------------------------
# Evaluate estimates
# -----------------------------------------------------------------------------

list.out <- lapply(list.df.est, 
                   function(x, list.true.vals = list(true.beta = true.beta)){
                     
                     outdf <- data.frame(converged = x$converged,
                                         coefnames = x$coefnames,
                                         est.beta = x$est.beta,
                                         true.beta = as.numeric(list.true.vals$true.beta)
                                         )
                     
                     outdf$bias <- outdf$true.beta - outdf$est.beta
                     
                     return(outdf)
                   }
)

df.out <- bind_rows(list.out)
df.out.summary <- df.out %>% filter(converged==1) %>% 
  group_by(coefnames) %>% summarise(est.bias = mean(bias))



