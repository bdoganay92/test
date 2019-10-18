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
input.tot.time <- 6  # Total no. of time points
input.rand.time <- 2  # Time when second randomization occurred (time is 1-indexed)
input.cutoff <- 0  # Cutoff in the definition of response status
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

input.marg.params <- CalcTrueMarginalParams(means = input.means, 
                                            prop.zeros = input.prop.zeros, 
                                            cutoff = input.cutoff, 
                                            tot.time = input.tot.time, 
                                            rand.time = input.rand.time)

# Reshape true.beta from square data frame to column matrix
true.beta <- MeltBeta(df.rectangle = input.marg.params$true.beta, 
                      tot.time = input.tot.time,
                      rand.time = input.rand.time)

# Quantities involving a linear combination of marginal means at each time
# point for every given DTR
blank.marginal.df <- data.frame(DTR = c("plusplus","plusminus","minusplus","minusminus"),
                                matrix(rep(NA, 4*input.tot.time), nrow=4, 
                                       dimnames = list(c(NULL),
                                                       c(paste("time.",1:input.tot.time,sep=""))
                                       )
                                )
)

# -----------------------------------------------------------------------------
# Set up contrast matrix and calculate true values of contrasts
# -----------------------------------------------------------------------------

input.C <- as.list(1:input.tot.time)
names(input.C) <- paste("MargMean.",1:input.tot.time,sep="")

input.C <- lapply(input.C, function(x, 
                                    tot.time = input.tot.time, 
                                    init.df = blank.marginal.df){
  time.now <- x
  x <- init.df
  
  if(time.now == 1){
    x[,paste("time.",2:tot.time,sep="")] <- 0
  }else if(time.now > 1 & time.now < tot.time){
    x[,paste("time.",2:(time.now-1),sep="")] <- 0
    x[,paste("time.",(time.now+1):tot.time,sep="")] <- 0
  }else{  # if time.now==tot.time
    x[,paste("time.",2:(tot.time-1),sep="")] <- 0
  }
  
  x[,"time.1"] <- 1
  x[,paste("time.",time.now,sep="")] <- 1
  
  return(x)
}
)

melted.input.C <- lapply(input.C, MeltC, 
                         tot.time = input.tot.time,  
                         rand.time = input.rand.time)

# -----------------------------------------------------------------------------
# Calculate true values of contrasts
# -----------------------------------------------------------------------------

true.margmeans <- lapply(melted.input.C, function(x, use.beta = true.beta){
  return(exp(x %*% use.beta))
})
true.margmeans <- bind_cols(true.margmeans)

# -----------------------------------------------------------------------------
# Begin tasks
# -----------------------------------------------------------------------------

ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("nsim", "input.N",
                    "input.tot.time","input.rand.time",
                    "input.cutoff","input.rho",
                    "input.means","input.prop.zeros",
                    "path.code","path.input_data",
                    "melted.input.C"))
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
                               rho=input.rho,
                               input.means=input.means,
                               input.prop.zeros=input.prop.zeros)

list.df.observed <- parLapply(cl = cl, 
                              X = list.df.potential, 
                              fun = GenerateObservedYit)

list.df.wr <- parLapply(cl = cl, 
                        X = list.df.observed, 
                        fun = WeightAndReplicate, 
                        tot.time = input.tot.time)

list.df.est.beta <- parLapply(cl = cl, 
                              X = list.df.wr, 
                              fun = AnalyzeData, 
                              tot.time = input.tot.time, 
                              rand.time = input.rand.time)

list.df.est.margmean <- parLapply(cl=cl,
                                  X = list.df.est.beta,
                                  fun = function(x, list.C = melted.input.C){
                                    if(x$converged==0){
                                      out <- NULL
                                    }else{
                                      est.beta <- as.matrix(x$est.beta)
                                      out <- lapply(list.C, 
                                                    function(mat, b = est.beta){
                                                      return(exp(mat%*%b))
                                                    })
                                    }
                                    return(out)
                                  })

list.df.est.margmean <- parLapply(cl=cl,
                                  X = list.df.est.margmean,
                                  fun = function(x){
                                    if(is.null(x)){
                                      out <- NULL
                                    }else{
                                      out <- bind_cols(x)
                                    }
                                    return(out)
                                  })


stopCluster(cl)

# -----------------------------------------------------------------------------
# Evaluate estimates: betas
# -----------------------------------------------------------------------------

list.out <- lapply(list.df.est.beta, 
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
df.out <- df.out %>% filter(converged==1) %>% 
  group_by(coefnames) %>% 
  summarise(est.bias = mean(bias))

bias.betas <- df.out
remove(df.out, list.out)

# -----------------------------------------------------------------------------
# Evaluate estimates: marginal means
# -----------------------------------------------------------------------------

list.out <- lapply(list.df.est.margmean,
                   function(x, list.true.vals = list(true.margmeans = true.margmeans)){
                     
                     outdf <- list.true.vals$true.margmeans - x
                     outdf <- as.matrix(outdf)
                     return(outdf)
                   }
                   )

df.out <- matrix(rep(0, 4*input.tot.time), nrow=4)
n.converged <- 0
for(i in 1:length(list.out)){
  
  this.mat <- list.out[[i]]
  if(nrow(this.mat)>0){
   df.out <- df.out + this.mat 
   n.converged <- n.converged + 1
  }else{
    next
  }
}

df.out <- df.out/n.converged

bias.margmean <- df.out
remove(df.out, list.out)

# -----------------------------------------------------------------------------
# Print out calculated quantities
# -----------------------------------------------------------------------------

df.out.bias <- data.frame(max.bias.betas = max(bias.betas$est.bias),
                          max.bias.margmean = max(bias.margmean)
                          )
df.out.bias  



