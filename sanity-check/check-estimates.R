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
input.alpha <- 0.05  # Type-I error rate

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
input.N <- c(500)  # Total no. of individuals
input.rho <- c(0.7)  # Dependence parameter

# Held fixed at all times
idx.nsim <- 1:3  # Total no. of monte carlo samples
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
# Generate potential outcomes and observed outcomes
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
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
                                                            input.means=this.gridx$input.means)
                                 return(df)
                               })

list.df.observed <- parLapply(cl = cl, 
                              X = list.df.potential, 
                              fun = GenerateObservedYit)

stopCluster(cl)

# -----------------------------------------------------------------------------
# Estimate the value of the parameters beta_j in our model for 
# log(E{Y_t^{(a_1,a_2)}})
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "list.df.observed"))
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

list.df.wr <- parLapply(cl = cl, 
                        X = list.df.observed, 
                        fun = WeightAndReplicate, 
                        tot.time = input.tot.time)

list.df.est.beta <- parLapply(cl = cl, 
                              X = list.df.wr, 
                              fun = AnalyzeData, 
                              tot.time = input.tot.time, 
                              rand.time = input.rand.time)

stopCluster(cl)

# -----------------------------------------------------------------------------
# Estimate the value of time specific means for each DTR 
# u=exp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)

ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "list.df.est.beta",
                    "list.C"))
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

list.df.est.u <- parLapply(cl = cl, 
                           X = list.df.est.beta, 
                           fun = EstimateMeans, 
                           list.C=list.C)

stopCluster(cl)

list.est.u.plusplus <- lapply(list.df.est.u, function(x){return(x["plusplus"])})
list.est.u.plusminus <- lapply(list.df.est.u, function(x){return(x["plusminus"])})
list.est.u.minusplus <- lapply(list.df.est.u, function(x){return(x["minusplus"])})
list.est.u.minusminus <- lapply(list.df.est.u, function(x){return(x["minusminus"])})

# -----------------------------------------------------------------------------
# Estimate the value of quantities of interest for each DTR 
# Lexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "list.df.est.beta",
                    "list.C",
                    "L.AUC", "L.eos.means"))

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

list.df.est.eos.means <- parLapply(cl = cl, 
                                   X = list.df.est.beta, 
                                   fun = EstimateLinearCombo, 
                                   L=L.eos.means,
                                   list.C=list.C)

list.df.est.AUC <- parLapply(cl = cl, 
                             X = list.df.est.beta, 
                             fun = EstimateLinearCombo, 
                             L=L.AUC,
                             list.C=list.C)

stopCluster(cl)

list.est.eos.means.plusplus <- lapply(list.df.est.eos.means, function(x){return(x["plusplus"])})
list.est.eos.means.plusminus <- lapply(list.df.est.eos.means, function(x){return(x["plusminus"])})
list.est.eos.means.minusplus <- lapply(list.df.est.eos.means, function(x){return(x["minusplus"])})
list.est.eos.means.minusminus <- lapply(list.df.est.eos.means, function(x){return(x["minusminus"])})

list.est.AUC.plusplus <- lapply(list.df.est.AUC, function(x){return(x["plusplus"])})
list.est.AUC.plusminus <- lapply(list.df.est.AUC, function(x){return(x["plusminus"])})
list.est.AUC.minusplus <- lapply(list.df.est.AUC, function(x){return(x["minusplus"])})
list.est.AUC.minusminus <- lapply(list.df.est.AUC, function(x){return(x["minusminus"])})

# -----------------------------------------------------------------------------
# Estimate contrasts for each DTR 
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "list.df.est.beta",
                    "list.C"))
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

list.df.est.diff.eos.means <- parLapply(cl = cl, 
                                        X = list.df.est.beta, 
                                        fun = EstimateDiffs, 
                                        D = D.eos.means,
                                        list.C=list.C)

list.df.est.diff.AUC <- parLapply(cl = cl, 
                                  X = list.df.est.beta, 
                                  fun = EstimateDiffs, 
                                  D = D.AUC,
                                  list.C=list.C)

stopCluster(cl)

list.est.diff.eos.means.plusplus.plusminus <- lapply(list.df.est.diff.eos.means, function(x){return(x["plusplus.plusminus"])})
list.est.diff.eos.means.plusplus.minusplus <- lapply(list.df.est.diff.eos.means, function(x){return(x["plusplus.minusplus"])})
list.est.diff.eos.means.plusplus.minusminus <- lapply(list.df.est.diff.eos.means, function(x){return(x["plusplus.minusminus"])})
list.est.diff.eos.means.plusminus.minusplus <- lapply(list.df.est.diff.eos.means, function(x){return(x["plusminus.minusplus"])})
list.est.diff.eos.means.plusminus.minusminus <- lapply(list.df.est.diff.eos.means, function(x){return(x["plusminus.minusminus"])})
list.est.diff.eos.means.minusminus.minusplus <- lapply(list.df.est.diff.eos.means, function(x){return(x["minusminus.minusplus"])})

list.est.diff.AUC.plusplus.plusminus <- lapply(list.df.est.diff.AUC, function(x){return(x["plusplus.plusminus"])})
list.est.diff.AUC.plusplus.minusplus <- lapply(list.df.est.diff.AUC, function(x){return(x["plusplus.minusplus"])})
list.est.diff.AUC.plusplus.minusminus <- lapply(list.df.est.diff.AUC, function(x){return(x["plusplus.minusminus"])})
list.est.diff.AUC.plusminus.minusplus <- lapply(list.df.est.diff.AUC, function(x){return(x["plusminus.minusplus"])})
list.est.diff.AUC.plusminus.minusminus <- lapply(list.df.est.diff.AUC, function(x){return(x["plusminus.minusminus"])})
list.est.diff.AUC.minusminus.minusplus <- lapply(list.df.est.diff.AUC, function(x){return(x["minusminus.minusplus"])})

# -----------------------------------------------------------------------------
# Calculate true value of beta
# -----------------------------------------------------------------------------

###############################################################################
# Using input.means we calculate the value of the parameters gamma_j
# in our model for log(E{Y_t^{(a_1,r,a_2)}})
###############################################################################
gamma.vec <- rep(NA,6*input.tot.time-4*input.rand.time-1)

# Take subset of input.means corresponding to each treatment sequence
# Treatment sequence by time 1
input.means.time.1 <- input.means[input.means$seq=="plus.r","time.1"]
# Treatment sequence by times 2 to input.rand.time
input.means.plus <- input.means[input.means$seq=="plus.r",paste("time.",2:input.rand.time,sep="")]
input.means.minus <- input.means[input.means$seq=="minus.r",paste("time.",2:input.rand.time,sep="")]
# Treatment sequence by time input.rand.time+1 to input.tot.time
input.means.plus.r <- input.means[input.means$seq=="plus.r",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.minus.r <- input.means[input.means$seq=="minus.r",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.plus.nr.plus <- input.means[input.means$seq=="plus.nr.plus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.plus.nr.minus <- input.means[input.means$seq=="plus.nr.minus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.minus.nr.plus <- input.means[input.means$seq=="minus.nr.plus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.minus.nr.minus <- input.means[input.means$seq=="minus.nr.minus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]

# Specify index of gamma_j parameters corresponding to each treatment sequence
# Treatment sequence by time 1
gamma.vec[1] <- log(input.means.time.1)
# Treatment sequence by times 2 to input.rand.time
gamma.idx.plus <- 2:input.rand.time
gamma.idx.minus <- (input.rand.time+1):(2*input.rand.time-1)
gamma.vec[gamma.idx.plus] <- log(input.means.plus) - gamma.vec[1]
gamma.vec[gamma.idx.minus] <- log(input.means.minus) - gamma.vec[1]
# Treatment sequence by time input.rand.time+1 to input.tot.time
gamma.idx.plus.r <- (2*input.rand.time):(input.tot.time+input.rand.time-1)
gamma.idx.minus.r <- (input.tot.time+input.rand.time):(2*input.tot.time-1)
gamma.idx.plus.nr.plus <- (2*input.tot.time):(3*input.tot.time-input.rand.time-1)
gamma.idx.plus.nr.minus <- (3*input.tot.time-input.rand.time):(4*input.tot.time-2*input.rand.time-1)
gamma.idx.minus.nr.plus <- (4*input.tot.time-2*input.rand.time):(5*input.tot.time-3*input.rand.time-1)
gamma.idx.minus.nr.minus <- (5*input.tot.time-3*input.rand.time):(6*input.tot.time-4*input.rand.time-1)
gamma.vec[gamma.idx.plus.r] <- log(input.means.plus.r) - gamma.vec[1]
gamma.vec[gamma.idx.minus.r] <- log(input.means.minus.r) - gamma.vec[1]
gamma.vec[gamma.idx.plus.nr.plus] <- log(input.means.plus.nr.plus) - gamma.vec[1]
gamma.vec[gamma.idx.plus.nr.minus] <- log(input.means.plus.nr.minus) - gamma.vec[1]
gamma.vec[gamma.idx.minus.nr.plus] <- log(input.means.minus.nr.plus) - gamma.vec[1]
gamma.vec[gamma.idx.minus.nr.minus] <- log(input.means.minus.nr.minus) - gamma.vec[1]
gamma.vec <- bind_cols(gamma.vec)
gamma.vec <- t(gamma.vec)
row.names(gamma.vec) <- paste("gamma",1:(6*input.tot.time-4*input.rand.time-1),sep=".")

# Calculate response probabilities
mat.sigma2 <- matrix(rep(NA, 6*input.tot.time), nrow=6)
mat.sigma2 <- data.frame(seq = c("plus.r",
                                 "plus.nr.plus",
                                 "plus.nr.minus",
                                 "minus.r",
                                 "minus.nr.plus",
                                 "minus.nr.minus"),
                         mat.sigma2, row.names=NULL)
colnames(mat.sigma2) <- c("seq",paste("time.",1:input.tot.time,sep=""))

for(i in 1:6){
  for(j in 2:(input.tot.time+1)){
    mat.sigma2[i,j] <- SolveForSigmaSquared(input.mu = input.means[i,j], 
                                            input.prop.zeros = input.prop.zeros[i,j])
  }
}

###############################################################################
# Calculate proportion of responders to a1 using cutoff, mean and variance
# in outcome at rand.time for treatment sequences beginning with a1
###############################################################################
# Treatment sequences beginning with a1=+1
use.sigma2 <- mat.sigma2[mat.sigma2$seq=="plus.r",paste("time.",input.rand.time,sep="")]
use.mean <- input.means[input.means$seq=="plus.r",paste("time.",input.rand.time,sep="")]
p <- pnbinom(q = input.cutoff, size = 1/use.sigma2, mu = use.mean)
# Treatment sequences beginning with a1=-1
use.sigma2 <- mat.sigma2[mat.sigma2$seq=="minus.r",paste("time.",input.rand.time,sep="")]
use.mean <- input.means[input.means$seq=="minus.r",paste("time.",input.rand.time,sep="")]
q <- pnbinom(q = input.cutoff, size = 1/use.sigma2, mu = use.mean)
remove(use.sigma2, use.mean)

###############################################################################
# Calculate the value of the parameters beta_j in our model for 
# log(E{Y_t^{(a_1,a_2)}})
###############################################################################
beta.vec <- as.matrix(rep(NA,4*input.tot.time-2*input.rand.time-1))
row.names(beta.vec) <- paste("beta",1:(4*input.tot.time-2*input.rand.time-1),sep=".")
# DTR from time 1 to input.rand.time
beta.vec[1:(2*input.rand.time-1)] <- gamma.vec[1:(2*input.rand.time-1)] 
# DTR from time input.rand.time+1 to input.tot.time
idx.plusplus <- (2*input.rand.time):(input.tot.time+input.rand.time-1)
idx.plusminus <- (2*input.rand.time):(input.tot.time+input.rand.time-1)
idx.minusplus <- (input.tot.time+input.rand.time):(2*input.tot.time-1)
idx.minusminus <- (input.tot.time+input.rand.time):(2*input.tot.time-1)

mm.plusplus <- p*exp(gamma.vec[idx.plusplus])+(1-p)*exp(gamma.vec[idx.plusplus - 2*input.rand.time+2*input.tot.time])
mm.plusminus <- p*exp(gamma.vec[idx.plusminus])+(1-p)*exp(gamma.vec[idx.plusminus - 3*input.rand.time+3*input.tot.time])
mm.minusplus <- q*exp(gamma.vec[idx.minusplus])+(1-q)*exp(gamma.vec[idx.minusplus - 3*input.rand.time+3*input.tot.time])
mm.minusminus <- q*exp(gamma.vec[idx.minusminus])+(1-q)*exp(gamma.vec[idx.minusminus - 4*input.rand.time+4*input.tot.time])

beta.vec[idx.plusplus] <- log(mm.plusplus)
beta.vec[idx.plusminus+input.tot.time-input.rand.time] <- log(mm.plusminus)
beta.vec[idx.minusplus+input.tot.time-input.rand.time] <- log(mm.minusplus)
beta.vec[idx.minusminus+2*input.tot.time-2*input.rand.time] <- log(mm.minusminus)

# -----------------------------------------------------------------------------
# Calculate true value of time specific means for each DTR 
# u=exp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
C.plusplus <- list.C$C.plusplus
C.plusminus <- list.C$C.plusminus
C.minusplus <- list.C$C.minusplus
C.minusminus <- list.C$C.minusminus

u.plusplus <- exp(C.plusplus %*% beta.vec)
u.plusminus <- exp(C.plusminus %*% beta.vec)
u.minusplus <- exp(C.minusplus %*% beta.vec)
u.minusminus <- exp(C.minusminus %*% beta.vec)

# -----------------------------------------------------------------------------
# Calculate true value of quantities of interest
# -----------------------------------------------------------------------------
eos.means.plusplus <- L.eos.means %*% exp(C.plusplus %*% beta.vec)
eos.means.plusminus <- L.eos.means %*% exp(C.plusminus %*% beta.vec)
eos.means.minusplus <- L.eos.means %*% exp(C.minusplus %*% beta.vec)
eos.means.minusminus <- L.eos.means %*% exp(C.minusminus %*% beta.vec)

AUC.plusplus <- L.AUC %*% exp(C.plusplus %*% beta.vec)
AUC.plusminus <- L.AUC %*% exp(C.plusminus %*% beta.vec)
AUC.minusplus <- L.AUC %*% exp(C.minusplus %*% beta.vec)
AUC.minusminus <- L.AUC %*% exp(C.minusminus %*% beta.vec)

# -----------------------------------------------------------------------------
# Calculate true value of contrasts
# -----------------------------------------------------------------------------
stacked.C.plusplus.plusminus <- rbind(C.plusplus, C.plusminus)
stacked.C.plusplus.minusplus <- rbind(C.plusplus, C.minusplus)
stacked.C.plusplus.minusminus <- rbind(C.plusplus, C.minusminus)
stacked.C.plusminus.minusplus <- rbind(C.plusminus, C.minusplus)
stacked.C.plusminus.minusminus <- rbind(C.plusminus, C.minusminus)
stacked.C.minusminus.minusplus <- rbind(C.minusminus, C.minusplus)

###############################################################################
# Differences in end-of-study means
###############################################################################
diff.eos.means.plusplus.plusminus <- D.eos.means %*% exp(stacked.C.plusplus.plusminus %*% beta.vec)
diff.eos.means.plusplus.minusplus <- D.eos.means %*% exp(stacked.C.plusplus.minusplus %*% beta.vec)
diff.eos.means.plusplus.minusminus <- D.eos.means %*% exp(stacked.C.plusplus.minusminus %*% beta.vec)
diff.eos.means.plusminus.minusplus <- D.eos.means %*% exp(stacked.C.plusminus.minusplus %*% beta.vec)
diff.eos.means.plusminus.minusminus <- D.eos.means %*% exp(stacked.C.plusminus.minusminus %*% beta.vec)
diff.eos.means.minusminus.minusplus <- D.eos.means %*% exp(stacked.C.minusminus.minusplus %*% beta.vec)

###############################################################################
# Differences in AUC
###############################################################################
diff.AUC.plusplus.plusminus <- D.AUC %*% exp(stacked.C.plusplus.plusminus %*% beta.vec)
diff.AUC.plusplus.minusplus <- D.AUC %*% exp(stacked.C.plusplus.minusplus %*% beta.vec)
diff.AUC.plusplus.minusminus <- D.AUC %*% exp(stacked.C.plusplus.minusminus %*% beta.vec)
diff.AUC.plusminus.minusplus <- D.AUC %*% exp(stacked.C.plusminus.minusplus %*% beta.vec)
diff.AUC.plusminus.minusminus <- D.AUC %*% exp(stacked.C.plusminus.minusminus %*% beta.vec)
diff.AUC.minusminus.minusplus <- D.AUC %*% exp(stacked.C.minusminus.minusplus %*% beta.vec)

# -----------------------------------------------------------------------------
# Calculate bias in estimates of betaj and other quantities
# -----------------------------------------------------------------------------
source(file.path(path.code,"sanity-check/calc-bias.R"))

print(list.all)

# -----------------------------------------------------------------------------
# Calculate bais in estimates of standard errors and coverage
# -----------------------------------------------------------------------------
source(file.path(path.code,"sanity-check/calc-coverage.R"))

print(bias.est.stderr.diff.eos.means)
print(bias.est.stderr.diff.AUC)
print(coverage.diff.eos.means)
print(coverage.diff.AUC)
