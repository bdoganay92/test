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
input.tot.time <- 3  # Total no. of measurement occasions
input.rand.time <- 2  # Time when second randomization occurred (time is 1-indexed)
# input.means contains mean of time-specific outcomes under each 
# treatment sequence from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "input_means.csv"), header = TRUE)
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"), header = TRUE)
# Check whether input data are in the correct format
CheckInputData(input.df=input.means, rand.time=input.rand.time, tot.time=input.tot.time)
CheckInputData(input.df=input.prop.zeros, rand.time=input.rand.time, tot.time=input.tot.time)

# -----------------------------------------------------------------------------
# Specify data generating parameters
# -----------------------------------------------------------------------------
idx.nsim <- 1:2000  # Total no. of monte carlo samples
input.N <- 2000  # Total no. of individuals
input.cutoff <- 0  # Cutoff in the definition of response status
input.rho <- 0.7  # Dependence parameter

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
# Using input.means we calculate the value of the parameters gamma_j
# in our model for log(E{Y_t^{(a_1,r,a_2)}})
# -----------------------------------------------------------------------------
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
gamma.vec[gamma.idx.plus.nr.minus] <- log(input.means.minus.nr.plus) - gamma.vec[1]
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

# -----------------------------------------------------------------------------
# Calculate proportion of responders to a1 using cutoff, mean and variance
# in outcome at rand.time for treatment sequences beginning with a1
# -----------------------------------------------------------------------------
# Treatment sequences beginning with a1=+1
use.sigma2 <- mat.sigma2[mat.sigma2$seq=="plus.r",paste("time.",input.rand.time,sep="")]
use.mean <- input.means[input.means$seq=="plus.r",paste("time.",input.rand.time,sep="")]
p <- pnbinom(q = input.cutoff, size = 1/use.sigma2, mu = use.mean)
# Treatment sequences beginning with a1=-1
use.sigma2 <- mat.sigma2[mat.sigma2$seq=="minus.r",paste("time.",input.rand.time,sep="")]
use.mean <- input.means[input.means$seq=="minus.r",paste("time.",input.rand.time,sep="")]
q <- pnbinom(q = input.cutoff, size = 1/use.sigma2, mu = use.mean)
remove(use.sigma2, use.mean)

# -----------------------------------------------------------------------------
# Calculate the value of the parameters beta_j in our model for 
# log(E{Y_t^{(a_1,a_2)}})
# -----------------------------------------------------------------------------
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
# Calculate the value of time specific means for each DTR 
# exp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
C.plusplus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)
C.plusminus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)
C.minusplus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)
C.minusminus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)

i <- 0
j <- 0
for(time.now in 1:input.tot.time){
  if(time.now==1){
    C.plusplus[time.now,1] <- 1
  }else if(time.now>1 & time.now<=input.rand.time){
    C.plusplus[time.now,1] <- 1
    C.plusplus[time.now,2+i] <- 1
    i <- i+1
  }else{
    C.plusplus[time.now,1] <- 1
    C.plusplus[time.now,2*input.rand.time+j] <- 1
    j <- j+1
  }
}

i <- 0
j <- 0
for(time.now in 1:input.tot.time){
  if(time.now==1){
    C.plusminus[time.now,1] <- 1
  }else if(time.now>1 & time.now<=input.rand.time){
    C.plusminus[time.now,1] <- 1
    C.plusminus[time.now,2+i] <- 1
    i <- i+1
  }else{
    C.plusminus[time.now,1] <- 1
    C.plusminus[time.now,input.tot.time+input.rand.time+j] <- 1
    j <- j+1
  }
}

i <- 0
j <- 0
for(time.now in 1:input.tot.time){
  if(time.now==1){
    C.minusplus[time.now,1] <- 1
  }else if(time.now>1 & time.now<=input.rand.time){
    C.minusplus[time.now,1] <- 1
    C.minusplus[time.now,input.rand.time+1+i] <- 1
    i <- i+1
  }else{
    C.minusplus[time.now,1] <- 1
    C.minusplus[time.now,2*input.tot.time+j] <- 1
    j <- j+1
  }
}

i <- 0
j <- 0
for(time.now in 1:input.tot.time){
  if(time.now==1){
    C.minusminus[time.now,1] <- 1
  }else if(time.now>1 & time.now<=input.rand.time){
    C.minusminus[time.now,1] <- 1
    C.minusminus[time.now,input.rand.time+1+i] <- 1
    i <- i+1
  }else{
    C.minusminus[time.now,1] <- 1
    C.minusminus[time.now,3*input.tot.time-input.rand.time+j] <- 1
    j <- j+1
  }
}

u.plusplus <- exp(C.plusplus %*% beta.vec)
u.plusminus <- exp(C.plusminus %*% beta.vec)
u.minusplus <- exp(C.minusplus %*% beta.vec)
u.minusminus <- exp(C.minusminus %*% beta.vec)

# -----------------------------------------------------------------------------
# Calculate the value of diferences in time specific means and AUC between
# two given DTRs: Dexp[Cbeta]
# -----------------------------------------------------------------------------
stacked.C.plusplus.plusminus <- rbind(C.plusplus, C.plusminus)
stacked.C.plusplus.minusplus <- rbind(C.plusplus, C.minusplus)
stacked.C.plusplus.minusminus <- rbind(C.plusplus, C.minusminus)
stacked.C.plusminus.minusplus <- rbind(C.plusminus, C.minusplus)
stacked.C.plusminus.minusminus <- rbind(C.plusminus, C.minusminus)
stacked.C.minusminus.minusplus <- rbind(C.minusminus, C.minusplus)

####### Differences in end-of-study means #####################################
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)
eos.means.diff.plusplus.plusminus <- D.eos.means %*% exp(stacked.C.plusplus.plusminus %*% beta.vec)
eos.means.diff.plusplus.minusplus <- D.eos.means %*% exp(stacked.C.plusplus.minusplus %*% beta.vec)
eos.means.diff.plusplus.minusminus <- D.eos.means %*% exp(stacked.C.plusplus.minusminus %*% beta.vec)
eos.means.diff.plusminus.minusplus <- D.eos.means %*% exp(stacked.C.plusminus.minusplus %*% beta.vec)
eos.means.diff.plusminus.minusminus <- D.eos.means %*% exp(stacked.C.plusminus.minusminus %*% beta.vec)
eos.means.diff.minusminus.minusplus <- D.eos.means %*% exp(stacked.C.minusminus.minusplus %*% beta.vec)

####### Differences in AUC ####################################################
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
AUC.diff.plusplus.plusminus <- D.AUC %*% exp(stacked.C.plusplus.plusminus %*% beta.vec)
AUC.diff.plusplus.minusplus <- D.AUC %*% exp(stacked.C.plusplus.minusplus %*% beta.vec)
AUC.diff.plusplus.minusminus <- D.AUC %*% exp(stacked.C.plusplus.minusminus %*% beta.vec)
AUC.diff.plusminus.minusplus <- D.AUC %*% exp(stacked.C.plusminus.minusplus %*% beta.vec)
AUC.diff.plusminus.minusminus <- D.AUC %*% exp(stacked.C.plusminus.minusminus %*% beta.vec)
AUC.diff.minusminus.minusplus <- D.AUC %*% exp(stacked.C.minusminus.minusplus %*% beta.vec)

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
# exp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
list.est.u.plusplus <- list()
list.est.u.plusminus <- list()
list.est.u.minusplus <- list()
list.est.u.minusminus <- list()

for(i in idx.nsim){
  if(list.df.est.beta[[i]]$converged==1){
    est.beta <- list.df.est.beta[[i]]$est.beta
    # DTR (+1,+1)
    est.u.plusplus <- exp(C.plusplus%*%est.beta)
    est.u.plusplus <- list(data.frame(est.u.plusplus = est.u.plusplus))
    # DTR (+1,-1)
    est.u.plusminus <- exp(C.plusminus%*%est.beta)
    est.u.plusminus <- list(data.frame(est.u.plusminus = est.u.plusminus))
    # DTR (-1,+1)
    est.u.minusplus <- exp(C.minusplus%*%est.beta)
    est.u.minusplus <- list(data.frame(est.u.minusplus = est.u.minusplus))
    # DTR (-1,-1)
    est.u.minusminus <- exp(C.minusminus%*%est.beta)
    est.u.minusminus <- list(data.frame(est.u.minusminus = est.u.minusminus))
  }else{
    est.u.plusplus <- list(data.frame(NULL))
    est.u.plusminus <- list(data.frame(NULL))
    est.u.minusplus <- list(data.frame(NULL))
    est.u.minusminus <- list(data.frame(NULL))
  }
  list.est.u.plusplus <- append(list.est.u.plusplus, est.u.plusplus)
  list.est.u.plusminus <- append(list.est.u.plusminus, est.u.plusminus)
  list.est.u.minusplus <- append(list.est.u.minusplus, est.u.minusplus)
  list.est.u.minusminus <- append(list.est.u.minusminus, est.u.minusminus)
}

# -----------------------------------------------------------------------------
# Estimate the value of differences in end of study means for pairs of DTRs 
# Dexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
list.est.eos.means.diff.plusplus.plusminus <- list()
list.est.eos.means.diff.plusplus.minusplus <- list()
list.est.eos.means.diff.plusplus.minusminus <- list()
list.est.eos.means.diff.plusminus.minusplus <- list()
list.est.eos.means.diff.plusminus.minusminus <- list()
list.est.eos.means.diff.minusminus.minusplus <- list()

for(i in idx.nsim){
  if(list.df.est.beta[[i]]$converged==1){
    est.beta <- list.df.est.beta[[i]]$est.beta
    # DTR (+1,+1) vs. (+1,-1)
    est.eos.means.diff.plusplus.plusminus <- D.eos.means %*% exp(stacked.C.plusplus.plusminus %*% est.beta)
    est.eos.means.diff.plusplus.plusminus <- list(data.frame(est.eos.means.diff.plusplus.plusminus = est.eos.means.diff.plusplus.plusminus))
    # DTR (+1,+1) vs. (-1,+1)
    est.eos.means.diff.plusplus.minusplus <- D.eos.means %*% exp(stacked.C.plusplus.minusplus %*% est.beta)
    est.eos.means.diff.plusplus.minusplus <- list(data.frame(est.eos.means.diff.plusplus.minusplus = est.eos.means.diff.plusplus.minusplus))
    # DTR (+1,+1) vs. (-1,-1)
    est.eos.means.diff.plusplus.minusminus <- D.eos.means %*% exp(stacked.C.plusplus.minusminus %*% est.beta)
    est.eos.means.diff.plusplus.minusminus <- list(data.frame(est.eos.means.diff.plusplus.minusminus = est.eos.means.diff.plusplus.minusminus))
    # DTR (+1,-1) vs. (-1,+1)
    est.eos.means.diff.plusminus.minusplus <- D.eos.means %*% exp(stacked.C.plusminus.minusplus %*% est.beta)
    est.eos.means.diff.plusminus.minusplus <- list(data.frame(est.eos.means.diff.plusminus.minusplus = est.eos.means.diff.plusminus.minusplus))
    # DTR (+1,-1) vs. (-1,-1)
    est.eos.means.diff.plusminus.minusminus <- D.eos.means %*% exp(stacked.C.plusminus.minusminus %*% est.beta)
    est.eos.means.diff.plusminus.minusminus <- list(data.frame(est.eos.means.diff.plusminus.minusminus = est.eos.means.diff.plusminus.minusminus))
    # DTR (-1,-1) vs. (-1,+1)
    est.eos.means.diff.minusminus.minusplus <- D.eos.means %*% exp(stacked.C.minusminus.minusplus %*% est.beta)
    est.eos.means.diff.minusminus.minusplus <- list(data.frame(est.eos.means.diff.minusminus.minusplus = est.eos.means.diff.minusminus.minusplus))
  }else{
    list.est.eos.means.diff.plusplus.plusminus <- list(data.frame(NULL))
    list.est.eos.means.diff.plusplus.minusplus <- list(data.frame(NULL))
    list.est.eos.means.diff.plusplus.minusminus <- list(data.frame(NULL))
    list.est.eos.means.diff.plusminus.minusplus <- list(data.frame(NULL))
    list.est.eos.means.diff.plusminus.minusminus <- list(data.frame(NULL))
    list.est.eos.means.diff.minusminus.minusplus <- list(data.frame(NULL))
  }
  list.est.eos.means.diff.plusplus.plusminus <- append(list.est.eos.means.diff.plusplus.plusminus, 
                                                       est.eos.means.diff.plusplus.plusminus)
  list.est.eos.means.diff.plusplus.minusplus <- append(list.est.eos.means.diff.plusplus.minusplus,
                                                       est.eos.means.diff.plusplus.minusplus)
  list.est.eos.means.diff.plusplus.minusminus <- append(list.est.eos.means.diff.plusplus.minusminus,
                                                        est.eos.means.diff.plusplus.minusminus)
  list.est.eos.means.diff.plusminus.minusplus <- append(list.est.eos.means.diff.plusminus.minusplus,
                                                        est.eos.means.diff.plusminus.minusplus)
  list.est.eos.means.diff.plusminus.minusminus <- append(list.est.eos.means.diff.plusminus.minusminus,
                                                         est.eos.means.diff.plusminus.minusminus)
  list.est.eos.means.diff.minusminus.minusplus <- append(list.est.eos.means.diff.minusminus.minusplus,
                                                         est.eos.means.diff.minusminus.minusplus)
}

# -----------------------------------------------------------------------------
# Estimate the value of differences in AUC from time 1 to input.tot.time for 
# pairs of DTRs: Dexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
list.est.AUC.diff.plusplus.plusminus <- list()
list.est.AUC.diff.plusplus.minusplus <- list()
list.est.AUC.diff.plusplus.minusminus <- list()
list.est.AUC.diff.plusminus.minusplus <- list()
list.est.AUC.diff.plusminus.minusminus <- list()
list.est.AUC.diff.minusminus.minusplus <- list()

for(i in idx.nsim){
  if(list.df.est.beta[[i]]$converged==1){
    est.beta <- list.df.est.beta[[i]]$est.beta
    # DTR (+1,+1) vs. (+1,-1)
    est.AUC.diff.plusplus.plusminus <- D.AUC %*% exp(stacked.C.plusplus.plusminus %*% est.beta)
    est.AUC.diff.plusplus.plusminus <- list(data.frame(est.AUC.diff.plusplus.plusminus = est.AUC.diff.plusplus.plusminus))
    # DTR (+1,+1) vs. (-1,+1)
    est.AUC.diff.plusplus.minusplus <- D.AUC %*% exp(stacked.C.plusplus.minusplus %*% est.beta)
    est.AUC.diff.plusplus.minusplus <- list(data.frame(est.AUC.diff.plusplus.minusplus = est.AUC.diff.plusplus.minusplus))
    # DTR (+1,+1) vs. (-1,-1)
    est.AUC.diff.plusplus.minusminus <- D.AUC %*% exp(stacked.C.plusplus.minusminus %*% est.beta)
    est.AUC.diff.plusplus.minusminus <- list(data.frame(est.AUC.diff.plusplus.minusminus = est.AUC.diff.plusplus.minusminus))
    # DTR (+1,-1) vs. (-1,+1)
    est.AUC.diff.plusminus.minusplus <- D.AUC %*% exp(stacked.C.plusminus.minusplus %*% est.beta)
    est.AUC.diff.plusminus.minusplus <- list(data.frame(est.AUC.diff.plusminus.minusplus = est.AUC.diff.plusminus.minusplus))
    # DTR (+1,-1) vs. (-1,-1)
    est.AUC.diff.plusminus.minusminus <- D.AUC %*% exp(stacked.C.plusminus.minusminus %*% est.beta)
    est.AUC.diff.plusminus.minusminus <- list(data.frame(est.AUC.diff.plusminus.minusminus = est.AUC.diff.plusminus.minusminus))
    # DTR (-1,-1) vs. (-1,+1)
    est.AUC.diff.minusminus.minusplus <- D.AUC %*% exp(stacked.C.minusminus.minusplus %*% est.beta)
    est.AUC.diff.minusminus.minusplus <- list(data.frame(est.AUC.diff.minusminus.minusplus = est.AUC.diff.minusminus.minusplus))
  }else{
    list.est.AUC.diff.plusplus.plusminus <- list(data.frame(NULL))
    list.est.AUC.diff.plusplus.minusplus <- list(data.frame(NULL))
    list.est.AUC.diff.plusplus.minusminus <- list(data.frame(NULL))
    list.est.AUC.diff.plusminus.minusplus <- list(data.frame(NULL))
    list.est.AUC.diff.plusminus.minusminus <- list(data.frame(NULL))
    list.est.AUC.diff.minusminus.minusplus <- list(data.frame(NULL))
  }
  list.est.AUC.diff.plusplus.plusminus <- append(list.est.AUC.diff.plusplus.plusminus, 
                                                       est.AUC.diff.plusplus.plusminus)
  list.est.AUC.diff.plusplus.minusplus <- append(list.est.AUC.diff.plusplus.minusplus,
                                                       est.AUC.diff.plusplus.minusplus)
  list.est.AUC.diff.plusplus.minusminus <- append(list.est.AUC.diff.plusplus.minusminus,
                                                        est.AUC.diff.plusplus.minusminus)
  list.est.AUC.diff.plusminus.minusplus <- append(list.est.AUC.diff.plusminus.minusplus,
                                                        est.AUC.diff.plusminus.minusplus)
  list.est.AUC.diff.plusminus.minusminus <- append(list.est.AUC.diff.plusminus.minusminus,
                                                         est.AUC.diff.plusminus.minusminus)
  list.est.AUC.diff.minusminus.minusplus <- append(list.est.AUC.diff.minusminus.minusplus,
                                                         est.AUC.diff.minusminus.minusplus)
}

# -----------------------------------------------------------------------------
# Calculate standard error of differences in AUC from time 1 to input.tot.time
# for pairs of DTRs: Dexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------

stderr.AUC.diff.plusplus.plusminus <- sqrt(var(do.call(rbind,list.est.AUC.diff.plusplus.plusminus)))
stderr.AUC.diff.plusplus.minusplus <- sqrt(var(do.call(rbind,list.est.AUC.diff.plusplus.minusplus)))
stderr.AUC.diff.plusplus.minusminus <- sqrt(var(do.call(rbind,list.est.AUC.diff.plusplus.minusminus)))
stderr.AUC.diff.plusminus.minusplus <- sqrt(var(do.call(rbind,list.est.AUC.diff.plusminus.minusplus)))
stderr.AUC.diff.plusminus.minusminus <- sqrt(var(do.call(rbind,list.est.AUC.diff.plusminus.minusminus)))
stderr.AUC.diff.minusminus.minusplus <- sqrt(var(do.call(rbind,list.est.AUC.diff.minusminus.minusplus)))

# -----------------------------------------------------------------------------
# Evaluate estimates: bias of estimated betas
# -----------------------------------------------------------------------------
list.out <- lapply(list.df.est.beta, 
                   function(x, list.true.vals = list(true.beta = beta.vec)){
                     converged <- x$converged
                     if(converged==1){
                       est.beta <- x$est.beta
                       true.beta <- as.numeric(list.true.vals$true.beta)
                       outdf <- data.frame(bias=true.beta - est.beta)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)

# -----------------------------------------------------------------------------
# Evaluate estimates: bias of estimated means of time-specific outcomes
# -----------------------------------------------------------------------------

##### DTR (+1,+1) #############################################################
list.out <- lapply(list.est.u.plusplus, 
                   function(x, list.true.vals = list(true.means = u.plusplus)){
                     if(nrow(x)>0){
                       outdf <- data.frame(bias=u.plusplus-x)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)

##### DTR (+1,-1) #############################################################
list.out <- lapply(list.est.u.plusminus, 
                   function(x, list.true.vals = list(true.means = u.plusminus)){
                     if(nrow(x)>0){
                       outdf <- data.frame(bias=u.plusplus-x)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)

##### DTR (-1,+1) #############################################################
list.out <- lapply(list.est.u.minusplus, 
                   function(x, list.true.vals = list(true.means = u.minusplus)){
                     if(nrow(x)>0){
                       outdf <- data.frame(bias=u.plusplus-x)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)

##### DTR (-1,-1) #############################################################
list.out <- lapply(list.est.u.minusminus, 
                   function(x, list.true.vals = list(true.means = u.minusminus)){
                     if(nrow(x)>0){
                       outdf <- data.frame(bias=u.plusplus-x)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)

# -----------------------------------------------------------------------------
# Estimate the value of differences in end of study means for pairs of DTRs
# Dexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------

##### DTR (+1,+1) vs. DTR (-1,-1) #############################################
list.out <- lapply(list.est.eos.means.diff.plusplus.minusminus, 
                   function(x, list.true.vals = list(true.diff = eos.means.diff.plusplus.minusminus)){
                     if(nrow(x)>0){
                       outdf <- data.frame(bias=eos.means.diff.plusplus.minusminus-x)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)

# -----------------------------------------------------------------------------
# Estimates of the value of differences in AUC for pairs of DTRs
# Dexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------

##### DTR (+1,+1) vs. DTR (-1,-1) #############################################
list.out <- lapply(list.est.AUC.diff.plusplus.minusminus, 
                   function(x, list.true.vals = list(true.diff = AUC.diff.plusplus.minusminus)){
                     if(nrow(x)>0){
                       outdf <- data.frame(bias=AUC.diff.plusplus.minusminus-x)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)

# -----------------------------------------------------------------------------
# Standard error of estimates of the value of differences in AUC for pairs of
# DTRs Dexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------

##### DTR (+1,+1) vs. DTR (-1,-1) #############################################
list.out <- lapply(list.est.stderr.AUC.diff.plusplus.minusminus, 
                   function(x, list.true.vals = list(true.diff = stderr.AUC.diff.plusplus.minusminus)){
                     if(nrow(x)>0){
                       outdf <- data.frame(bias=list.true.vals$true.diff-x)
                     }else{
                       outdf <- data.frame(NULL)
                     }
                     return(outdf)
                   })

out <- bind_cols(list.out)
n.converged <- ncol(out)
summarise.out <- rowMeans(out)/n.converged
summarise.out <- round(summarise.out, digits = 3)
print(summarise.out)



