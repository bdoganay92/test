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
input.tot.time <- 11  # Total no. of time points
input.rand.time <- 7  # Time when second randomization occurred (time is 1-indexed)
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
  x[,paste("time.",time.now,sep="")] <- 1
  
  if(time.now == 1){
    x[,paste("time.",2:tot.time,sep="")] <- 0
  }else if(time.now > 1 & time.now < tot.time){
    x[,paste("time.",1:(time.now-1),sep="")] <- 0
    x[,paste("time.",(time.now+1):tot.time,sep="")] <- 0
  }else{
    x[,paste("time.",1:(tot.time-1),sep="")] <- 0
  }
  
  return(x)
}
)

melted.input.C <- lapply(input.C, MeltC, 
                         tot.time = input.tot.time, 
                         rand.time = input.rand.time)

# -----------------------------------------------------------------------------
# Calculate true values of contrasts
# -----------------------------------------------------------------------------

true.addbeta <- c(true.beta[1], true.beta[2:length(true.beta)] + true.beta[1])
true.addbeta <- as.matrix(true.addbeta)

true.margmeans <- lapply(melted.input.C, function(x, use.addbeta = true.addbeta){
  return(exp(x %*% use.addbeta))
})
true.margmeans <- bind_cols(true.margmeans)

input.L <- list(AUC = NULL, EndOfStudyMeans = NULL)
input.L$AUC <- blank.marginal.df
input.L$AUC[,"time.1"] <- 1/2
input.L$AUC[,paste("time.",input.tot.time,sep="")] <- 1/2
input.L$AUC[,paste("time.",2:(input.tot.time-1),sep="")] <- 1

input.L$EndOfStudyMeans <- blank.marginal.df
input.L$EndOfStudyMeans[,paste("time.",input.tot.time,sep="")] <- 1
input.L$EndOfStudyMeans[,paste("time.",1:(input.tot.time-1),sep="")] <- 0

# Resulting quantities
true.margquantities <- as.list(rep(NA, length(input.L)))
names(true.margquantities) <- names(input.L)

for(i in 1:length(input.L)){
  mat <- input.L[[i]][,2:ncol(input.L[[i]])]
  true.margquantities[[i]] <- mat * true.margmeans
  true.margquantities[[i]] <- rowSums(true.margquantities[[i]])
  true.margquantities[[i]] <- as.matrix(true.margquantities[[i]])
}

# Differences between DTRs
input.D <- as.list(rep(NA, length(input.L)))
names(input.D) <- names(input.L)
for(i in 1:length(input.L)){
  these.cols <- 2:ncol(input.L[[i]])
  names1 <- colnames(input.L[[i]][,these.cols])
  names2 <- names1
  names1 <- paste("d1.",names1,sep="")
  names2 <- paste("d2.",names2,sep="")
  
  these.elements <- input.L[[i]][1,these.cols]
  mat1 <- rbind(these.elements, these.elements, these.elements,
                these.elements, these.elements, these.elements)
  mat2 <- -1*mat1
  
  input.D[[i]]<- cbind(c("plusplus.plusminus",
                         "plusplus.minusplus",
                         "plusplus.minusminus",
                         "plusminus.minusplus",
                         "plusminus.minusminus",
                         "minusplus.minusminus"),
                       mat1, mat2)
  colnames(input.D[[i]]) <- c("diff",names1,names2)
}

# Resulting quantities
true.diffs <- as.list(rep(NA, length(input.D)))
names(true.diffs) <- names(input.D)

for(i in 1:length(input.D)){
  mat1 <- input.D[[i]][,2:ncol(input.D[[i]])]
  
  plusplus.plusminus <- cbind(true.margmeans[1,],true.margmeans[2,])
  plusplus.minusplus <- cbind(true.margmeans[1,],true.margmeans[3,])
  plusplus.minusminus <- cbind(true.margmeans[1,],true.margmeans[4,])
  plusminus.minusplus <- cbind(true.margmeans[2,],true.margmeans[3,])
  plusminus.minusminus <- cbind(true.margmeans[2,],true.margmeans[4,])
  minusplus.minusminus <- cbind(true.margmeans[3,],true.margmeans[4,])
  
  mat2 <- rbind(plusplus.plusminus, plusplus.minusplus,
                plusplus.minusminus, plusminus.minusplus,
                plusminus.minusminus, minusplus.minusminus)
  
  true.diffs[[i]] <- mat1 * mat2
  true.diffs[[i]] <- rowSums(true.diffs[[i]])
  true.diffs[[i]] <- as.matrix(true.diffs[[i]])
}


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
                    "melted.input.C","input.L","input.D"))
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
                                      est.addbeta <- est.beta
                                      these.rows <- 2:nrow(est.addbeta)
                                      est.addbeta[these.rows] <- est.addbeta[these.rows] + est.beta[1] 
                                      out <- lapply(list.C, 
                                                    function(mat, b = est.addbeta){
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

list.df.est.margquantities <-  parLapply(cl=cl,
                                         X = list.df.est.margmean,
                                         fun = function(x, 
                                                        list.L = input.L){
                                           if(is.null(x)){
                                             out <- NULL
                                           }else{
                                             out <- as.list(rep(NA, length(list.L)))
                                             names(out) <- names(list.L)
                                             for(i in 1:length(list.L)){
                                               mat <- list.L[[i]][,2:ncol(list.L[[i]])]
                                               out[[i]] <- mat*x
                                               out[[i]] <- rowSums(out[[i]])
                                               out[[i]] <- as.matrix(out[[i]])
                                             }
                                           }
                                           return(out)
                                         })

list.df.est.diffs <- parLapply(cl=cl,
                               X = list.df.est.margmean, 
                               fun = function(x,list.D = input.D){
                                 if(is.null(x)){
                                   out <- NULL
                                 }else{
                                   out <- as.list(rep(NA, length(list.D)))
                                   names(out) <- names(list.D)
                                   for(i in 1:length(list.D)){
                                     mat1 <- list.D[[i]][,2:ncol(list.D[[i]])]
                                     
                                     plusplus.plusminus <- cbind(x[1,],x[2,])
                                     plusplus.minusplus <- cbind(x[1,],x[3,])
                                     plusplus.minusminus <- cbind(x[1,],x[4,])
                                     plusminus.minusplus <- cbind(x[2,],x[3,])
                                     plusminus.minusminus <- cbind(x[2,],x[4,])
                                     minusplus.minusminus <- cbind(x[3,],x[4,])
                                     
                                     mat2 <- rbind(plusplus.plusminus, plusplus.minusplus,
                                                   plusplus.minusminus, plusminus.minusplus,
                                                   plusminus.minusminus, minusplus.minusminus)
                                     
                                     out[[i]] <- mat1 * mat2
                                     out[[i]] <- rowSums(out[[i]])
                                     out[[i]] <- as.matrix(out[[i]])
                                     out[[i]] <- data.frame(DTRPair = c("plusplus.plusminus",
                                                                        "plusplus.minusplus",
                                                                        "plusplus.minusminus",
                                                                        "plusminus.minusplus",
                                                                        "plusminus.minusminus",
                                                                        "minusplus.minusminus"),
                                                            d = out[[i]])
                                   }
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
# Evaluate estimates: resulting quantities
# -----------------------------------------------------------------------------

list.out <- lapply(list.df.est.margquantities,
                   function(x, list.true.vals = list(true.margquantities = true.margquantities)){
                     if(length(x)==1){
                       if(nrow(x)==0){
                         outlist <- data.frame(NULL)
                       }else{
                         outlist[[i]] <- x[[i]] - list.true.vals$true.margquantities[[i]]
                       }
                     }else{
                       num.quantities <- length(true.margquantities)
                       outlist <- as.list(rep(NA), num.quantities)
                       for(i in 1:num.quantities){
                         outlist[[i]] <- x[[i]] - list.true.vals$true.margquantities[[i]]
                       }
                     }
                     return(outlist)
                   }
)

list.out <- lapply(list.out, function(x){
  return(bind_cols(x))
})

df.out <- matrix(rep(0, 4*length(true.margquantities)), nrow=4)
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

bias.margquantities <- df.out
remove(df.out, list.out)

# -----------------------------------------------------------------------------
# Evaluate estimates: resulting differences
# -----------------------------------------------------------------------------

list.out <- lapply(list.df.est.diffs,
                   function(x, list.true.vals = list(true.diffs = true.diffs)){
                     if(length(x)==1){
                       if(nrow(x)==0){
                         outlist <- data.frame(NULL)
                       }else{
                         outlist[[i]] <- x[[i]][,"d"] - list.true.vals$true.diffs[[i]]
                       }
                     }else{
                       num.quantities <- length(true.diffs)
                       outlist <- as.list(rep(NA), num.quantities)
                       for(i in 1:num.quantities){
                         outlist[[i]] <- x[[i]][,"d"] - list.true.vals$true.diffs[[i]]
                       }
                     }
                     return(outlist)
                   }
)


df.out <- matrix(rep(0, 6*length(true.diffs)), nrow=6)
n.converged <- 0

for(i in 1:length(list.out)){
  
  this.mat <- bind_cols(list.out[[i]])
  if(nrow(this.mat)>0){
    df.out <- df.out + this.mat 
    n.converged <- n.converged + 1
  }else{
    next
  }
}

df.out <- df.out/n.converged

bias.diffs <- df.out
remove(df.out, list.out)

# -----------------------------------------------------------------------------
# Print out calculated quantities
# -----------------------------------------------------------------------------

df.out.bias <- data.frame(max.bias.betas = max(bias.betas$est.bias),
                          max.bias.margmean = max(bias.margmean),
                          max.bias.margquantities = max(bias.margquantities),
                          max.bias.diffs = max(bias.diffs)
                          )
df.out.bias  



