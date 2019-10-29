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
# Result of calibration step
# -----------------------------------------------------------------------------
input.rho <- rho.star

# -----------------------------------------------------------------------------
# Combine all inputs into a grid
# -----------------------------------------------------------------------------
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

# Create C matrix
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
C.plusplus <- list.C$C.plusplus
C.plusminus <- list.C$C.plusminus
C.minusplus <- list.C$C.minusplus
C.minusminus <- list.C$C.minusminus

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
# Calculate delta
# -----------------------------------------------------------------------------
gridx <- expand.grid(nsim=idx.nsim, 
                     input.N=5000,
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

###############################################################################
# Aggregate results for end-of-study means
###############################################################################
list.delta.eos.means.plusplus.plusminus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.plusminus"])})
list.delta.eos.means.plusplus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.minusplus"])})
list.delta.eos.means.plusplus.minusminus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.minusminus"])})
list.delta.eos.means.plusminus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["plusminus.minusplus"])})
list.delta.eos.means.plusminus.minusminus <- lapply(list.delta.eos.means, function(x){return(x["plusminus.minusminus"])})
list.delta.eos.means.minusminus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["minusminus.minusplus"])})

delta.eos.means.plusplus.plusminus <- lapply(list.delta.eos.means.plusplus.plusminus, ReshapeList)
delta.eos.means.plusplus.plusminus <- bind_rows(delta.eos.means.plusplus.plusminus)
delta.eos.means.plusplus.plusminus <- delta.eos.means.plusplus.plusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusplus.minusplus <- lapply(list.delta.eos.means.plusplus.minusplus, ReshapeList)
delta.eos.means.plusplus.minusplus <- bind_rows(delta.eos.means.plusplus.minusplus)
delta.eos.means.plusplus.minusplus <- delta.eos.means.plusplus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusplus.minusminus <- lapply(list.delta.eos.means.plusplus.minusminus, ReshapeList)
delta.eos.means.plusplus.minusminus <- bind_rows(delta.eos.means.plusplus.minusminus)
delta.eos.means.plusplus.minusminus <- delta.eos.means.plusplus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusminus.minusplus <- lapply(list.delta.eos.means.plusminus.minusplus, ReshapeList)
delta.eos.means.plusminus.minusplus <- bind_rows(delta.eos.means.plusminus.minusplus)
delta.eos.means.plusminus.minusplus <- delta.eos.means.plusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusminus.minusminus <- lapply(list.delta.eos.means.plusminus.minusminus, ReshapeList)
delta.eos.means.plusminus.minusminus <- bind_rows(delta.eos.means.plusminus.minusminus)
delta.eos.means.plusminus.minusminus <- delta.eos.means.plusminus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.minusminus.minusplus <- lapply(list.delta.eos.means.minusminus.minusplus, ReshapeList)
delta.eos.means.minusminus.minusplus <- bind_rows(delta.eos.means.minusminus.minusplus)
delta.eos.means.minusminus.minusplus <- delta.eos.means.minusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

list.delta.eos.means <- list(delta.eos.means.plusplus.plusminus=delta.eos.means.plusplus.plusminus,
                             delta.eos.means.plusplus.minusplus=delta.eos.means.plusplus.minusplus,
                             delta.eos.means.plusplus.minusminus=delta.eos.means.plusplus.minusminus,
                             delta.eos.means.plusminus.minusplus=delta.eos.means.plusminus.minusplus,
                             delta.eos.means.plusminus.minusminus=delta.eos.means.plusminus.minusminus,
                             delta.eos.means.minusminus.minusplus=delta.eos.means.minusminus.minusplus)

###############################################################################
# Aggregate results for AUC
###############################################################################
list.delta.AUC.plusplus.plusminus <- lapply(list.delta.AUC, function(x){return(x["plusplus.plusminus"])})
list.delta.AUC.plusplus.minusplus <- lapply(list.delta.AUC, function(x){return(x["plusplus.minusplus"])})
list.delta.AUC.plusplus.minusminus <- lapply(list.delta.AUC, function(x){return(x["plusplus.minusminus"])})
list.delta.AUC.plusminus.minusplus <- lapply(list.delta.AUC, function(x){return(x["plusminus.minusplus"])})
list.delta.AUC.plusminus.minusminus <- lapply(list.delta.AUC, function(x){return(x["plusminus.minusminus"])})
list.delta.AUC.minusminus.minusplus <- lapply(list.delta.AUC, function(x){return(x["minusminus.minusplus"])})


delta.AUC.plusplus.plusminus <- lapply(list.delta.AUC.plusplus.plusminus, ReshapeList)
delta.AUC.plusplus.plusminus <- bind_rows(delta.AUC.plusplus.plusminus)
delta.AUC.plusplus.plusminus <- delta.AUC.plusplus.plusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusplus.minusplus <- lapply(list.delta.AUC.plusplus.minusplus, ReshapeList)
delta.AUC.plusplus.minusplus <- bind_rows(delta.AUC.plusplus.minusplus)
delta.AUC.plusplus.minusplus <- delta.AUC.plusplus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusplus.minusminus <- lapply(list.delta.AUC.plusplus.minusminus, ReshapeList)
delta.AUC.plusplus.minusminus <- bind_rows(delta.AUC.plusplus.minusminus)
delta.AUC.plusplus.minusminus <- delta.AUC.plusplus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusminus.minusplus <- lapply(list.delta.AUC.plusminus.minusplus, ReshapeList)
delta.AUC.plusminus.minusplus <- bind_rows(delta.AUC.plusminus.minusplus)
delta.AUC.plusminus.minusplus <- delta.AUC.plusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusminus.minusminus <- lapply(list.delta.AUC.plusminus.minusminus, ReshapeList)
delta.AUC.plusminus.minusminus <- bind_rows(delta.AUC.plusminus.minusminus)
delta.AUC.plusminus.minusminus <- delta.AUC.plusminus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.minusminus.minusplus <- lapply(list.delta.AUC.minusminus.minusplus, ReshapeList)
delta.AUC.minusminus.minusplus <- bind_rows(delta.AUC.minusminus.minusplus)
delta.AUC.minusminus.minusplus <- delta.AUC.minusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

list.delta.AUC <- list(delta.AUC.plusplus.plusminus=delta.AUC.plusplus.plusminus,
                       delta.AUC.plusplus.minusplus=delta.AUC.plusplus.minusplus,
                       delta.AUC.plusplus.minusminus=delta.AUC.plusplus.minusminus,
                       delta.AUC.plusminus.minusplus=delta.AUC.plusminus.minusplus,
                       delta.AUC.plusminus.minusminus=delta.AUC.plusminus.minusminus,
                       delta.AUC.minusminus.minusplus=delta.AUC.minusminus.minusplus)

###############################################################################
# Aggregate all results
###############################################################################
delta.eos.means <- bind_rows(list.delta.eos.means)
delta.AUC <- bind_rows(list.delta.AUC)

delta.eos.means$pair <- 1:6
delta.AUC$pair <- 1:6

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
# Reshape list of estimates
# -----------------------------------------------------------------------------
list.est.diff.eos.means.plusplus.plusminus <- lapply(list.est.diff.eos.means.plusplus.plusminus, ReshapeList)
list.est.diff.eos.means.plusplus.minusplus <- lapply(list.est.diff.eos.means.plusplus.minusplus, ReshapeList)
list.est.diff.eos.means.plusplus.minusminus <- lapply(list.est.diff.eos.means.plusplus.minusminus, ReshapeList)
list.est.diff.eos.means.plusminus.minusplus <- lapply(list.est.diff.eos.means.plusminus.minusplus, ReshapeList)
list.est.diff.eos.means.plusminus.minusminus <- lapply(list.est.diff.eos.means.plusminus.minusminus, ReshapeList)
list.est.diff.eos.means.minusminus.minusplus <- lapply(list.est.diff.eos.means.minusminus.minusplus, ReshapeList)

list.est.diff.AUC.plusplus.plusminus <- lapply(list.est.diff.AUC.plusplus.plusminus, ReshapeList)
list.est.diff.AUC.plusplus.minusplus <- lapply(list.est.diff.AUC.plusplus.minusplus, ReshapeList)
list.est.diff.AUC.plusplus.minusminus <- lapply(list.est.diff.AUC.plusplus.minusminus, ReshapeList)
list.est.diff.AUC.plusminus.minusplus <- lapply(list.est.diff.AUC.plusminus.minusplus, ReshapeList)
list.est.diff.AUC.plusminus.minusminus <- lapply(list.est.diff.AUC.plusminus.minusminus, ReshapeList)
list.est.diff.AUC.minusminus.minusplus <- lapply(list.est.diff.AUC.minusminus.minusplus, ReshapeList)

# -----------------------------------------------------------------------------
# Aggregate list of estimates
# -----------------------------------------------------------------------------
est.diff.eos.means.plusplus.plusminus <- bind_rows(list.est.diff.eos.means.plusplus.plusminus)
est.diff.eos.means.plusplus.minusplus <- bind_rows(list.est.diff.eos.means.plusplus.minusplus)
est.diff.eos.means.plusplus.minusminus <- bind_rows(list.est.diff.eos.means.plusplus.minusminus)
est.diff.eos.means.plusminus.minusplus <- bind_rows(list.est.diff.eos.means.plusminus.minusplus)
est.diff.eos.means.plusminus.minusminus <- bind_rows(list.est.diff.eos.means.plusminus.minusminus)
est.diff.eos.means.minusminus.minusplus <- bind_rows(list.est.diff.eos.means.minusminus.minusplus)

est.diff.AUC.plusplus.plusminus <- bind_rows(list.est.diff.AUC.plusplus.plusminus)
est.diff.AUC.plusplus.minusplus <- bind_rows(list.est.diff.AUC.plusplus.minusplus)
est.diff.AUC.plusplus.minusminus <- bind_rows(list.est.diff.AUC.plusplus.minusminus)
est.diff.AUC.plusminus.minusplus <- bind_rows(list.est.diff.AUC.plusminus.minusplus)
est.diff.AUC.plusminus.minusminus <- bind_rows(list.est.diff.AUC.plusminus.minusminus)
est.diff.AUC.minusminus.minusplus <- bind_rows(list.est.diff.AUC.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate estimates of standard error of pairwise differences in 
# end of study means between DTRs
# -----------------------------------------------------------------------------
list.est.stderr.diff.eos.means <- lapply(list.df.est.beta, EstimateStdErrDiffs, D=D.eos.means, list.C=list.C)

list.est.stderr.diff.eos.means.plusplus.plusminus <- lapply(list.est.stderr.diff.eos.means, function(x){return(x["plusplus.plusminus"])})
list.est.stderr.diff.eos.means.plusplus.minusplus <- lapply(list.est.stderr.diff.eos.means, function(x){return(x["plusplus.minusplus"])})
list.est.stderr.diff.eos.means.plusplus.minusminus <- lapply(list.est.stderr.diff.eos.means, function(x){return(x["plusplus.minusminus"])})
list.est.stderr.diff.eos.means.plusminus.minusplus <- lapply(list.est.stderr.diff.eos.means, function(x){return(x["plusminus.minusplus"])})
list.est.stderr.diff.eos.means.plusminus.minusminus <- lapply(list.est.stderr.diff.eos.means, function(x){return(x["plusminus.minusminus"])})
list.est.stderr.diff.eos.means.minusminus.minusplus <- lapply(list.est.stderr.diff.eos.means, function(x){return(x["minusminus.minusplus"])})

list.est.stderr.diff.eos.means.plusplus.plusminus <- lapply(list.est.stderr.diff.eos.means.plusplus.plusminus, ReshapeList)
list.est.stderr.diff.eos.means.plusplus.minusplus <- lapply(list.est.stderr.diff.eos.means.plusplus.minusplus, ReshapeList)
list.est.stderr.diff.eos.means.plusplus.minusminus <- lapply(list.est.stderr.diff.eos.means.plusplus.minusminus, ReshapeList)
list.est.stderr.diff.eos.means.plusminus.minusplus <- lapply(list.est.stderr.diff.eos.means.plusminus.minusplus, ReshapeList)
list.est.stderr.diff.eos.means.plusminus.minusminus <- lapply(list.est.stderr.diff.eos.means.plusminus.minusminus, ReshapeList)
list.est.stderr.diff.eos.means.minusminus.minusplus <- lapply(list.est.stderr.diff.eos.means.minusminus.minusplus, ReshapeList)

est.stderr.diff.eos.means.plusplus.plusminus <- bind_rows(list.est.stderr.diff.eos.means.plusplus.plusminus)
est.stderr.diff.eos.means.plusplus.minusplus <- bind_rows(list.est.stderr.diff.eos.means.plusplus.minusplus)
est.stderr.diff.eos.means.plusplus.minusminus <- bind_rows(list.est.stderr.diff.eos.means.plusplus.minusminus)
est.stderr.diff.eos.means.plusminus.minusplus <- bind_rows(list.est.stderr.diff.eos.means.plusminus.minusplus)
est.stderr.diff.eos.means.plusminus.minusminus <- bind_rows(list.est.stderr.diff.eos.means.plusminus.minusminus)
est.stderr.diff.eos.means.minusminus.minusplus <- bind_rows(list.est.stderr.diff.eos.means.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate estimates of standard error of pairwise differences in 
# AUC between DTRs
# -----------------------------------------------------------------------------
list.est.stderr.diff.AUC <- lapply(list.df.est.beta, EstimateStdErrDiffs, D=D.AUC, list.C=list.C)

list.est.stderr.diff.AUC.plusplus.plusminus <- lapply(list.est.stderr.diff.AUC, function(x){return(x["plusplus.plusminus"])})
list.est.stderr.diff.AUC.plusplus.minusplus <- lapply(list.est.stderr.diff.AUC, function(x){return(x["plusplus.minusplus"])})
list.est.stderr.diff.AUC.plusplus.minusminus <- lapply(list.est.stderr.diff.AUC, function(x){return(x["plusplus.minusminus"])})
list.est.stderr.diff.AUC.plusminus.minusplus <- lapply(list.est.stderr.diff.AUC, function(x){return(x["plusminus.minusplus"])})
list.est.stderr.diff.AUC.plusminus.minusminus <- lapply(list.est.stderr.diff.AUC, function(x){return(x["plusminus.minusminus"])})
list.est.stderr.diff.AUC.minusminus.minusplus <- lapply(list.est.stderr.diff.AUC, function(x){return(x["minusminus.minusplus"])})

list.est.stderr.diff.AUC.plusplus.plusminus <- lapply(list.est.stderr.diff.AUC.plusplus.plusminus, ReshapeList)
list.est.stderr.diff.AUC.plusplus.minusplus <- lapply(list.est.stderr.diff.AUC.plusplus.minusplus, ReshapeList)
list.est.stderr.diff.AUC.plusplus.minusminus <- lapply(list.est.stderr.diff.AUC.plusplus.minusminus, ReshapeList)
list.est.stderr.diff.AUC.plusminus.minusplus <- lapply(list.est.stderr.diff.AUC.plusminus.minusplus, ReshapeList)
list.est.stderr.diff.AUC.plusminus.minusminus <- lapply(list.est.stderr.diff.AUC.plusminus.minusminus, ReshapeList)
list.est.stderr.diff.AUC.minusminus.minusplus <- lapply(list.est.stderr.diff.AUC.minusminus.minusplus, ReshapeList)

est.stderr.diff.AUC.plusplus.plusminus <- bind_rows(list.est.stderr.diff.AUC.plusplus.plusminus)
est.stderr.diff.AUC.plusplus.minusplus <- bind_rows(list.est.stderr.diff.AUC.plusplus.minusplus)
est.stderr.diff.AUC.plusplus.minusminus <- bind_rows(list.est.stderr.diff.AUC.plusplus.minusminus)
est.stderr.diff.AUC.plusminus.minusplus <- bind_rows(list.est.stderr.diff.AUC.plusminus.minusplus)
est.stderr.diff.AUC.plusminus.minusminus <- bind_rows(list.est.stderr.diff.AUC.plusminus.minusminus)
est.stderr.diff.AUC.minusminus.minusplus <- bind_rows(list.est.stderr.diff.AUC.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate power to reject null hypothesis on pairwise differences in
# end-of-study means between DTRs
# -----------------------------------------------------------------------------
power.diff.eos.means.plusplus.plusminus <- left_join(est.diff.eos.means.plusplus.plusminus,
                                                     est.stderr.diff.eos.means.plusplus.plusminus,
                                                     by = c("datagen.params.N", 
                                                            "datagen.params.rho",
                                                            "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.eos.means.plusplus.minusplus <- left_join(est.diff.eos.means.plusplus.minusplus,
                                                     est.stderr.diff.eos.means.plusplus.minusplus,
                                                     by = c("datagen.params.N", 
                                                            "datagen.params.rho",
                                                            "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.eos.means.plusplus.minusminus <- left_join(est.diff.eos.means.plusplus.minusminus,
                                                      est.stderr.diff.eos.means.plusplus.minusminus,
                                                      by = c("datagen.params.N", 
                                                             "datagen.params.rho",
                                                             "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.eos.means.plusminus.minusplus <- left_join(est.diff.eos.means.plusminus.minusplus,
                                                      est.stderr.diff.eos.means.plusminus.minusplus,
                                                      by = c("datagen.params.N", 
                                                             "datagen.params.rho",
                                                             "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.eos.means.plusminus.minusminus <- left_join(est.diff.eos.means.plusminus.minusminus,
                                                       est.stderr.diff.eos.means.plusminus.minusminus,
                                                       by = c("datagen.params.N", 
                                                              "datagen.params.rho",
                                                              "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.eos.means.minusminus.minusplus <- left_join(est.diff.eos.means.minusminus.minusplus,
                                                       est.stderr.diff.eos.means.minusminus.minusplus,
                                                       by = c("datagen.params.N", 
                                                              "datagen.params.rho",
                                                              "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

list.power.diff.eos.means <- list(power.diff.eos.means.plusplus.plusminus=power.diff.eos.means.plusplus.plusminus,
                                  power.diff.eos.means.plusplus.minusplus=power.diff.eos.means.plusplus.minusplus,
                                  power.diff.eos.means.plusplus.minusminus=power.diff.eos.means.plusplus.minusminus,
                                  power.diff.eos.means.plusminus.minusplus=power.diff.eos.means.plusminus.minusplus,
                                  power.diff.eos.means.plusminus.minusminus=power.diff.eos.means.plusminus.minusminus,
                                  power.diff.eos.means.minusminus.minusplus=power.diff.eos.means.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate power to reject null hypothesis on pairwise differences in AUC 
# between DTRs
# -----------------------------------------------------------------------------
power.diff.AUC.plusplus.plusminus <- left_join(est.diff.AUC.plusplus.plusminus,
                                               est.stderr.diff.AUC.plusplus.plusminus,
                                               by = c("datagen.params.N", 
                                                      "datagen.params.rho",
                                                      "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.AUC.plusplus.minusplus <- left_join(est.diff.AUC.plusplus.minusplus,
                                               est.stderr.diff.AUC.plusplus.minusplus,
                                               by = c("datagen.params.N", 
                                                      "datagen.params.rho",
                                                      "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.AUC.plusplus.minusminus <- left_join(est.diff.AUC.plusplus.minusminus,
                                                est.stderr.diff.AUC.plusplus.minusminus,
                                                by = c("datagen.params.N", 
                                                       "datagen.params.rho",
                                                       "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.AUC.plusminus.minusplus <- left_join(est.diff.AUC.plusminus.minusplus,
                                                est.stderr.diff.AUC.plusminus.minusplus,
                                                by = c("datagen.params.N", 
                                                       "datagen.params.rho",
                                                       "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.AUC.plusminus.minusminus <- left_join(est.diff.AUC.plusminus.minusminus,
                                                 est.stderr.diff.AUC.plusminus.minusminus,
                                                 by = c("datagen.params.N", 
                                                        "datagen.params.rho",
                                                        "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.AUC.minusminus.minusplus <- left_join(est.diff.AUC.minusminus.minusplus,
                                                 est.stderr.diff.AUC.minusminus.minusplus,
                                                 by = c("datagen.params.N", 
                                                        "datagen.params.rho",
                                                        "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

list.power.diff.AUC <- list(power.diff.AUC.plusplus.plusminus=power.diff.AUC.plusplus.plusminus,
                            power.diff.AUC.plusplus.minusplus=power.diff.AUC.plusplus.minusplus,
                            power.diff.AUC.plusplus.minusminus=power.diff.AUC.plusplus.minusminus,
                            power.diff.AUC.plusminus.minusplus=power.diff.AUC.plusminus.minusplus,
                            power.diff.AUC.plusminus.minusminus=power.diff.AUC.plusminus.minusminus,
                            power.diff.AUC.minusminus.minusplus=power.diff.AUC.minusminus.minusplus)


# -----------------------------------------------------------------------------
# Aggregate list of estimates
# -----------------------------------------------------------------------------
power.diff.eos.means <- bind_rows(list.power.diff.eos.means)
power.diff.AUC <- bind_rows(list.power.diff.AUC)
power.diff.eos.means$pair <- 1:6
power.diff.AUC$pair <- 1:6

# -----------------------------------------------------------------------------
# Merge delta and power
# -----------------------------------------------------------------------------
power.diff.eos.means <- left_join(delta.eos.means, power.diff.eos.means,
                                  by = c("datagen.params.N",
                                         "datagen.params.rho",
                                         "pair"))

power.diff.AUC <- left_join(delta.eos.means, power.diff.AUC,
                            by = c("datagen.params.N",
                                   "datagen.params.rho",
                                   "pair"))

