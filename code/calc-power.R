library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
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
                     input.N=use.input.N,
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
                    "path.output_data",
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
                    "path.output_data",
                    "list.df.est.beta",
                    "list.C",
                    "D.eos.means",
                    "D.AUC",
                    "D.change.score"))
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

list.df.est.diff.change.score <- parLapply(cl = cl, 
                                           X = list.df.est.beta, 
                                           fun = EstimateDiffs, 
                                           D = D.change.score,
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

list.est.diff.change.score.plusplus.plusminus <- lapply(list.df.est.diff.change.score, function(x){return(x["plusplus.plusminus"])})
list.est.diff.change.score.plusplus.minusplus <- lapply(list.df.est.diff.change.score, function(x){return(x["plusplus.minusplus"])})
list.est.diff.change.score.plusplus.minusminus <- lapply(list.df.est.diff.change.score, function(x){return(x["plusplus.minusminus"])})
list.est.diff.change.score.plusminus.minusplus <- lapply(list.df.est.diff.change.score, function(x){return(x["plusminus.minusplus"])})
list.est.diff.change.score.plusminus.minusminus <- lapply(list.df.est.diff.change.score, function(x){return(x["plusminus.minusminus"])})
list.est.diff.change.score.minusminus.minusplus <- lapply(list.df.est.diff.change.score, function(x){return(x["minusminus.minusplus"])})

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

list.est.diff.change.score.plusplus.plusminus <- lapply(list.est.diff.change.score.plusplus.plusminus, ReshapeList)
list.est.diff.change.score.plusplus.minusplus <- lapply(list.est.diff.change.score.plusplus.minusplus, ReshapeList)
list.est.diff.change.score.plusplus.minusminus <- lapply(list.est.diff.change.score.plusplus.minusminus, ReshapeList)
list.est.diff.change.score.plusminus.minusplus <- lapply(list.est.diff.change.score.plusminus.minusplus, ReshapeList)
list.est.diff.change.score.plusminus.minusminus <- lapply(list.est.diff.change.score.plusminus.minusminus, ReshapeList)
list.est.diff.change.score.minusminus.minusplus <- lapply(list.est.diff.change.score.minusminus.minusplus, ReshapeList)

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

est.diff.change.score.plusplus.plusminus <- bind_rows(list.est.diff.change.score.plusplus.plusminus)
est.diff.change.score.plusplus.minusplus <- bind_rows(list.est.diff.change.score.plusplus.minusplus)
est.diff.change.score.plusplus.minusminus <- bind_rows(list.est.diff.change.score.plusplus.minusminus)
est.diff.change.score.plusminus.minusplus <- bind_rows(list.est.diff.change.score.plusminus.minusplus)
est.diff.change.score.plusminus.minusminus <- bind_rows(list.est.diff.change.score.plusminus.minusminus)
est.diff.change.score.minusminus.minusplus <- bind_rows(list.est.diff.change.score.minusminus.minusplus)

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
# Calculate estimates of standard error of pairwise differences in 
# change score between DTRs
# -----------------------------------------------------------------------------
list.est.stderr.diff.change.score <- lapply(list.df.est.beta, EstimateStdErrDiffs, D=D.change.score, list.C=list.C)

list.est.stderr.diff.change.score.plusplus.plusminus <- lapply(list.est.stderr.diff.change.score, function(x){return(x["plusplus.plusminus"])})
list.est.stderr.diff.change.score.plusplus.minusplus <- lapply(list.est.stderr.diff.change.score, function(x){return(x["plusplus.minusplus"])})
list.est.stderr.diff.change.score.plusplus.minusminus <- lapply(list.est.stderr.diff.change.score, function(x){return(x["plusplus.minusminus"])})
list.est.stderr.diff.change.score.plusminus.minusplus <- lapply(list.est.stderr.diff.change.score, function(x){return(x["plusminus.minusplus"])})
list.est.stderr.diff.change.score.plusminus.minusminus <- lapply(list.est.stderr.diff.change.score, function(x){return(x["plusminus.minusminus"])})
list.est.stderr.diff.change.score.minusminus.minusplus <- lapply(list.est.stderr.diff.change.score, function(x){return(x["minusminus.minusplus"])})

list.est.stderr.diff.change.score.plusplus.plusminus <- lapply(list.est.stderr.diff.change.score.plusplus.plusminus, ReshapeList)
list.est.stderr.diff.change.score.plusplus.minusplus <- lapply(list.est.stderr.diff.change.score.plusplus.minusplus, ReshapeList)
list.est.stderr.diff.change.score.plusplus.minusminus <- lapply(list.est.stderr.diff.change.score.plusplus.minusminus, ReshapeList)
list.est.stderr.diff.change.score.plusminus.minusplus <- lapply(list.est.stderr.diff.change.score.plusminus.minusplus, ReshapeList)
list.est.stderr.diff.change.score.plusminus.minusminus <- lapply(list.est.stderr.diff.change.score.plusminus.minusminus, ReshapeList)
list.est.stderr.diff.change.score.minusminus.minusplus <- lapply(list.est.stderr.diff.change.score.minusminus.minusplus, ReshapeList)

est.stderr.diff.change.score.plusplus.plusminus <- bind_rows(list.est.stderr.diff.change.score.plusplus.plusminus)
est.stderr.diff.change.score.plusplus.minusplus <- bind_rows(list.est.stderr.diff.change.score.plusplus.minusplus)
est.stderr.diff.change.score.plusplus.minusminus <- bind_rows(list.est.stderr.diff.change.score.plusplus.minusminus)
est.stderr.diff.change.score.plusminus.minusplus <- bind_rows(list.est.stderr.diff.change.score.plusminus.minusplus)
est.stderr.diff.change.score.plusminus.minusminus <- bind_rows(list.est.stderr.diff.change.score.plusminus.minusminus)
est.stderr.diff.change.score.minusminus.minusplus <- bind_rows(list.est.stderr.diff.change.score.minusminus.minusplus)

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
# Calculate power to reject null hypothesis on pairwise differences in
# change score between DTRs
# -----------------------------------------------------------------------------
power.diff.change.score.plusplus.plusminus <- left_join(est.diff.change.score.plusplus.plusminus,
                                                        est.stderr.diff.change.score.plusplus.plusminus,
                                                        by = c("datagen.params.N", 
                                                               "datagen.params.rho",
                                                               "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.change.score.plusplus.minusplus <- left_join(est.diff.change.score.plusplus.minusplus,
                                                        est.stderr.diff.change.score.plusplus.minusplus,
                                                        by = c("datagen.params.N", 
                                                               "datagen.params.rho",
                                                               "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.change.score.plusplus.minusminus <- left_join(est.diff.change.score.plusplus.minusminus,
                                                         est.stderr.diff.change.score.plusplus.minusminus,
                                                         by = c("datagen.params.N", 
                                                                "datagen.params.rho",
                                                                "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.change.score.plusminus.minusplus <- left_join(est.diff.change.score.plusminus.minusplus,
                                                         est.stderr.diff.change.score.plusminus.minusplus,
                                                         by = c("datagen.params.N", 
                                                                "datagen.params.rho",
                                                                "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.change.score.plusminus.minusminus <- left_join(est.diff.change.score.plusminus.minusminus,
                                                          est.stderr.diff.change.score.plusminus.minusminus,
                                                          by = c("datagen.params.N", 
                                                                 "datagen.params.rho",
                                                                 "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

power.diff.change.score.minusminus.minusplus <- left_join(est.diff.change.score.minusminus.minusplus,
                                                          est.stderr.diff.change.score.minusminus.minusplus,
                                                          by = c("datagen.params.N", 
                                                                 "datagen.params.rho",
                                                                 "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

list.power.diff.change.score <- list(power.diff.change.score.plusplus.plusminus=power.diff.change.score.plusplus.plusminus,
                                     power.diff.change.score.plusplus.minusplus=power.diff.change.score.plusplus.minusplus,
                                     power.diff.change.score.plusplus.minusminus=power.diff.change.score.plusplus.minusminus,
                                     power.diff.change.score.plusminus.minusplus=power.diff.change.score.plusminus.minusplus,
                                     power.diff.change.score.plusminus.minusminus=power.diff.change.score.plusminus.minusminus,
                                     power.diff.change.score.minusminus.minusplus=power.diff.change.score.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Aggregate list of estimates
# -----------------------------------------------------------------------------
for(i in 1:length(list.power.diff.eos.means)){
  list.power.diff.eos.means[[i]]$pair <- i
}

for(i in 1:length(list.power.diff.AUC)){
  list.power.diff.AUC[[i]]$pair <- i
}

for(i in 1:length(list.power.diff.change.score)){
  list.power.diff.change.score[[i]]$pair <- i
}

power.diff.eos.means <- bind_rows(list.power.diff.eos.means)
power.diff.AUC <- bind_rows(list.power.diff.AUC)
power.diff.change.score <- bind_rows(list.power.diff.change.score)

print(power.diff.eos.means)
print(power.diff.change.score)

# -----------------------------------------------------------------------------
# Set up data frames for merging below
# -----------------------------------------------------------------------------
#delta.eos.means <- delta.eos.means[, !(colnames(delta.eos.means) %in% "datagen.params.N")]
#delta.AUC <- delta.AUC[, !(colnames(delta.AUC) %in% "datagen.params.N")]

# -----------------------------------------------------------------------------
# Merge delta and power
# -----------------------------------------------------------------------------
#power.diff.eos.means <- left_join(delta.eos.means, power.diff.eos.means,
#                                  by = c("datagen.params.rho",
#                                         "pair"))

#power.diff.AUC <- left_join(delta.AUC, power.diff.AUC,
#                            by = c("datagen.params.rho",
#                                   "pair"))


