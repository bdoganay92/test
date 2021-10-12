library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)
library(gridExtra)

path.code <- Sys.getenv("path.code")
path.output_data <- Sys.getenv("path.output_data")

source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))
source(file.path(path.code, "geemMod.r"))
environment(geemMod) <- asNamespace("geeM")

# Note that 
#   - input.M
#   - input.rand.time, input.tot.time, 
#   - input.cutoff
#   - input.N
#   - input.n4
#   - input.rho
#   - input.corr.str
#   - input.other.corr.params
#   - input.means, input.prop.zeros
# need to be specified prior to running the code below

# -----------------------------------------------------------------------------
# Combine inputs which may vary from simulation-to-simulation into a grid
# -----------------------------------------------------------------------------
gridx <- expand.grid(nsim=1:input.M, 
                     input.N=input.N,
                     input.rho=input.rho,
                     input.n4=input.n4,
                     input.corr.str=input.corr.str,
                     input.other.corr.params=input.other.corr.params,
                     stringsAsFactors = FALSE)

list.gridx <- list()
for(j in 1:nrow(gridx)){
  curr_list <- list(nsim = gridx[j, "nsim"],
                    input.N = gridx[j, "input.N"],
                    input.rho = gridx[j, "input.rho"],
                    input.n4 = gridx[j, "input.n4"],
                    input.corr.str = gridx[j, "input.corr.str"],
                    input.other.corr.params = gridx[j, "input.other.corr.params"])
  
  list.gridx <- append(list.gridx, list(curr_list))
}

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
                    "path.output_data",
                    "list.gridx",
                    "input.tot.time", "input.rand.time", "input.cutoff"))

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
                                                            tot.time=input.tot.time, 
                                                            rand.time=input.rand.time, 
                                                            cutoff=input.cutoff, 
                                                            rho=this.gridx$input.rho, 
                                                            input.prop.zeros=this.gridx$input.prop.zeros, 
                                                            input.means=this.gridx$input.means,
                                                            input.n4=this.gridx$input.n4,
                                                            corr.str=this.gridx$input.corr.str, 
                                                            other.corr.params=this.gridx$input.other.corr.params)
                                 return(df)
                               })

list.df.observed <- parLapply(cl = cl, 
                              X = list.df.potential, 
                              fun = GenerateObservedYit)

stopCluster(cl)

remove(list.df.potential, list.gridx)

# -----------------------------------------------------------------------------
# Estimate the value of the parameters beta_j in our model for 
# log(E{Y_t^{(a_1,a_2)}})
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 752043)
clusterExport(cl, c("path.code",
                    "path.output_data",
                    "list.df.observed",
                    "input.tot.time",
                    "input.rand.time",
                    "use.working.corr"))
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
               source(file.path(path.code, "geemMod.r"))
               environment(geemMod) <- asNamespace('geeM')
             })

list.df.wr <- parLapply(cl = cl, 
                        X = list.df.observed, 
                        fun = WeightAndReplicate, 
                        tot.time = input.tot.time)

list.df.est.beta <- parLapply(cl = cl, 
                              X = list.df.wr, 
                              fun = AnalyzeData, 
                              tot.time = input.tot.time, 
                              rand.time = input.rand.time,
                              working.corr = "ar1")



stopCluster(cl)

remove(list.df.observed, list.df.wr)


