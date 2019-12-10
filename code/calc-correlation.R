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

# Note that 
#   - list.df.potential
# need to be specified prior to running the code below

# -----------------------------------------------------------------------------
# Calculate correlation between time-specific outcomes under each DTR
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "list.df.potential"))
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

list.empirical.corr <- parLapply(cl = cl, 
                                 X = list.df.potential,
                                 fun = DTRCorrelationPO)

stopCluster(cl)

list.empirical.corr <- lapply(list.empirical.corr, 
                              function(x){
                                datagen.params <- x$datagen.params
                                df <- x$estimates
                                row.names(df) <- NULL
                                outlist <- data.frame(datagen.params = datagen.params,
                                                      estimates = df)
                                return(outlist)
                              })

empirical.corr <- bind_rows(list.empirical.corr)
empirical.corr <- empirical.corr %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(hat.tau.ave = mean(tau.ave, na.rm=TRUE)) %>%
  arrange(datagen.params.N, datagen.params.rho)
