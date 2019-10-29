library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

# -----------------------------------------------------------------------------
# Parameters held fixed at all times
# -----------------------------------------------------------------------------
idx.nsim <- 1:1000  # Total no. of monte carlo samples
input.cutoff <- 0  # Cutoff in the definition of response status

# -----------------------------------------------------------------------------
# Read and prepare input data
# -----------------------------------------------------------------------------
# input.means contains mean of time-specific outcomes under each 
# treatment sequence from time 1 until tot.time
input.means <- read.csv(file.path(path.input_data, "6-months/input_means.csv"), header = TRUE)
input.prop.zeros <- read.csv(file.path(path.input_data, "6-months/input_prop_zeros.csv"), header = TRUE)
# Check whether input data are in the correct format
CheckInputData(input.df=input.means, rand.time=input.rand.time, tot.time=input.tot.time)
CheckInputData(input.df=input.prop.zeros, rand.time=input.rand.time, tot.time=input.tot.time)

# -----------------------------------------------------------------------------
# Specify data generating parameters
# -----------------------------------------------------------------------------
# Held fixed at all times
input.rho <- seq(0,0.99,0.01)  # Grid of dependence parameters to search over

# Combine all inputs into a grid
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

# -----------------------------------------------------------------------------
# Generate potential outcomes
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

stopCluster(cl)

# -----------------------------------------------------------------------------
# Calculate correlation between time-specific outcomes under each DTR
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

list.empirical.corr <- parLapply(cl = cl, 
                                 X = list.df.potential, 
                                 fun = DTRCorrelationPO)

stopCluster(cl)

list.empirical.corr <- lapply(list.empirical.corr, 
                              function(x){
                                datagen.params <- x$datagen.params
                                df <- x$estimates
                                df$DTR <- row.names(df)
                                row.names(df) <- NULL
                                outlist <- list(datagen.params = datagen.params,
                                                estimates = df)
                                return(outlist)
                              })

by.DTR.empirical.corr <- lapply(list.empirical.corr, 
                                function(x){
                                  datagen.params <- x$datagen.params
                                  estimates <- x$estimates
                                  outdf <- data.frame(datagen.params = datagen.params,
                                                      estimates = estimates)
                                  return(outdf)
                                })

by.DTR.empirical.corr <- bind_rows(by.DTR.empirical.corr)
by.DTR.empirical.corr <- by.DTR.empirical.corr %>% 
  group_by(datagen.params.N, datagen.params.rho, DTR=estimates.DTR) %>%
  summarise(tau.max = mean(estimates.tau.max, na.rm=TRUE),
            tau.min = mean(estimates.tau.min, na.rm=TRUE),
            tau.ave = mean(estimates.tau.ave, na.rm=TRUE)) %>%
  arrange(datagen.params.N, datagen.params.rho, desc(DTR))

empirical.corr <- by.DTR.empirical.corr %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(hat.tau = min(tau.min, na.rm=TRUE)) %>%
  arrange(datagen.params.N, datagen.params.rho)

# -----------------------------------------------------------------------------
# Plot ghat
# -----------------------------------------------------------------------------
gg.base <- ggplot(empirical.corr, aes(datagen.params.rho,hat.tau))
gg <- gg.base + xlab("rho") + ylab("tau_MIN")
gg <- gg + scale_x_continuous(limits = c(-0.2,0.9), breaks = seq(-0.2,1,0.1)) + scale_y_continuous(limits = c(-0.2,0.9), breaks = seq(-0.2,1,0.1))
gg <- gg + geom_abline(slope=1, intercept=0)
gg <- gg + geom_point(size=2, na.rm=TRUE)
ggsave(file.path(path.input_data,"ghat.jpeg"), width = 7, height = 7, units = "in")

# -----------------------------------------------------------------------------
# Determine rho.star
# -----------------------------------------------------------------------------
empirical.corr <- empirical.corr %>% 
  mutate(input.tau.max = input.tau.max) %>%
  mutate(is.greater.than = hat.tau>=input.tau.max)

this.idx <- min(which(empirical.corr$is.greater.than))
rho.star <- empirical.corr[this.idx,"datagen.params.rho"]
rho.star <- as.numeric(rho.star)

remove(input.rho, gridx, list.gridx)