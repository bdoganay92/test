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
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

# -----------------------------------------------------------------------------
# Parameters held fixed at all times in calibrate-params.R
# -----------------------------------------------------------------------------
idx.nsim <- 1:1000  # Total no. of monte carlo samples
input.cutoff <- 0  # Cutoff in the definition of response status

# -----------------------------------------------------------------------------
# Specify data generating parameters
# -----------------------------------------------------------------------------
# Held fixed at all times
input.rho <- seq(0,0.99,0.05)  # Grid of dependence parameters to search over

# Combine all inputs into a grid
gridx <- expand.grid(nsim=idx.nsim, 
                     input.N=1000,
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
                                row.names(df) <- NULL
                                outlist <- data.frame(datagen.params = datagen.params,
                                                      estimates = df)
                                return(outlist)
                              })

empirical.corr <- bind_rows(list.empirical.corr)
empirical.corr <- empirical.corr %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(hat.tau = mean(tau.ave, na.rm=TRUE)) %>%
  arrange(datagen.params.N, datagen.params.rho)

# -----------------------------------------------------------------------------
# Plot ghat
# -----------------------------------------------------------------------------
gg.base <- ggplot(empirical.corr, aes(datagen.params.rho,hat.tau))
gg <- gg.base + xlab("rho") + ylab("tau_MEAN")
gg <- gg + scale_x_continuous(limits = c(-0.2,0.9), breaks = seq(-0.2,1,0.1)) + scale_y_continuous(limits = c(-0.2,0.9), breaks = seq(-0.2,1,0.1))
gg <- gg + geom_abline(slope=1, intercept=0)
gg <- gg + geom_point(size=3.5, na.rm=TRUE)
ggsave(file.path(path.output_data,"ghat.jpeg"), width = 7, height = 7, units = "in")

# -----------------------------------------------------------------------------
# Determine rho.star
# -----------------------------------------------------------------------------
empirical.corr <- empirical.corr %>% 
  mutate(input.tau.max = input.tau.max) %>%
  mutate(is.greater.than = hat.tau>=input.tau.max)

this.idx <- min(which(empirical.corr$is.greater.than))
rho.star <- empirical.corr[this.idx,"datagen.params.rho"]
rho.star <- as.numeric(rho.star)

write.csv(empirical.corr, file.path(path.output_data,"empirical.corr.csv"), row.names=FALSE)

remove(input.rho, gridx, list.gridx)

