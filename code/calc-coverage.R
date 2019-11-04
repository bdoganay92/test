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
# Calculate standard error of pairwise differences in end of study means
# between DTRs
# -----------------------------------------------------------------------------
stderr.diff.eos.means.plusplus.plusminus <- bind_rows(list.est.diff.eos.means.plusplus.plusminus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.eos.means.plusplus.minusplus <- bind_rows(list.est.diff.eos.means.plusplus.minusplus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.eos.means.plusplus.minusminus <- bind_rows(list.est.diff.eos.means.plusplus.minusminus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.eos.means.plusminus.minusplus <- bind_rows(list.est.diff.eos.means.plusminus.minusplus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.eos.means.plusminus.minusminus <- bind_rows(list.est.diff.eos.means.plusminus.minusminus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.eos.means.minusminus.minusplus <- bind_rows(list.est.diff.eos.means.minusminus.minusplus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

# -----------------------------------------------------------------------------
# Calculate standard error of pairwise differences in AUC between DTRs
# -----------------------------------------------------------------------------
stderr.diff.AUC.plusplus.plusminus <- bind_rows(list.est.diff.AUC.plusplus.plusminus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.AUC.plusplus.minusplus <- bind_rows(list.est.diff.AUC.plusplus.minusplus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.AUC.plusplus.minusminus <- bind_rows(list.est.diff.AUC.plusplus.minusminus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.AUC.plusminus.minusplus <- bind_rows(list.est.diff.AUC.plusminus.minusplus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.AUC.plusminus.minusminus <- bind_rows(list.est.diff.AUC.plusminus.minusminus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

stderr.diff.AUC.minusminus.minusplus <- bind_rows(list.est.diff.AUC.minusminus.minusplus) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(stderr=sd(estimates, na.rm=TRUE))

# -----------------------------------------------------------------------------
# Calculate bias in estimates of standard error of pairwise differences in 
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

bias.est.stderr.diff.eos.means.plusplus.plusminus <- left_join(est.stderr.diff.eos.means.plusplus.plusminus, 
                                                               stderr.diff.eos.means.plusplus.plusminus,
                                                               by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.eos.means.plusplus.minusplus <- left_join(est.stderr.diff.eos.means.plusplus.minusplus, 
                                                               stderr.diff.eos.means.plusplus.minusplus,
                                                               by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.eos.means.plusplus.minusminus <- left_join(est.stderr.diff.eos.means.plusplus.minusminus, 
                                                                stderr.diff.eos.means.plusplus.minusminus,
                                                                by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.eos.means.plusminus.minusplus <- left_join(est.stderr.diff.eos.means.plusminus.minusplus, 
                                                                stderr.diff.eos.means.plusminus.minusplus,
                                                                by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.eos.means.plusminus.minusminus <- left_join(est.stderr.diff.eos.means.plusminus.minusminus, 
                                                                 stderr.diff.eos.means.plusminus.minusminus,
                                                                 by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.eos.means.minusminus.minusplus <- left_join(est.stderr.diff.eos.means.minusminus.minusplus, 
                                                                 stderr.diff.eos.means.minusminus.minusplus,
                                                                 by = c("datagen.params.N", "datagen.params.rho"))

bias.est.stderr.diff.eos.means.plusplus.plusminus <- bias.est.stderr.diff.eos.means.plusplus.plusminus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.eos.means.plusplus.minusplus <- bias.est.stderr.diff.eos.means.plusplus.minusplus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.eos.means.plusplus.minusminus <- bias.est.stderr.diff.eos.means.plusplus.minusminus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.eos.means.plusminus.minusplus <- bias.est.stderr.diff.eos.means.plusminus.minusplus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.eos.means.plusminus.minusminus <- bias.est.stderr.diff.eos.means.plusminus.minusminus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.eos.means.minusminus.minusplus <- bias.est.stderr.diff.eos.means.minusminus.minusplus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

list.bias.est.stderr.diff.eos.means <- list(bias.est.stderr.diff.eos.means.plusplus.plusminus=bias.est.stderr.diff.eos.means.plusplus.plusminus,
                                            bias.est.stderr.diff.eos.means.plusplus.minusplus=bias.est.stderr.diff.eos.means.plusplus.minusplus,
                                            bias.est.stderr.diff.eos.means.plusplus.minusminus=bias.est.stderr.diff.eos.means.plusplus.minusminus,
                                            bias.est.stderr.diff.eos.means.plusminus.minusplus=bias.est.stderr.diff.eos.means.plusminus.minusplus,
                                            bias.est.stderr.diff.eos.means.plusminus.minusminus=bias.est.stderr.diff.eos.means.plusminus.minusminus,
                                            bias.est.stderr.diff.eos.means.minusminus.minusplus=bias.est.stderr.diff.eos.means.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate bias in estimates of standard error of pairwise differences in 
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

bias.est.stderr.diff.AUC.plusplus.plusminus <- left_join(est.stderr.diff.AUC.plusplus.plusminus, 
                                                         stderr.diff.AUC.plusplus.plusminus,
                                                         by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.AUC.plusplus.minusplus <- left_join(est.stderr.diff.AUC.plusplus.minusplus, 
                                                         stderr.diff.AUC.plusplus.minusplus,
                                                         by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.AUC.plusplus.minusminus <- left_join(est.stderr.diff.AUC.plusplus.minusminus, 
                                                          stderr.diff.AUC.plusplus.minusminus,
                                                          by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.AUC.plusminus.minusplus <- left_join(est.stderr.diff.AUC.plusminus.minusplus, 
                                                          stderr.diff.AUC.plusminus.minusplus,
                                                          by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.AUC.plusminus.minusminus <- left_join(est.stderr.diff.AUC.plusminus.minusminus, 
                                                           stderr.diff.AUC.plusminus.minusminus,
                                                           by = c("datagen.params.N", "datagen.params.rho"))
bias.est.stderr.diff.AUC.minusminus.minusplus <- left_join(est.stderr.diff.AUC.minusminus.minusplus, 
                                                           stderr.diff.AUC.minusminus.minusplus,
                                                           by = c("datagen.params.N", "datagen.params.rho"))

bias.est.stderr.diff.AUC.plusplus.plusminus <- bias.est.stderr.diff.AUC.plusplus.plusminus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.AUC.plusplus.minusplus <- bias.est.stderr.diff.AUC.plusplus.minusplus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.AUC.plusplus.minusminus <- bias.est.stderr.diff.AUC.plusplus.minusminus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.AUC.plusminus.minusplus <- bias.est.stderr.diff.AUC.plusminus.minusplus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.AUC.plusminus.minusminus <- bias.est.stderr.diff.AUC.plusminus.minusminus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

bias.est.stderr.diff.AUC.minusminus.minusplus <- bias.est.stderr.diff.AUC.minusminus.minusplus %>% 
  mutate(bias = estimates - stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(est.bias = mean(bias, na.rm=TRUE))

list.bias.est.stderr.diff.AUC <- list(bias.est.stderr.diff.AUC.plusplus.plusminus=bias.est.stderr.diff.AUC.plusplus.plusminus,
                                      bias.est.stderr.diff.AUC.plusplus.minusplus=bias.est.stderr.diff.AUC.plusplus.minusplus,
                                      bias.est.stderr.diff.AUC.plusplus.minusminus=bias.est.stderr.diff.AUC.plusplus.minusminus,
                                      bias.est.stderr.diff.AUC.plusminus.minusplus=bias.est.stderr.diff.AUC.plusminus.minusplus,
                                      bias.est.stderr.diff.AUC.plusminus.minusminus=bias.est.stderr.diff.AUC.plusminus.minusminus,
                                      bias.est.stderr.diff.AUC.minusminus.minusplus=bias.est.stderr.diff.AUC.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate coverage of estimates of pairwise differences in end-of-study means
# between DTRs
# -----------------------------------------------------------------------------
coverage.diff.eos.means.plusplus.plusminus <- left_join(est.diff.eos.means.plusplus.plusminus,
                                                        est.stderr.diff.eos.means.plusplus.plusminus,
                                                        by = c("datagen.params.N", 
                                                               "datagen.params.rho",
                                                               "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusplus.plusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusplus.minusplus <- left_join(est.diff.eos.means.plusplus.minusplus,
                                                        est.stderr.diff.eos.means.plusplus.minusplus,
                                                        by = c("datagen.params.N", 
                                                               "datagen.params.rho",
                                                               "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusplus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusplus.minusminus <- left_join(est.diff.eos.means.plusplus.minusminus,
                                                         est.stderr.diff.eos.means.plusplus.minusminus,
                                                         by = c("datagen.params.N", 
                                                                "datagen.params.rho",
                                                                "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusplus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusminus.minusplus <- left_join(est.diff.eos.means.plusminus.minusplus,
                                                         est.stderr.diff.eos.means.plusminus.minusplus,
                                                         by = c("datagen.params.N", 
                                                                "datagen.params.rho",
                                                                "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusminus.minusminus <- left_join(est.diff.eos.means.plusminus.minusminus,
                                                          est.stderr.diff.eos.means.plusminus.minusminus,
                                                          by = c("datagen.params.N", 
                                                                 "datagen.params.rho",
                                                                 "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusminus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.minusminus.minusplus <- left_join(est.diff.eos.means.minusminus.minusplus,
                                                          est.stderr.diff.eos.means.minusminus.minusplus,
                                                          by = c("datagen.params.N", 
                                                                 "datagen.params.rho",
                                                                 "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.minusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

list.coverage.diff.eos.means <- list(coverage.diff.eos.means.plusplus.plusminus=coverage.diff.eos.means.plusplus.plusminus,
                                     coverage.diff.eos.means.plusplus.minusplus=coverage.diff.eos.means.plusplus.minusplus,
                                     coverage.diff.eos.means.plusplus.minusminus=coverage.diff.eos.means.plusplus.minusminus,
                                     coverage.diff.eos.means.plusminus.minusplus=coverage.diff.eos.means.plusminus.minusplus,
                                     coverage.diff.eos.means.plusminus.minusminus=coverage.diff.eos.means.plusminus.minusminus,
                                     coverage.diff.eos.means.minusminus.minusplus=coverage.diff.eos.means.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate coverage of estimates of pairwise differences in AUC between DTRs
# -----------------------------------------------------------------------------
coverage.diff.AUC.plusplus.plusminus <- left_join(est.diff.AUC.plusplus.plusminus,
                                                  est.stderr.diff.AUC.plusplus.plusminus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusplus.plusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusplus.minusplus <- left_join(est.diff.AUC.plusplus.minusplus,
                                                  est.stderr.diff.AUC.plusplus.minusplus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusplus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusplus.minusminus <- left_join(est.diff.AUC.plusplus.minusminus,
                                                  est.stderr.diff.AUC.plusplus.minusminus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusplus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusminus.minusplus <- left_join(est.diff.AUC.plusminus.minusplus,
                                                  est.stderr.diff.AUC.plusminus.minusplus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusminus.minusminus <- left_join(est.diff.AUC.plusminus.minusminus,
                                                  est.stderr.diff.AUC.plusminus.minusminus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusminus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.minusminus.minusplus <- left_join(est.diff.AUC.minusminus.minusplus,
                                                  est.stderr.diff.AUC.minusminus.minusplus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.minusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

list.coverage.diff.AUC <- list(coverage.diff.AUC.plusplus.plusminus=coverage.diff.AUC.plusplus.plusminus,
                               coverage.diff.AUC.plusplus.minusplus=coverage.diff.AUC.plusplus.minusplus,
                               coverage.diff.AUC.plusplus.minusminus=coverage.diff.AUC.plusplus.minusminus,
                               coverage.diff.AUC.plusminus.minusplus=coverage.diff.AUC.plusminus.minusplus,
                               coverage.diff.AUC.plusminus.minusminus=coverage.diff.AUC.plusminus.minusminus,
                               coverage.diff.AUC.minusminus.minusplus=coverage.diff.AUC.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Aggregate results
# -----------------------------------------------------------------------------
list.all.coverage <- list(bias.est.stderr.diff.eos.means=list(list.bias.est.stderr.diff.eos.means),
                          bias.est.stderr.diff.AUC=list(list.bias.est.stderr.diff.AUC),
                          coverage.diff.eos.means = list(list.coverage.diff.eos.means),
                          coverage.diff.AUC = list(list.coverage.diff.AUC)
                          )



