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

###############################################################################
# Specify inputs
###############################################################################
input.alpha <- 0.05
input.rand.time <- 2
input.tot.time <- 6
list.input.rho <- list(0.2, 0.5, 0.8)
input.cutoff <- 0
names.seq <- matrix(c("plus.r", "plus.nr.plus", "plus.nr.minus", 
                      "minus.r", "minus.nr.plus", "minus.nr.minus"), 
                    ncol=1, dimnames = list(NULL, "seq"))
this.pair <- 2 # Compare DTR plus.plus vs. DTR minus.plus

###############################################################################
# Create list.input.means data frames where difference in end-of-study means
# or change score between DTRs ++ and -+ is gradually increased. 
# This will be used to calculate power when N is fixed while standardized 
# effect size is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- seq(0,5,0.1)

list.input.means <- list()
for(i in 1:length(increments)){
  k <- increments[i]
  tmpdat <- dat
  tmpdat[tmpdat$seq=="minus.r","time.6"] <- tmpdat[tmpdat$seq=="minus.r","time.6"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] + k
  list.input.means <- append(list.input.means, list(tmpdat))
}

###############################################################################
# Create shortlist.input.means data frames for a fixed choice of 
# difference in end-of-study means or change score between DTRs ++ and -+
# This will be used to calculate power when standardized effect size is fixed
# and N is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- 2.8

shortlist.input.means <- list()
for(i in 1:length(increments)){
  k <- increments[i]
  tmpdat <- dat
  tmpdat[tmpdat$seq=="minus.r","time.6"] <- tmpdat[tmpdat$seq=="minus.r","time.6"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] + k
  shortlist.input.means <- append(shortlist.input.means, list(tmpdat))
}

###############################################################################
# Create auc.list.input.means data frames where difference in AUC
# between DTRs ++ and -+ is gradually increased. This will be used to calculate
# power when N is fixed while standardized effect size is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- seq(0,5,0.1)

auc.list.input.means <- list()
for(i in 1:length(increments)){
  k <- increments[i]
  tmpdat <- dat
  
  tmpdat[tmpdat$seq=="minus.r","time.3"] <- tmpdat[tmpdat$seq=="minus.r","time.3"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.3"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.3"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.4"] <- tmpdat[tmpdat$seq=="minus.r","time.4"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.4"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.4"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.5"] <- tmpdat[tmpdat$seq=="minus.r","time.5"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.5"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.5"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.6"] <- tmpdat[tmpdat$seq=="minus.r","time.6"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] + k
  
  auc.list.input.means <- append(auc.list.input.means, list(tmpdat))
}

###############################################################################
# Create auc.shortlist.input.means data frames for a fixed choice of 
# difference in AUC between DTRs ++ and -+
# This will be used to calculate power when standardized effect size is fixed
# and N is varied
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 1.5)
increments <- 2.8

auc.shortlist.input.means <- list()
for(i in 1:length(increments)){
  k <- increments[i]
  tmpdat <- dat
  
  tmpdat[tmpdat$seq=="minus.r","time.3"] <- tmpdat[tmpdat$seq=="minus.r","time.3"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.3"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.3"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.4"] <- tmpdat[tmpdat$seq=="minus.r","time.4"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.4"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.4"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.5"] <- tmpdat[tmpdat$seq=="minus.r","time.5"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.5"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.5"] + k
  
  tmpdat[tmpdat$seq=="minus.r","time.6"] <- tmpdat[tmpdat$seq=="minus.r","time.6"] + k
  tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] <- tmpdat[tmpdat$seq=="minus.nr.plus","time.6"] + k
  
  auc.shortlist.input.means <- append(auc.shortlist.input.means, list(tmpdat))
}

###############################################################################
# Create input.prop.zeros data frames
###############################################################################
dat <- matrix(rep(NA, 6*(input.tot.time)), byrow=TRUE, ncol=input.tot.time)
colnames(dat) <- paste("time",1:input.tot.time, sep=".")
dat <- data.frame(names.seq, dat)
dat <- replace(dat, is.na(dat), 0.6)
input.prop.zeros <- dat

###############################################################################
# Use working independence correlation structure
###############################################################################
use.working.corr <- "independence"

###############################################################################
# Calculate power: difference in eos means or change score
# =============================================================================
# N is fixed while standardized effect size is varied
###############################################################################
input.N <- 300
collect.power <- list()
collect.delta <- list()
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(list.input.means)){
    input.means <- list.input.means[[j]]
    
    source(file.path(path.code,"calc-power.R"))
    power.diff.eos.means$idx.input.means <- j
    power.diff.change.score$idx.input.means <- j
    tmp.power <- list(eos.means = power.diff.eos.means[power.diff.eos.means$pair==this.pair,],
                      change.score = power.diff.change.score[power.diff.change.score$pair==this.pair,]
    )
    collect.power <- append(collect.power, list(tmp.power))
    source(file.path(path.code,"calc-delta.R"))
    delta.eos.means$idx.input.means <- j
    delta.change.score$idx.input.means <- j
    tmp.delta <- list(eos.means = delta.eos.means[delta.eos.means$pair==this.pair,],
                      change.score =  delta.change.score[delta.change.score$pair==this.pair,]
    )
    collect.delta <- append(collect.delta, list(tmp.delta))
    source(file.path(path.code,"calc-correlation.R"))
    empirical.corr$idx.input.means <- j
    collect.correlation <- append(collect.correlation, list(empirical.corr))
  }
}

# Prepare data frames for plotting
df.collect.power.eos.means <- lapply(collect.power, function(x){return(x$eos.means)})
df.collect.power.change.score <- lapply(collect.power, function(x){return(x$change.score)})

df.collect.power.eos.means <- bind_rows(df.collect.power.eos.means)
df.collect.power.change.score <- bind_rows(df.collect.power.change.score)

df.collect.delta.eos.means <- lapply(collect.delta, function(x){return(x$eos.means)})
df.collect.delta.change.score <- lapply(collect.delta, function(x){return(x$change.score)})

df.collect.delta.eos.means <- bind_rows(df.collect.delta.eos.means)
df.collect.delta.change.score <- bind_rows(df.collect.delta.change.score)

df.collect.eos.means <- left_join(df.collect.delta.eos.means, 
                                  df.collect.power.eos.means, 
                                  by = c("datagen.params.N","datagen.params.rho","pair","idx.input.means"))

df.collect.change.score <- left_join(df.collect.delta.change.score, 
                                     df.collect.power.change.score, 
                                     by = c("datagen.params.N","datagen.params.rho","pair","idx.input.means"))

###############################################################################
# Calculate power: difference in AUC
# =============================================================================
# N is fixed while standardized effect size is varied
###############################################################################
input.N <- 300
collect.power <- list()
collect.delta <- list()
collect.correlation <- list()

for(i in 1:length(list.input.rho)){
  input.rho <- list.input.rho[[i]]
  
  for(j in 1:length(list.input.means)){
    input.means <- auc.list.input.means[[j]]
    
    source(file.path(path.code,"calc-power.R"))
    power.diff.AUC$idx.input.means <- j
    tmp.power <- list(AUC = power.diff.AUC[power.diff.AUC$pair==this.pair,]
    )
    collect.power <- append(collect.power, list(tmp.power))
    source(file.path(path.code,"calc-delta.R"))
    delta.AUC$idx.input.means <- j
    tmp.delta <- list(AUC = delta.AUC[delta.AUC$pair==this.pair,]
    )
    collect.delta <- append(collect.delta, list(tmp.delta))
    source(file.path(path.code,"calc-correlation.R"))
    empirical.corr$idx.input.means <- j
    collect.correlation <- append(collect.correlation, list(empirical.corr))
  }
}

# Prepare data frames for plotting
df.collect.power.AUC <- lapply(collect.power, function(x){return(x$AUC)})
df.collect.power.AUC <- bind_rows(df.collect.power.AUC)
df.collect.delta.AUC <- lapply(collect.delta, function(x){return(x$AUC)})
df.collect.delta.AUC <- bind_rows(df.collect.delta.AUC)
df.collect.AUC <- left_join(df.collect.delta.AUC, 
                            df.collect.power.AUC, 
                            by = c("datagen.params.N","datagen.params.rho","pair","idx.input.means"))

# =============================================================================
# Collate all results when N is fixed and standardized effect size is varied
# =============================================================================
list.all.FIXEDN <- list(df.collect.eos.means = df.collect.eos.means,
                        df.collect.change.score = df.collect.change.score,
                        df.collect.AUC = df.collect.AUC)

###############################################################################
# Save workspace
###############################################################################
save.image(file = file.path(path.output_data, "curve-ind-6M-FIXEDN.RData"))

