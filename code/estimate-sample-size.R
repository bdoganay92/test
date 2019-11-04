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
# Always keep fixed: Initialize variables
# -----------------------------------------------------------------------------
epsilon <- 0.01
N.initial <- 1000
use.input.N <- N.initial
current.power <- 0
lb <- 0
ub <- Inf
bounded <- FALSE

# -----------------------------------------------------------------------------
# Begin calculations
# -----------------------------------------------------------------------------

if(what.quantity == "AUC"){
  # -----------------------------------------------------------------------------
  # We use the bisection method to find the sample size needed to achieve power
  # to reject H0 to be between (1-type.ii.error) and (1-type.ii.error)+epsilon
  # -----------------------------------------------------------------------------
  collect.power.diff.AUC <- list()
  
  while((current.power<(1-type.ii.error))|(current.power>(1-type.ii.error+epsilon))){
    # Sanity check before power is calculated
    assert_that(lb<=ub)
    
    # Calculate power with current value of input.N
    source(file.path(path.code, "calc-power.R"))
    current.power <- power.diff.AUC[power.diff.AUC$pair==this.pair,"power"]
    current.power <- as.numeric(current.power)
    print(power.diff.AUC)
    power.diff.AUC <- list(power.diff.AUC)
    collect.power.diff.AUC <- append(collect.power.diff.AUC, power.diff.AUC)
    
    if((current.power<(1-type.ii.error)) & (bounded==FALSE)){
      lb <- use.input.N
      use.input.N <- lb+lb/2
      use.input.N <- ceiling(use.input.N)
    }else if((current.power>(1-type.ii.error+epsilon)) & (bounded==FALSE)){
      ub <- use.input.N
      bounded <- TRUE
      use.input.N <- (lb+ub)/2
      use.input.N <- ceiling(use.input.N)
    }else if((current.power<(1-type.ii.error)) & (bounded==TRUE)){
      lb <- use.input.N
      use.input.N <- (lb+ub)/2
      use.input.N <- ceiling(use.input.N)
    }else if((current.power>(1-type.ii.error+epsilon)) & (bounded==TRUE)){
      ub <- use.input.N
      use.input.N <- (lb+ub)/2
      use.input.N <- ceiling(use.input.N)
    }else{
      break
    }
  }
  
  df.collect.power.diff.AUC <- bind_rows(collect.power.diff.AUC)
  df.this.pair <- df.collect.power.diff.AUC %>% 
    filter(pair==this.pair) %>%
    arrange(datagen.params.N)
  
  write.csv(df.this.pair, 
            file.path(path.output_data, "power.diff.AUC.csv"), 
            row.names=FALSE)
  
  # -----------------------------------------------------------------------------
  # Plot sample size vs. power
  # -----------------------------------------------------------------------------
  gg.base <- ggplot(df.this.pair, aes(datagen.params.N,power))
  gg <- gg.base + xlab("N") + ylab("power")
  gg <- gg + scale_x_continuous(limits = c(0,max(df.this.pair$datagen.params.N)+100), 
                                breaks = seq(0, max(df.this.pair$datagen.params.N)+100, 100)) 
  gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  gg <- gg + geom_line(color = "red", linetype=3, size = 1)
  gg <- gg + geom_point(size=3.5, na.rm=TRUE)
  ggsave(file.path(path.output_data,paste("diff.AUC","pair",this.pair,".jpeg", sep="")), 
         width = 7, height = 7, units = "in")
}

if(what.quantity == "eos.means"){
  # -----------------------------------------------------------------------------
  # We use the bisection method to find the sample size needed to achieve power
  # to reject H0 to be between (1-type.ii.error) and (1-type.ii.error)+epsilon
  # -----------------------------------------------------------------------------
  collect.power.diff.eos.means <- list()
  
  while((current.power<(1-type.ii.error))|(current.power>(1-type.ii.error+epsilon))){
    # Sanity check before power is calculated
    assert_that(lb<=ub)
    
    # Calculate power with current value of input.N
    source(file.path(path.code, "calc-power.R"))
    current.power <- power.diff.eos.means[power.diff.eos.means$pair==this.pair,"power"]
    current.power <- as.numeric(current.power)
    print(power.diff.eos.means)
    power.diff.eos.means <- list(power.diff.eos.means)
    collect.power.diff.eos.means <- append(collect.power.diff.eos.means, power.diff.eos.means)
    
    if((current.power<(1-type.ii.error)) & (bounded==FALSE)){
      lb <- input.N
      use.input.N <- lb+lb/2
      use.input.N <- ceiling(use.input.N)
    }else if((current.power>(1-type.ii.error+epsilon)) & (bounded==FALSE)){
      ub <- use.input.N
      bounded <- TRUE
      use.input.N <- (lb+ub)/2
      use.input.N <- ceiling(use.input.N)
    }else if((current.power<(1-type.ii.error)) & (bounded==TRUE)){
      lb <- use.input.N
      use.input.N <- (lb+ub)/2
      use.input.N <- ceiling(use.input.N)
    }else if((current.power>(1-type.ii.error+epsilon)) & (bounded==TRUE)){
      ub <- use.input.N
      use.input.N <- (lb+ub)/2
      use.input.N <- ceiling(use.input.N)
    }else{
      break
    }
  }
  
  df.collect.power.diff.eos.means <- bind_rows(collect.power.diff.eos.means)
  df.this.pair <- df.collect.power.diff.eos.means %>% 
    filter(pair==this.pair) %>%
    arrange(datagen.params.N)
  
  write.csv(df.this.pair, 
            file.path(path.output_data, "power.diff.eos.means.csv"), 
            row.names=FALSE)
  
  # -----------------------------------------------------------------------------
  # Plot sample size vs. power
  # -----------------------------------------------------------------------------
  gg.base <- ggplot(df.this.pair, aes(datagen.params.N,power))
  gg <- gg.base + xlab("N") + ylab("power")
  gg <- gg + scale_x_continuous(limits = c(0,max(df.this.pair$datagen.params.N)+100), 
                                breaks = seq(0, max(df.this.pair$datagen.params.N)+100, 100)) 
  gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  gg <- gg + geom_line(color = "red", linetype=3, size = 1)
  gg <- gg + geom_point(size=3.5, na.rm=TRUE)
  ggsave(file.path(path.output_data,paste("diff.eos.means","pair",this.pair,".jpeg", sep="")), 
         width = 7, height = 7, units = "in")
}


remove(df.this.pair, gg)

