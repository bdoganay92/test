library(dplyr)
library(assertthat)  # All functions will require assertthat
library(rootSolve)
library(mvtnorm)

ReadInput <- function(input.df, tot.time, rand.time){
  
  # Args:
  #   input.df: data frame containing means or proportions of zeros
  #   tot.time: Total no. of time points
  #   rand.time: Time when second randomization occurred (time is 1-indexed)
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Sanity check tot.time and rand.time
  assert_that(rand.time < tot.time, msg = "rand.time must come before tot.time")
  
  # input.df has correct number of columns
  assert_that(ncol(input.df)-1==tot.time, msg="Missing columns or incorrect tot.time")
  
  # No missing values permitted in input file
  assert_that(sum(is.na(input.df))==0, msg = "Missing values in input file")
  
  # Labels of treatment sequences are in correct order
  seq.names <- c("plus.r","plus.nr.plus","plus.nr.minus",
                 "minus.r","minus.nr.plus","minus.nr.minus")
  check.equal <- input.df$seq != seq.names
  assert_that(sum(check.equal)==0, msg = "Incorrect labels in column 1")
  
  # All quantities at first randomization are equal
  first.rand <- input.df[,"time.1"]
  assert_that(min(first.rand)==max(first.rand), msg = "All entries must be equal")
  
  # Quantities after first randomization until re-randomization are equal
  pre.rerand.plus <- as.matrix(input.df[1:3,paste("time.",2:rand.time,sep="")])
  check.equal <- apply(pre.rerand.plus,2,min) == apply(pre.rerand.plus,2,max)
  assert_that(sum(check.equal)==rand.time-1, msg = "All entries must be equal")
  
  # Quantities after first randomization until re-randomization are equal
  pre.rerand.minus <- as.matrix(input.df[4:6,paste("time.",2:rand.time,sep="")])
  check.equal <- apply(pre.rerand.minus,2,min) == apply(pre.rerand.minus,2,max)
  assert_that(sum(check.equal)==rand.time-1, msg = "All entries must be equal")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  initial <- input.df[1,"time.1"]
  plus <- input.df[1,paste("time.",2:rand.time,sep="")]
  minus <- input.df[4,paste("time.",2:rand.time,sep="")]
  plus.r <- input.df[1,paste("time.",(rand.time+1):tot.time,sep="")]
  plus.nr.plus <- input.df[2,paste("time.",(rand.time+1):tot.time,sep="")]
  plus.nr.minus <- input.df[3,paste("time.",(rand.time+1):tot.time,sep="")]
  minus.r <- input.df[4,paste("time.",(rand.time+1):tot.time,sep="")]
  minus.nr.plus <- input.df[5,paste("time.",(rand.time+1):tot.time,sep="")]
  minus.nr.minus <- input.df[6,paste("time.",(rand.time+1):tot.time,sep="")]
  
  group.01 <- list(initial = initial,
                   plus = plus,
                   minus = minus,
                   plus.r = plus.r,
                   minus.r = minus.r)
  
  group.02 <- list(initial = initial,
                   plus = plus,
                   minus = minus,
                   plus.r = plus.r,
                   minus.nr.plus = minus.nr.plus,
                   minus.nr.minus = minus.nr.minus)
  
  group.03 <- list(initial = initial,
                   plus = plus,
                   minus = minus,
                   minus.r = minus.r,
                   plus.nr.plus = plus.nr.plus,
                   plus.nr.minus = plus.nr.minus)
  
  group.04 <- list(initial = initial,
                   plus = plus,
                   minus = minus,
                   plus.nr.plus = plus.nr.plus,
                   plus.nr.minus = plus.nr.minus,
                   minus.nr.plus = minus.nr.plus,
                   minus.nr.minus = minus.nr.minus)
  
  group.01 <- lapply(group.01, as.numeric)
  group.02 <- lapply(group.02, as.numeric)
  group.03 <- lapply(group.03, as.numeric)
  group.04 <- lapply(group.04, as.numeric)
  
  all.groups <- list(group.01 = group.01,
                     group.02 = group.02,
                     group.03 = group.03,
                     group.04 = group.04)
  
  return(all.groups)
}

GetVariance <- function(input.mu, input.prop.zeros){
  
  # Requires library: rootSolve
  # Args:
  #   input.mu: Mean of a negative binomial distributed random variable
  #   input.prop.zeros: Proportion of zeros of a negative binomial
  #                     distributed random variable
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(is.scalar(input.mu), msg = "input.mu must be a scalar")
  assert_that(is.scalar(input.prop.zeros), msg = "input.prop.zeros must be a scalar")
  assert_that(input.mu>0, msg = "input.mu must be positive")
  assert_that((input.prop.zeros>=0) & (input.prop.zeros<=1), 
              msg = "input.prop.zeros must be between 0 and 1")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  k <- multiroot(f = function(x, prop.zeros, mu){
    ans <- prop.zeros - (x/(mu+x))^x
    return(ans)
  }, start = 0, prop.zeros = input.prop.zeros, mu = input.mu)
  
  sigma2 <- 1/k$root
  
  return(sigma2)
}

GetVarianceByGroup <- function(all.mu, all.tau){
  
  # Requires library: rootSolve
  # Args:
  #   all.mu: list of means of negative binomial distributed random variables
  #   all.prop.zeros: list of proportion of zeros of a negative binomial
  #                   distributed random variables
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(length(all.mu) == length(all.tau), 
              msg = "number of means and proportion of zeros must be equal")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------  
  all.sigma2 <- list()
  
  for(i in 1:length(all.mu)){
    nam <- names(all.mu[i])
    these.mu <- unlist(all.mu[i])
    these.tau <- unlist(all.tau[i])
    these.sigma2 <- list()
    
    for(j in 1:length(these.mu)){
      this.sigma2 <- GetVariance(input.mu = these.mu[j], input.prop.zeros = these.tau[j])
      these.sigma2 <- append(these.sigma2, list(this.sigma2))
    }
    
    these.sigma2 <- unlist(these.sigma2)
    these.sigma2 <- list(these.sigma2)
    names(these.sigma2) <- nam
    all.sigma2 <- append(all.sigma2, these.sigma2)
  }
  
  return(all.sigma2)
}



