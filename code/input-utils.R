library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)

CheckInputData <- function(input.df, rand.time, tot.time){
  
  # Args:
  #   input.df: takes in input.means or input.prop.zeros
  #     * input.means contains mean of time-specific outcomes under each 
  #       treatment sequence from time 1 until tot.time
  #     * input.prop.zeros contains proportion of individuals having their
  #       time-specific outcomes equal to zero for each treatment sequence
  #       from time 1 until tot.time
  #   rand.time: Time when second randomization occurred (time is 1-indexed)
  #   tot.time: Total no. of measurement occasions
  
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
  
  return(print("All checks on input data passed"))
}

ReadInput <- function(input.df, tot.time, rand.time){
  
  # Args:
  #   input.df: data frame containing means or proportions of zeros
  #   tot.time: Total no. of time points
  #   rand.time: Time when second randomization occurred (time is 1-indexed)
  
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







