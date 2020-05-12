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
  check1 <- try(assert_that(rand.time < tot.time, msg = "rand.time must come before tot.time"), silent=TRUE)
  
  # input.df has correct number of columns
  check2 <- try(assert_that(ncol(input.df)-1==tot.time, msg="Missing columns or incorrect tot.time"), silent=TRUE)
  
  # No missing values permitted in input file
  check3 <- try(assert_that(sum(is.na(input.df))==0, msg = "Missing values in input file"), silent=TRUE)
  
  # Labels of treatment sequences are in correct order
  seq.names <- c("plus.r","plus.nr.plus","plus.nr.minus",
                 "minus.r","minus.nr.plus","minus.nr.minus")
  check.equal <- input.df$seq != seq.names
  check4 <- try(assert_that(sum(check.equal)==0, msg = "Incorrect labels in column 1"), silent=TRUE)
  
  # All quantities at first randomization are equal
  first.rand <- input.df[,"time.1"]
  check5 <- try(assert_that(min(first.rand)==max(first.rand), msg = "All entries must be equal"), silent=TRUE)
  
  # Quantities after first randomization until re-randomization are equal
  pre.rerand.plus <- as.matrix(input.df[1:3,paste("time.",2:rand.time,sep="")])
  check.equal <- apply(pre.rerand.plus,2,min) == apply(pre.rerand.plus,2,max)
  check6 <- try(assert_that(sum(check.equal)==rand.time-1, msg = "All entries must be equal"), silent=TRUE)
  
  # Quantities after first randomization until re-randomization are equal
  pre.rerand.minus <- as.matrix(input.df[4:6,paste("time.",2:rand.time,sep="")])
  check.equal <- apply(pre.rerand.minus,2,min) == apply(pre.rerand.minus,2,max)
  check7 <- try(assert_that(sum(check.equal)==rand.time-1, msg = "All entries must be equal"), silent=TRUE)
  
  if(class(check1)=="try-error"|class(check2)=="try-error"|class(check3)=="try-error"|class(check4)=="try-error"|class(check5)=="try-error"|class(check6)=="try-error"|class(check7)=="try-error"){
    check.list <- list(check1, check2, check3, check4, check5, check6, check7)
    return(check.list)
  }else{
    return(print("All checks on input data passed")) 
  }
}


