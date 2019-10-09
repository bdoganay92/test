library(dplyr)
library(assertthat)  # All functions will require assertthat
library(rootSolve)
library(mvtnorm)

IdentityMat <- function(m){
  
  # Args:
  #   m: number of rows
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(is.scalar(m), msg = "m must be a scalar")
  assert_that((m>0) & (as.integer(m)==m), msg = "m must be positive integer")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  return(diag(m))
}

IdentityCol <- function(m){
  
  # Args:
  #   m: number of rows
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(is.scalar(m), msg = "m must be a scalar")
  assert_that((m>0) & (as.integer(m)==m), msg = "m must be positive integer")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  return(as.matrix(rep(1,m)))
}

ZeroCol <- function(m){
  
  # Args:
  #   m: number of rows
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(is.scalar(m), msg = "m must be a scalar")
  assert_that((m>0) & (as.integer(m)==m), msg = "m must be positive integer")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  return(as.matrix(rep(0,m)))
}

ExchangeableMat <- function(m, rho){
  
  # Args:
  #   m: number of rows
  #   rho: correlation between any two given quantities
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(is.scalar(m), msg = "m must be a scalar")
  assert_that((m>0) & (as.integer(m)==m), msg = "m must be a positive integer")
  assert_that((rho>=0) & (rho<=1), msg = "rho must be between 0 and 1")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  mat <- IdentityMat(m) + rho*(IdentityCol(m)%*%t(IdentityCol(m)) - IdentityMat(m))
  return(mat)
}

ByGroupGenerateU <- function(n.group, corrdim, corrmat){
  
  # Requires mvtnorm
  #
  # Args:
  #   n.group: no. of individuals in group
  #   corrdim: no. of rows in correlation matrix
  #   corrmat: correlation matrix associated with group
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that((n.group>=0) & (as.integer(n.group)==n.group), 
              msg = "n.group must be a positive integer or zero")
  assert_that((corrdim == ncol(corrmat)) & (corrdim == nrow(corrmat)), 
              msg = "corrdim must be consistent with corrmat")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # --------------------------------------------------------------------------- 
  
  if(n.group == 0){
    U <- data.frame(NULL)
  }else{
    Z <- rmvnorm(n = n.group, 
                 mean=as.matrix(rep(0, corrdim)), 
                 sigma = corrmat, 
                 method = "svd") 
    U <- pnorm(Z)
    U <- t(U) 
  }
  
  return(U)
}

ByGroupMatrixToList <- function(mat, group, rand.time, tot.time){
  
  # Args:
  #   mat: U matrix for a given group
  #   group: group number; options are (1, 2, 3, 4)
  #   rand.time: Time when second randomization occurred (time is 1-indexed)
  #   tot.time: Total no. of time points
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(group==1|group==2|group==3|group==4, 
              msg="Valid options for group: 1,2,3,4")
  assert_that(rand.time < tot.time, 
              msg = "Rerandomization must occur before total time")
  
  corrdim.01 <- 2*tot.time - 1
  corrdim.02 <- 3*tot.time - rand.time - 1
  corrdim.03 <- 3*tot.time - rand.time - 1
  corrdim.04 <- 4*tot.time - 2*rand.time - 1
  
  if(group==1){
    corrdim <- corrdim.01
  }else if(group==2){
    corrdim <- corrdim.02
  }else if(group==3){
    corrdim <- corrdim.03
  }else{
    corrdim <- corrdim.04
  }
  
  assert_that((nrow(mat)==0)|(nrow(mat)==corrdim), 
              msg="If mat has rows, no. of rows of mat must equal total time")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # --------------------------------------------------------------------------- 
  
  if(nrow(mat)==0){
    newlist <- data.frame(NULL)
  }else{
    if(group==1){
      mat.initial <- mat[1,]
      mat.plus <- mat[2:rand.time,]
      mat.minus <- mat[(rand.time+1):(2*rand.time-1),]
      mat.plus.r <- mat[(2*rand.time):(tot.time+rand.time-1),]
      mat.minus.r <- mat[(tot.time+rand.time):(2*tot.time-1),]
      
      newlist <- list(initial = mat.initial,
                      plus = mat.plus,
                      minus = mat.minus,
                      plus.r = mat.plus.r,
                      minus.r = mat.minus.r)
      
    }else if(group==2){
      mat.initial <- mat[1,]
      mat.plus <- mat[2:rand.time,]
      mat.minus <- mat[(rand.time+1):(2*rand.time-1),]
      mat.plus.r <- mat[(2*rand.time):(tot.time+rand.time-1),]
      mat.minus.nr.plus <- mat[(tot.time+rand.time):(2*tot.time-1),]
      mat.minus.nr.minus <- mat[(2*tot.time):(3*tot.time-rand.time-1),]
      
      newlist <- list(initial = mat.initial,
                      plus = mat.plus,
                      minus = mat.minus,
                      plus.r = mat.plus.r,
                      minus.nr.plus = mat.minus.nr.plus,
                      minus.nr.minus = mat.minus.nr.minus)
      
    }else if(group==3){
      mat.initial <- mat[1,]
      mat.plus <- mat[2:rand.time,]
      mat.minus <- mat[(rand.time+1):(2*rand.time-1),]
      mat.plus.nr.plus <- mat[(2*rand.time):(tot.time+rand.time-1),]
      mat.plus.nr.minus <- mat[(tot.time+rand.time):(2*tot.time-1),]
      mat.minus.r <- mat[(2*tot.time):(3*tot.time-rand.time-1),]
      
      newlist <- list(initial = mat.initial,
                      plus = mat.plus,
                      minus = mat.minus,
                      plus.nr.plus = mat.plus.nr.plus,
                      plus.nr.minus = mat.plus.nr.minus,
                      minus.r = mat.minus.r)
      
    }else{ # group==4
      mat.initial <- mat[1,]
      mat.plus <- mat[2:rand.time,]
      mat.minus <- mat[(rand.time+1):(2*rand.time-1),]
      mat.plus.nr.plus <- mat[(2*rand.time):(tot.time+rand.time-1),]
      mat.plus.nr.minus <- mat[(tot.time+rand.time):(2*tot.time-1),]
      mat.minus.nr.plus <- mat[(2*tot.time):(3*tot.time-rand.time-1),]
      mat.minus.nr.minus <- mat[(3*tot.time-rand.time):(4*tot.time-2*rand.time-1),]
      
      newlist <- list(initial = mat.initial,
                      plus = mat.plus,
                      minus = mat.minus,
                      plus.nr.plus = mat.plus.nr.plus,
                      plus.nr.minus = mat.plus.nr.minus,
                      minus.nr.plus = mat.minus.nr.plus,
                      minus.nr.minus = mat.minus.nr.minus)
    }
    
    newlist <- lapply(newlist, function(x){
      
      if(!is.matrix(x)){
        x <- t(matrix(x))
      }
      
      return(x)
    })
    
  }
  
  return(newlist)
}

qUpperTruncNB <- function(p, lambda, sigma2, cutoff){
  
  # Args:
  #   p: quantile
  #   lambda: mean of un-truncated negative binomial distributed random variable
  #   sigma2: parameter in variance of un-truncated negative binomial 
  #           distributed random variable
  #   cutoff: cutoff in definition of response status
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(p>=0 & p<=1)
  assert_that(lambda>0)
  assert_that(sigma2>0)
  assert_that(cutoff>0)
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # --------------------------------------------------------------------------- 
  
  if(p==1){
    return(cutoff)
  }else{
    check <- 0
    q <- 0
    while(check == 0){
      y <- 0:q
      num <- dnbinom(x = y, mu = lambda, size = 1/sigma2)*(1*(y <= cutoff))
      denom <- pnbinom(q = cutoff, mu = lambda, size = 1/sigma2)
      probability <- num/denom
      check <- (sum(probability) >= p)
      q <- q+1
    }
    quantile <- q-1
    return(quantile)
  }
}

qLowerTruncNB <- function(p, lambda, sigma2, cutoff){
  
  # Args:
  #   p: quantile
  #   lambda: mean of un-truncated negative binomial distributed random variable
  #   sigma2: parameter in variance of un-truncated negative binomial 
  #           distributed random variable
  #   cutoff: cutoff in definition of response status
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(p>=0 & p<=1)
  assert_that(lambda>0)
  assert_that(sigma2>0)
  assert_that(cutoff>0)
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # --------------------------------------------------------------------------- 
  
  if(p == 0){
    return(cutoff+1)
  }else{
    check <- 0
    q <- 0
    while(check == 0){
      y <- 0:q
      num <- dnbinom(x = y, mu = lambda, size = 1/sigma2)*(1*(y > cutoff))
      denom <- 1-pnbinom(q = cutoff, mu = lambda, size = 1/sigma2)
      probability <- num/denom
      check <- (sum(probability) >= p)
      q <- q+1
    }
    quantile <- q-1
    return(quantile) 
  }
}

ByGroupGenerateY <- function(list.U, group, list.mu, list.sigma2, cutoff){
  
  if(group==1 & length(unlist(list.U$group.01))>0){
    
    # At first randomization
    Y.initial <- qnbinom(list.U$group.01$initial, mu = list.mu$group.01$initial, size=1/list.sigma2$group.01$initial)
    
    # After first randomization until second randomization
    Y.plus <- qnbinom(list.U$group.01$plus, mu = list.mu$group.01$plus, size=1/list.sigma2$group.01$plus)
    Y.minus <- qnbinom(list.U$group.01$minus, mu = list.mu$group.01$minus, size=1/list.sigma2$group.01$minus)
    
    Y.plus[nrow(Y.plus),] <- apply(as.matrix(list.U$group.01$plus[nrow(Y.plus),]), 1, 
                                   qUpperTruncNB, 
                                   lambda = list.mu$group.01$plus[nrow(Y.plus)], 
                                   sigma2 = list.sigma2$group.01$plus[nrow(Y.plus)], 
                                   cutoff = cutoff)
    
    Y.minus[nrow(Y.minus),] <- apply(as.matrix(list.U$group.01$minus[nrow(Y.minus),]), 1, 
                                     qUpperTruncNB, 
                                     lambda = list.mu$group.01$minus[nrow(Y.minus)], 
                                     sigma2 = list.sigma2$group.01$minus[nrow(Y.minus)], 
                                     cutoff = cutoff)
    
    # After second randomization until total time
    Y.plus.r <- qnbinom(list.U$group.01$plus.r, mu = list.mu$group.01$plus.r, size=1/list.sigma2$group.01$plus.r)
    Y.minus.r <- qnbinom(list.U$group.01$minus.r, mu = list.mu$group.01$minus.r, size=1/list.sigma2$group.01$minus.r)
    
    Y.group <- list(initial = Y.initial,
                    plus = Y.plus,
                    minus = Y.minus,
                    plus.r = Y.plus.r,
                    minus.r = Y.minus.r)
    
  }else if(group==2 & length(unlist(list.U$group.02))>0){
    
    # At first randomization
    Y.initial <- qnbinom(list.U$group.02$initial, mu = list.mu$group.02$initial, size=1/list.sigma2$group.02$initial)
    
    # After first randomization until second randomization
    Y.plus <- qnbinom(list.U$group.02$plus, mu = list.mu$group.02$plus, size=1/list.sigma2$group.02$plus)
    Y.minus <- qnbinom(list.U$group.02$minus, mu = list.mu$group.02$minus, size=1/list.sigma2$group.02$minus)
    
    Y.plus[nrow(Y.plus),] <- apply(as.matrix(list.U$group.02$plus[nrow(Y.plus),]), 1, 
                                   qUpperTruncNB, 
                                   lambda = list.mu$group.02$plus[nrow(Y.plus)], 
                                   sigma2 = list.sigma2$group.02$plus[nrow(Y.plus)], 
                                   cutoff = cutoff)
    
    Y.minus[nrow(Y.minus),] <- apply(as.matrix(list.U$group.02$minus[nrow(Y.minus),]), 1, 
                                     qLowerTruncNB, 
                                     lambda = list.mu$group.02$minus[nrow(Y.minus)], 
                                     sigma2 = list.sigma2$group.02$minus[nrow(Y.minus)], 
                                     cutoff = cutoff)
    
    # After second randomization until total time
    Y.plus.r <- qnbinom(list.U$group.02$plus.r, mu = list.mu$group.02$plus.r, size=1/list.sigma2$group.02$plus.r)
    Y.minus.nr.plus <- qnbinom(list.U$group.02$minus.nr.plus, mu = list.mu$group.02$minus.nr.plus, size=1/list.sigma2$group.02$minus.nr.plus)
    Y.minus.nr.minus <- qnbinom(list.U$group.02$minus.nr.minus, mu = list.mu$group.02$minus.nr.minus, size=1/list.sigma2$group.02$minus.nr.minus)
    
    Y.group <- list(initial = Y.initial,
                    plus = Y.plus,
                    minus = Y.minus,
                    plus.r = Y.plus.r,
                    minus.nr.plus = Y.minus.nr.plus,
                    minus.nr.minus = Y.minus.nr.minus)
    
  }else if(group==3 & length(unlist(list.U$group.03))>0){
    
    # At first randomization
    Y.initial <- qnbinom(list.U$group.03$initial, mu = list.mu$group.03$initial, size=1/list.sigma2$group.03$initial)
    
    # After first randomization until second randomization
    Y.plus <- qnbinom(list.U$group.03$plus, mu = list.mu$group.03$plus, size=1/list.sigma2$group.03$plus)
    Y.minus <- qnbinom(list.U$group.03$minus, mu = list.mu$group.03$minus, size=1/list.sigma2$group.03$minus)
    
    Y.plus[nrow(Y.plus),] <- apply(as.matrix(list.U$group.03$plus[nrow(Y.plus),]), 1, 
                                   qLowerTruncNB, 
                                   lambda = list.mu$group.03$plus[nrow(Y.plus)], 
                                   sigma2 = list.sigma2$group.03$plus[nrow(Y.plus)], 
                                   cutoff = cutoff)
    
    Y.minus[nrow(Y.minus),] <- apply(as.matrix(list.U$group.03$minus[nrow(Y.minus),]), 1, 
                                     qUpperTruncNB, 
                                     lambda = list.mu$group.03$minus[nrow(Y.minus)], 
                                     sigma2 = list.sigma2$group.03$minus[nrow(Y.minus)], 
                                     cutoff = cutoff)
    
    # After second randomization until total time
    Y.plus.nr.plus <- qnbinom(list.U$group.03$plus.nr.plus, mu = list.mu$group.03$plus.nr.plus, size=1/list.sigma2$group.03$plus.nr.plus)
    Y.plus.nr.minus <- qnbinom(list.U$group.03$plus.nr.minus, mu = list.mu$group.03$plus.nr.minus, size=1/list.sigma2$group.03$plus.nr.minus)
    Y.minus.r <- qnbinom(list.U$group.03$minus.r, mu = list.mu$group.03$minus.r, size=1/list.sigma2$group.03$minus.r)
    
    Y.group <- list(initial = Y.initial,
                    plus = Y.plus,
                    minus = Y.minus,
                    plus.nr.plus = Y.plus.nr.plus,
                    plus.nr.minus = Y.plus.nr.minus,
                    minus.r = Y.minus.r)
    
  }else if(group==4 & length(unlist(list.U$group.04))>0){ 
    
    # At first randomization
    Y.initial <- qnbinom(list.U$group.04$initial, mu = list.mu$group.04$initial, size=1/list.sigma2$group.04$initial)
    
    # After first randomization until second randomization
    Y.plus <- qnbinom(list.U$group.04$plus, mu = list.mu$group.04$plus, size=1/list.sigma2$group.04$plus)
    Y.minus <- qnbinom(list.U$group.04$minus, mu = list.mu$group.04$minus, size=1/list.sigma2$group.04$minus)
    
    Y.plus[nrow(Y.plus),] <- apply(as.matrix(list.U$group.04$plus[nrow(Y.plus),]), 1, 
                                   qLowerTruncNB, 
                                   lambda = list.mu$group.04$plus[nrow(Y.plus)], 
                                   sigma2 = list.sigma2$group.04$plus[nrow(Y.plus)], 
                                   cutoff = cutoff)
    
    Y.minus[nrow(Y.minus),] <- apply(as.matrix(list.U$group.04$minus[nrow(Y.minus),]), 1, 
                                     qLowerTruncNB, 
                                     lambda = list.mu$group.04$minus[nrow(Y.minus)], 
                                     sigma2 = list.sigma2$group.04$minus[nrow(Y.minus)], 
                                     cutoff = cutoff)
    
    # After second randomization until total time
    Y.plus.nr.plus <- qnbinom(list.U$group.04$plus.nr.plus, mu = list.mu$group.04$plus.nr.plus, size=1/list.sigma2$group.04$plus.nr.plus)
    Y.plus.nr.minus <- qnbinom(list.U$group.04$plus.nr.minus, mu = list.mu$group.04$plus.nr.minus, size=1/list.sigma2$group.04$plus.nr.minus)
    Y.minus.nr.plus <- qnbinom(list.U$group.04$minus.nr.plus, mu = list.mu$group.04$minus.nr.plus, size=1/list.sigma2$group.04$minus.nr.plus)
    Y.minus.nr.minus <- qnbinom(list.U$group.04$minus.nr.minus, mu = list.mu$group.04$minus.nr.minus, size=1/list.sigma2$group.04$minus.nr.minus)
    
    Y.group <- list(initial = Y.initial,
                    plus = Y.plus,
                    minus = Y.minus,
                    plus.nr.plus = Y.plus.nr.plus,
                    plus.nr.minus = Y.plus.nr.minus,
                    minus.nr.plus = Y.minus.nr.plus,
                    minus.nr.minus = Y.minus.nr.minus)
    
  }else if(group==1 & length(unlist(list.U$group.01))==0){
    
    Y.group <- data.frame(NULL)
    
  }else if(group==2 & length(unlist(list.U$group.02))==0){
    
    Y.group <- data.frame(NULL)
    
  }else if(group==3 & length(unlist(list.U$group.03))==0){
    
    Y.group <- data.frame(NULL)
    
  }else if(group==4 & length(unlist(list.U$group.04))==0){
    
    Y.group <- data.frame(NULL)
    
  }else{
    print("Check results")
  }
  
  return(Y.group)
}

ByGroupToLongData <- function(Y.group, n.group, group, tot.time, rand.time){
  
  # Args:
  #   Y.group: ADDLATER
  #   n.group: ADDLATER
  #   group: ADDLATER
  #   tot.time: ADDLATER
  #   rand.time: ADDLATER
  
  if(group==1 & length(unlist(Y.group))>0){
    df.group <- data.frame(id = c(1:n.group, rep(1:n.group, each = rand.time-1), rep(1:n.group, each = tot.time-rand.time)),
                           t = c(rep(1,n.group), rep(2:rand.time, times = n.group), rep((rand.time+1):tot.time, times = n.group)),
                           group = rep(1,n.group*tot.time),
                           Y.plusplus = c(Y.group$initial, Y.group$plus, Y.group$plus.r),
                           Y.plusminus = c(Y.group$initial, Y.group$plus, Y.group$plus.r),
                           Y.minusplus = c(Y.group$initial, Y.group$minus, Y.group$minus.r),
                           Y.minusminus = c(Y.group$initial, Y.group$minus, Y.group$minus.r))
    
    reshaped.df.group <- rbind(data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= +1, Yit = df.group$Y.plusplus, R=1),
                               data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= -1, Yit = df.group$Y.plusminus, R=1),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= +1, Yit = df.group$Y.minusplus, R=1),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= -1, Yit = df.group$Y.minusminus, R=1))
    
  }else if(group==2 & length(unlist(Y.group))>0){
    df.group <- data.frame(id = c(1:n.group, rep(1:n.group, each = rand.time-1), rep(1:n.group, each = tot.time-rand.time)),
                           t = c(rep(1,n.group), rep(2:rand.time, times = n.group), rep((rand.time+1):tot.time, times = n.group)),
                           group = rep(2,n.group*tot.time),
                           Y.plusplus = c(Y.group$initial, Y.group$plus, Y.group$plus.r),
                           Y.plusminus = c(Y.group$initial, Y.group$plus, Y.group$plus.r),
                           Y.minusplus = c(Y.group$initial, Y.group$minus, Y.group$minus.nr.plus),
                           Y.minusminus = c(Y.group$initial, Y.group$minus, Y.group$minus.nr.minus))
    
    reshaped.df.group <- rbind(data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= +1, Yit = df.group$Y.plusplus, R=1),
                               data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= -1, Yit = df.group$Y.plusminus, R=1),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= +1, Yit = df.group$Y.minusplus, R=0),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= -1, Yit = df.group$Y.minusminus, R=0))
    
  }else if(group==3 & length(unlist(Y.group))>0){
    df.group <- data.frame(id = c(1:n.group, rep(1:n.group, each = rand.time-1), rep(1:n.group, each = tot.time-rand.time)),
                           t = c(rep(1,n.group), rep(2:rand.time, times = n.group), rep((rand.time+1):tot.time, times = n.group)),
                           group = rep(3,n.group*tot.time),
                           Y.plusplus = c(Y.group$initial, Y.group$plus, Y.group$plus.nr.plus),
                           Y.plusminus = c(Y.group$initial, Y.group$plus, Y.group$plus.nr.minus),
                           Y.minusplus = c(Y.group$initial, Y.group$minus, Y.group$minus.r),
                           Y.minusminus = c(Y.group$initial, Y.group$minus, Y.group$minus.r))
    
    reshaped.df.group <- rbind(data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= +1, Yit = df.group$Y.plusplus, R=0),
                               data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= -1, Yit = df.group$Y.plusminus, R=0),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= +1, Yit = df.group$Y.minusplus, R=1),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= -1, Yit = df.group$Y.minusminus, R=1))
    
  }else if(group==4 & length(unlist(Y.group))>0){
    df.group <- data.frame(id = c(1:n.group, rep(1:n.group, each = rand.time-1), rep(1:n.group, each = tot.time-rand.time)),
                           t = c(rep(1,n.group), rep(2:rand.time, times = n.group), rep((rand.time+1):tot.time, times = n.group)),
                           group = rep(4,n.group*tot.time),
                           Y.plusplus = c(Y.group$initial, Y.group$plus, Y.group$plus.nr.plus),
                           Y.plusminus = c(Y.group$initial, Y.group$plus, Y.group$plus.nr.minus),
                           Y.minusplus = c(Y.group$initial, Y.group$minus, Y.group$minus.nr.plus),
                           Y.minusminus = c(Y.group$initial, Y.group$minus, Y.group$minus.nr.minus))
    
    reshaped.df.group <- rbind(data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= +1, Yit = df.group$Y.plusplus, R=0),
                               data.frame(id = df.group$id, t = df.group$t, A1 = +1, A2= -1, Yit = df.group$Y.plusminus, R=0),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= +1, Yit = df.group$Y.minusplus, R=0),
                               data.frame(id = df.group$id, t = df.group$t, A1 = -1, A2= -1, Yit = df.group$Y.minusminus, R=0))
    
  }else if(group==1 & length(unlist(Y.group))==0){
    
    reshaped.df.group <- data.frame(NULL)
    
  }else if(group==2 & length(unlist(Y.group))==0){
    
    reshaped.df.group <- data.frame(NULL)
    
  }else if(group==3 & length(unlist(Y.group))==0){
    
    reshaped.df.group <- data.frame(NULL)
    
  }else if(group==4 & length(unlist(Y.group))==0){
    
    reshaped.df.group <- data.frame(NULL)
    
  }else{
    print("Check results")
  }
  
  return(reshaped.df.group)
}

GeneratePotentialYit <- function(sim, N, tot.time, rand.time, cutoff, rho){
  
  ###############################################################################
  # No manual inputs after this line
  ###############################################################################
  
  # Calculate dimensions of correlation matrices among four groups
  corrdim.01 <- 2*tot.time - 1
  corrdim.02 <- 3*tot.time - rand.time - 1
  corrdim.03 <- 3*tot.time - rand.time - 1
  corrdim.04 <- 4*tot.time - 2*rand.time - 1
  
  # Create correlation matrix for each group
  list.corrmat <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
  list.corrmat$group.01 <- ExchangeableMat(m = corrdim.01, rho = rho)
  list.corrmat$group.02 <- ExchangeableMat(m = corrdim.02, rho = rho)
  list.corrmat$group.03 <- ExchangeableMat(m = corrdim.03, rho = rho)
  list.corrmat$group.04 <- ExchangeableMat(m = corrdim.04, rho = rho)
  
  # input.means contains mean outcome under each treatment sequence
  # from time 1 until tot.time
  list.mu <- ReadInput(input.df = input.means, tot.time = tot.time, rand.time = rand.time)
  
  # input.prop.zeros contains proportion of zeros in the outcome under each 
  # treatment sequence from time 1 until tot.time
  list.tau <- ReadInput(input.df = input.prop.zeros, tot.time = tot.time, rand.time = rand.time)
  
  # Under the working assumption that the outcome under each treatment sequence
  # from time 1 until tot.time is negative binomial distributed, we use means
  # in input.means and proportion of zeros in input.prop.zeros to calculate the
  # variance in the outcome under each treatment sequence from time 1 until
  # tot.time
  list.sigma2 <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
  list.sigma2$group.01 <- GetVarianceByGroup(all.mu = list.mu$group.01, all.tau = list.tau$group.01)
  list.sigma2$group.02 <- GetVarianceByGroup(all.mu = list.mu$group.02, all.tau = list.tau$group.02)
  list.sigma2$group.03 <- GetVarianceByGroup(all.mu = list.mu$group.03, all.tau = list.tau$group.03)
  list.sigma2$group.04 <- GetVarianceByGroup(all.mu = list.mu$group.04, all.tau = list.tau$group.04)
  
  # Calculate proportion of responders to A1=+1 using cutoff, mean and variance
  # in outcome at rand.time for treatment sequences beginning with A1=+1
  use.var <- tail(list.sigma2$group.01$plus, n=1)
  use.mean <- tail(as.numeric(list.mu$group.01$plus), n=1)
  p <- pnbinom(q = cutoff, size = 1/use.var, mu = use.mean)
  remove(use.var, use.mean)
  
  # Calculate proportion of responders to A1=-1 using cutoff, mean and variance
  # in outcome at rand.time for treatment sequences beginning with A1=-1
  use.var <- tail(list.sigma2$group.01$minus, n=1)
  use.mean <- tail(as.numeric(list.mu$group.01$minus), n=1)
  q <- pnbinom(q = cutoff, size = 1/use.var, mu = use.mean)
  remove(use.var, use.mean)
  
  # Calculate number of individuals belonging to each group
  n4 <- min(ceiling(N*(1-p)), ceiling(N*(1-q)))
  
  n.per.group <- solve(a = matrix(c(1,1,1,
                                    1,1,0,
                                    1,0,1), 
                                  ncol = 3, byrow = TRUE),
                       b = matrix(c(N-n4,
                                    N*p,
                                    N*q), 
                                  ncol = 1, byrow = TRUE)
  )
  
  n1 <- ceiling(n.per.group[1])
  n2 <- ceiling(n.per.group[2])
  n3 <- ceiling(n.per.group[3])
  
  n.generated <- n1+n2+n3+n4
  
  ###############################################################################
  # Checks in between to ensure that calculations are performed as expected
  ###############################################################################
  
  assert_that(abs((n1+n2)/N - p) < 0.01, msg = "Emprical proportion of responders is incorrect")
  assert_that(abs((n1+n3)/N - q) < 0.01, msg = "Emprical proportion of responders is incorrect")
  assert_that(abs((n3+n4)/N - (1-p)) < 0.01, msg = "Emprical proportion of non-responders is incorrect")
  assert_that(abs((n2+n4)/N - (1-q)) < 0.01, msg = "Emprical proportion of non-responders is incorrect")
  
  # -----------------------------------------------------------------------------
  # Simulate potential outcomes
  # -----------------------------------------------------------------------------
  
  # Generate U matrix
  list.U <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
  list.U$group.01 <- ByGroupGenerateU(n.group = n1, corrdim = corrdim.01, corrmat = list.corrmat$group.01)
  list.U$group.02 <- ByGroupGenerateU(n.group = n2, corrdim = corrdim.02, corrmat = list.corrmat$group.02)
  list.U$group.03 <- ByGroupGenerateU(n.group = n3, corrdim = corrdim.03, corrmat = list.corrmat$group.03)
  list.U$group.04 <- ByGroupGenerateU(n.group = n4, corrdim = corrdim.04, corrmat = list.corrmat$group.04)
  
  # In preparation for obtaining Y, obtain subsets of elements of U matrix and 
  # rearrange into list
  list.U$group.01 <- ByGroupMatrixToList(mat = list.U$group.01, group = 1, rand.time = rand.time, tot.time = tot.time)
  list.U$group.02 <- ByGroupMatrixToList(mat = list.U$group.02, group = 2, rand.time = rand.time, tot.time = tot.time)
  list.U$group.03 <- ByGroupMatrixToList(mat = list.U$group.03, group = 3, rand.time = rand.time, tot.time = tot.time)
  list.U$group.04 <- ByGroupMatrixToList(mat = list.U$group.04, group = 4, rand.time = rand.time, tot.time = tot.time)
  
  # Generate Y matrix
  list.Y <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
  list.Y$group.01 <- ByGroupGenerateY(list.U=list.U, group=1, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)
  list.Y$group.02 <- ByGroupGenerateY(list.U=list.U, group=2, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)
  list.Y$group.03 <- ByGroupGenerateY(list.U=list.U, group=3, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)
  list.Y$group.04 <- ByGroupGenerateY(list.U=list.U, group=4, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)
  
  # Reshape Y from list to data frame
  list.Y$group.01 <- ByGroupToLongData(Y.group = list.Y$group.01, n.group=n1, group=1, tot.time=tot.time, rand.time=rand.time)
  list.Y$group.02 <- ByGroupToLongData(Y.group = list.Y$group.02, n.group=n2, group=2, tot.time=tot.time, rand.time=rand.time)
  list.Y$group.03 <- ByGroupToLongData(Y.group = list.Y$group.03, n.group=n3, group=3, tot.time=tot.time, rand.time=rand.time)
  list.Y$group.04 <- ByGroupToLongData(Y.group = list.Y$group.04, n.group=n4, group=4, tot.time=tot.time, rand.time=rand.time)
  
  list.Y$group.02$id <- n1 + list.Y$group.02$id
  list.Y$group.03$id <- n1 + n2 + list.Y$group.03$id
  list.Y$group.04$id <- n1 + n2 + n3 + list.Y$group.04$id
  
  # Finally, we have a data frame with the potential outcomes
  df.potential.Yit <- bind_rows(list.Y)
  df.potential.Yit <- df.potential.Yit %>% arrange(id, desc(A1), desc(A2), t)
  df.potential.Yit <- df.potential.Yit %>% mutate(sim = sim) %>% mutate(rho = rho)
  
  return(df.potential.Yit)
}

DTRCorrelationPO <- function(df){
  
  # Args:
  #   df: ADDLATER
  
  # DTR (++)
  widedat.plusplus <- df %>% filter(A1==1 & A2==1) %>% select(id, t, Yit) %>%
    reshape(data = ., timevar = "t", idvar = "id", direction = "wide") %>%
    select(-id)
  
  cormat.plusplus <- cor(widedat.plusplus)
  
  # DTR (+-)
  widedat.plusminus <- df %>% filter(A1==1 & A2==-1) %>% select(id, t, Yit) %>%
    reshape(data = ., timevar = "t", idvar = "id", direction = "wide") %>%
    select(-id)
  
  cormat.plusminus <- cor(widedat.plusminus)
  
  # DTR (-+)
  widedat.minusplus <- df %>% filter(A1==-1 & A2==1) %>% select(id, t, Yit) %>%
    reshape(data = ., timevar = "t", idvar = "id", direction = "wide") %>%
    select(-id)
  
  cormat.minusplus <- cor(widedat.minusplus)
  
  # DTR (--)
  widedat.minusminus <- df %>% filter(A1==-1 & A2==-1) %>% select(id, t, Yit) %>%
    reshape(data = ., timevar = "t", idvar = "id", direction = "wide") %>%
    select(-id)
  
  cormat.minusminus <- cor(widedat.minusminus)
  
  # Across all DTRs
  rho.star <- data.frame(plusplus = cormat.plusplus[upper.tri(cormat.plusplus)],
                         plusminus = cormat.plusminus[upper.tri(cormat.plusminus)],
                         minusplus = cormat.minusplus[upper.tri(cormat.minusplus)],
                         minusminus = cormat.minusminus[upper.tri(cormat.minusminus)])
  
  rho.star.max <- apply(rho.star, 2, max)
  rho.star.min <- apply(rho.star, 2, min)
  rho.star.ave <- colMeans(rho.star)
  
  list.out <- list(sim = df$sim[1],
                   rho = df$rho[1],
                   rho.star.max = rho.star.max, 
                   rho.star.min = rho.star.min,
                   rho.star.ave = rho.star.ave)
  
  return(list.out)
}

GenerateObservedYit <- function(df.potential.Yit){
  
  N <- max(df.potential.Yit$id)
  obsdf <- data.frame(id = seq(1,N),
                      observed.A1 = rep(NA, times = N),
                      observed.A2 = rep(NA, times = N))
  obsdf$observed.A1 <- base::sample(x = c(-1,1), size = nrow(obsdf), replace = TRUE, prob = c(0.5, 0.5))
  # Later on, we will set observed.A2 to NA for those who responded to A1
  obsdf$observed.A2 <- base::sample(x = c(-1,1), size = nrow(obsdf), replace = TRUE, prob = c(0.5, 0.5))
  obsdf <- obsdf %>% arrange(id)
  
  df.potential.Yit <- left_join(df.potential.Yit, obsdf, by = c("id"))
  df.potential.Yit <- df.potential.Yit %>% mutate(observed.A2 = replace(observed.A2, R==1, NA))
  # whichDTR is an indicator for which among the four sets of potential outcomes was actually observed
  df.potential.Yit <- df.potential.Yit %>% 
    mutate(whichDTR = case_when(A1 == observed.A1 & R == 1 ~ 1, 
                                A1 == observed.A1 & R == 0 & A2 == observed.A2 ~ 1,
                                TRUE ~ 0))
  df.observed.Yit <- df.potential.Yit %>% filter(whichDTR == 1) %>%
    select(id, t, observed.A1, observed.A2, R, Yit)
  
  # df.observed.Yit contains duplicate rows for responders to first stage intervention
  # we remove those duplicate rows
  df.observed.Yit <- unique(df.observed.Yit)
  df.observed.Yit <- df.observed.Yit %>% arrange(id, t)
  df.observed.Yit <- apply(df.observed.Yit, 2, as.numeric)
  df.observed.Yit <- as.data.frame(df.observed.Yit)
  
  return(df.observed.Yit)
}
