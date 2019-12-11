library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
  
WeightAndReplicate <- function(list.df, tot.time){
  
  # This function weights and replicates observations of participants 
  # who have undergone a SMART
  #
  # Args:
  #   DataLongFormat -- dataset in long format, as returned by the 
  #   GenerateData function
  #
  # Returns:
  #   data.for.analysis -- weighted and replicated dataset in long format,
  #   ready to be used for analysis in R package geeM
  
  datagen.params <- list.df$datagen.params
  DataLongFormat <- list.df$df.observed.Yit
  
  nwaves <- tot.time
  DataLongFormat$KnownWeight1 <- 2
  DataLongFormat$KnownWeight2 <- 2*(DataLongFormat$R==0)+1*(DataLongFormat$R==1)
  
  # that is, IF (R = 0) THEN known_wt2 = 2, ELSE known_wt2 = 1
  DataLongFormat$KnownWeight <- DataLongFormat$KnownWeight1 * DataLongFormat$KnownWeight2
  
  # Create "replications"
  DataLongFormat$ActualTime <- DataLongFormat$t
  RowsToReplicate <- DataLongFormat[which(DataLongFormat$R==1),]
  PlusOnePseudodata <- RowsToReplicate
  PlusOnePseudodata$observed.A2 <- 1
  MinusOnePseudodata <- RowsToReplicate
  MinusOnePseudodata$observed.A2 <- -1
  MinusOnePseudodata$t <- MinusOnePseudodata$t + nwaves
  
  RowsNotToReplicate <- DataLongFormat[which(DataLongFormat$R==0),]
  
  # We keep the same subject ID to show that we don't really have all those
  # new participants. So we have to distinguish the new observations somehow,
  # and so we treat them as new waves of data on the same person.
  # Create the final analysis dataset including replicates.
  data.for.analysis <- rbind(PlusOnePseudodata, 
                             MinusOnePseudodata, 
                             RowsNotToReplicate)
  data.for.analysis <- data.for.analysis[order(data.for.analysis$id,data.for.analysis$t),] 
  
  colnames(data.for.analysis) <- c("id", "wave", "A1", "A2", "R", "Y", 
                                   "KnownWeight1", "KnownWeight2", "KnownWeight", "ActualTime")
  
  out.list <- list(datagen.params=datagen.params,
                   data.for.analysis=data.for.analysis)
  
  return(out.list)
}

AnalyzeData <- function(list.df, tot.time, rand.time, working.corr="independence"){
  
  # Args:
  #   df.replicated.observed.Yit: ADDLATER
  #   tot.time: ADDLATER
  #   rand.time: ADDLATER
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  #ADDLATER
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  datagen.params <- list.df$datagen.params
  df.replicated.observed.Yit <- list.df$data.for.analysis
  
  fo.int <- "1"
  fo.plus <- paste("I(ActualTime==", 2:rand.time, " & A1==+1)", sep="")
  fo.minus <- paste("I(ActualTime==", 2:rand.time, " & A1==-1)", sep="")
  fo.plusplus <- paste("I(ActualTime==", (rand.time+1):tot.time, " & A1==+1 & A2==+1)", sep="") 
  fo.plusminus <- paste("I(ActualTime==", (rand.time+1):tot.time, " & A1==+1 & A2==-1)", sep="") 
  fo.minusplus <- paste("I(ActualTime==", (rand.time+1):tot.time, " & A1==-1 & A2==+1)", sep="") 
  fo.minusminus <- paste("I(ActualTime==", (rand.time+1):tot.time, " & A1==-1 & A2==-1)", sep="") 
  
  fo.all <- c(fo.plus, fo.minus, 
              fo.plusplus, fo.plusminus, 
              fo.minusplus, fo.minusminus)
  fo <- fo.int
  
  for(i in 1:length(fo.all)){
    fo <- paste(fo, fo.all[i], sep="+")
  }
  
  fo <- paste("Y ~ ", fo, sep="")
  fo <- as.formula(fo)
  
  # ---------------------------------------------------------------------------
  # Fit initial model and residuals
  # ---------------------------------------------------------------------------
  model.init <- geemMod(formula = fo,
                        data = df.replicated.observed.Yit, 
                        id = id,
                        waves = wave, # No missing data 
                        family = "poisson",
                        corstr = "independence",
                        weights = KnownWeight,
                        scale.fix = TRUE)
  
  if(model.init$converged==FALSE){
    est <- list(est.beta=NA,
                coefnames=NA,
                est.cov.beta=NA,
                converged=0) 
    
    out.list <- list(datagen.params = datagen.params,
                     estimates=est)
  }else{
    # Obtain initial estimates of beta
    init.est.beta <- as.matrix(model.init$beta)
    df.replicated.observed.Yit$yhat <- exp((model.init$X) %*% init.est.beta)
    df.replicated.observed.Yit$resid <- df.replicated.observed.Yit$Y - df.replicated.observed.Yit$yhat 
    df.replicated.observed.Yit$resid.squared <- (df.replicated.observed.Yit$resid)^2
    
    summary.df <- df.replicated.observed.Yit %>% 
      group_by(A1, A2, ActualTime) %>%
      summarise(mean.resid = weighted.mean(x = resid, w = KnownWeight),
                mean.resid.squared = weighted.mean(x = resid.squared, w = KnownWeight),
                mean.yhat = weighted.mean(x = yhat, w = KnownWeight)) %>%
      mutate(sigsq = (mean.resid.squared - mean.yhat) / (mean.yhat^2)) %>%
      select(A1, A2, ActualTime, sigsq)
    
    df.replicated.observed.Yit <- left_join(df.replicated.observed.Yit, summary.df, by = c("A1", "A2", "ActualTime"))
    disp.param <- df.replicated.observed.Yit$sigsq
    
    # ---------------------------------------------------------------------------
    # Specify FunList
    # ---------------------------------------------------------------------------
    LinkFun <- function(mu){
      out <- log(mu)
      return(out)
    }
    
    VarFun <- function(mu){
      out <- mu + disp.param*(mu^2)
      return(out)
    }
    
    InvLink <- function(eta){
      out <- exp(eta)
      return(out)
    }
    
    InvLinkDeriv <- function(eta){
      out <- exp(eta)
      return(out)
    }
    
    FunList <- list(LinkFun, VarFun, InvLink, InvLinkDeriv)
    
    if(working.corr=="independence"){
      model.indep <- geemMod(formula = fo,
                             data = df.replicated.observed.Yit, 
                             id = id,
                             waves = wave, # No missing data 
                             family = FunList,
                             corstr = "independence",
                             weights = KnownWeight,
                             scale.fix = TRUE,
                             fullmat = FALSE)
      
      if(model.indep$converged == FALSE){
        est <- list(est.beta=NA,
                    coefnames=NA,
                    est.cov.beta=NA,
                    converged=0) 
        
        out.list <- list(datagen.params = datagen.params,
                         estimates=est)
      }else{
        # Calculate estimates of the mean per DTR and time
        est.beta <- as.matrix(model.indep$beta)
        coefnames <- model.indep$coefnames
        est.cov.beta <- model.indep$var
        converged <- 1*(model.indep$converged==TRUE)
        est <- list(est.beta = est.beta, 
                    coefnames = coefnames,
                    est.cov.beta = est.cov.beta, 
                    converged = converged)
        out.list <- list(datagen.params = datagen.params,
                         estimates=est)
      }
    }else if(working.corr=="ar1"){
      # ADD LATER
    }else{
      assert_that(working.corr %in% c("independence","ar1"), 
                  msg = "Enter valid working correlation")
    }
  }

  return(out.list)
}

CreateC <- function(input.tot.time, input.rand.time){
  
  C.plusplus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)
  C.plusminus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)
  C.minusplus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)
  C.minusminus <- matrix(rep(0,input.tot.time*(4*input.tot.time-2*input.rand.time-1)), nrow=input.tot.time)
  
  i <- 0
  j <- 0
  for(time.now in 1:input.tot.time){
    if(time.now==1){
      C.plusplus[time.now,1] <- 1
    }else if(time.now>1 & time.now<=input.rand.time){
      C.plusplus[time.now,1] <- 1
      C.plusplus[time.now,2+i] <- 1
      i <- i+1
    }else{
      C.plusplus[time.now,1] <- 1
      C.plusplus[time.now,2*input.rand.time+j] <- 1
      j <- j+1
    }
  }
  
  i <- 0
  j <- 0
  for(time.now in 1:input.tot.time){
    if(time.now==1){
      C.plusminus[time.now,1] <- 1
    }else if(time.now>1 & time.now<=input.rand.time){
      C.plusminus[time.now,1] <- 1
      C.plusminus[time.now,2+i] <- 1
      i <- i+1
    }else{
      C.plusminus[time.now,1] <- 1
      C.plusminus[time.now,input.tot.time+input.rand.time+j] <- 1
      j <- j+1
    }
  }
  
  i <- 0
  j <- 0
  for(time.now in 1:input.tot.time){
    if(time.now==1){
      C.minusplus[time.now,1] <- 1
    }else if(time.now>1 & time.now<=input.rand.time){
      C.minusplus[time.now,1] <- 1
      C.minusplus[time.now,input.rand.time+1+i] <- 1
      i <- i+1
    }else{
      C.minusplus[time.now,1] <- 1
      C.minusplus[time.now,2*input.tot.time+j] <- 1
      j <- j+1
    }
  }
  
  i <- 0
  j <- 0
  for(time.now in 1:input.tot.time){
    if(time.now==1){
      C.minusminus[time.now,1] <- 1
    }else if(time.now>1 & time.now<=input.rand.time){
      C.minusminus[time.now,1] <- 1
      C.minusminus[time.now,input.rand.time+1+i] <- 1
      i <- i+1
    }else{
      C.minusminus[time.now,1] <- 1
      C.minusminus[time.now,3*input.tot.time-input.rand.time+j] <- 1
      j <- j+1
    }
  }
  
  list.C <- list(C.plusplus = C.plusplus,
                 C.plusminus = C.plusminus,
                 C.minusplus = C.minusplus,
                 C.minusminus = C.minusminus)
  
  return(list.C)
}

EstimateMeans <- function(list.df, list.C){
  
  C.plusplus <- list.C$C.plusplus
  C.plusminus <- list.C$C.plusminus
  C.minusplus <- list.C$C.minusplus
  C.minusminus <- list.C$C.minusminus
  
  datagen.params <- list.df$datagen.params
  est.beta <- list.df$estimates$est.beta
  converged <- list.df$estimates$converged
  
  if(converged==1){
    # DTR (+1,+1)
    est.u.plusplus <- exp(C.plusplus%*%est.beta)
    est.u.plusplus <- list(datagen.params=datagen.params,
                           estimates=data.frame(est.u.plusplus = est.u.plusplus))
    # DTR (+1,-1)
    est.u.plusminus <- exp(C.plusminus%*%est.beta)
    est.u.plusminus <- list(datagen.params=datagen.params,
                            estimates=data.frame(est.u.plusminus = est.u.plusminus))
    # DTR (-1,+1)
    est.u.minusplus <- exp(C.minusplus%*%est.beta)
    est.u.minusplus <- list(datagen.params=datagen.params,
                            estimates=data.frame(est.u.minusplus = est.u.minusplus))
    # DTR (-1,-1)
    est.u.minusminus <- exp(C.minusminus%*%est.beta)
    est.u.minusminus <- list(datagen.params=datagen.params,
                             estimates=data.frame(est.u.minusminus = est.u.minusminus))
  }else{
    est.u.plusplus <- list(datagen.params=datagen.params,
                           estimates=data.frame(NULL))
    est.u.plusminus <- list(datagen.params=datagen.params,
                            estimates=data.frame(NULL))
    est.u.minusplus <- list(datagen.params=datagen.params,
                            estimates=data.frame(NULL))
    est.u.minusminus <- list(datagen.params=datagen.params,
                             estimates=data.frame(NULL))
  }
  
  list.out <- list(plusplus=est.u.plusplus,
                   plusminus=est.u.plusminus,
                   minusplus=est.u.minusplus,
                   minusminus=est.u.minusminus)
  
  return(list.out)
}

EstimateLinearCombo <- function(list.df, L, list.C){
  
  C.plusplus <- list.C$C.plusplus
  C.plusminus <- list.C$C.plusminus
  C.minusplus <- list.C$C.minusplus
  C.minusminus <- list.C$C.minusminus
  
  datagen.params <- list.df$datagen.params
  est.beta <- list.df$estimates$est.beta
  converged <- list.df$estimates$converged
  
  if(converged==1){
    # DTR (+1,+1)
    est.plusplus <- L %*% exp(C.plusplus%*%est.beta)
    est.plusplus <- list(datagen.params=datagen.params,
                         estimates=data.frame(est.plusplus = est.plusplus))
    # DTR (+1,-1)
    est.plusminus <- L %*% exp(C.plusminus%*%est.beta)
    est.plusminus <- list(datagen.params=datagen.params,
                          estimates=data.frame(est.plusminus = est.plusminus))
    # DTR (-1,+1)
    est.minusplus <- L %*% exp(C.minusplus%*%est.beta)
    est.minusplus <- list(datagen.params=datagen.params,
                          estimates=data.frame(est.minusplus = est.minusplus))
    # DTR (-1,-1)
    est.minusminus <- L %*% exp(C.minusminus%*%est.beta)
    est.minusminus <- list(datagen.params=datagen.params,
                           estimates=data.frame(est.minusminus = est.minusminus))
  }else{
    est.plusplus <- list(datagen.params=datagen.params,
                         estimates=data.frame(NULL))
    est.plusminus <- list(datagen.params=datagen.params,
                          estimates=data.frame(NULL))
    est.minusplus <- list(datagen.params=datagen.params,
                          estimates=data.frame(NULL))
    est.minusminus <- list(datagen.params=datagen.params,
                           estimates=data.frame(NULL))
  }
  
  list.out <- list(plusplus=est.plusplus,
                   plusminus=est.plusminus,
                   minusplus=est.minusplus,
                   minusminus=est.minusminus)
  
  return(list.out)
}

EstimateDiffs <- function(list.df, D, list.C){
  
  datagen.params <- list.df$datagen.params
  est.beta <- list.df$estimates$est.beta
  converged <- list.df$estimates$converged
  
  C.plusplus <- list.C$C.plusplus
  C.plusminus <- list.C$C.plusminus
  C.minusplus <- list.C$C.minusplus
  C.minusminus <- list.C$C.minusminus
  
  stacked.C.plusplus.plusminus <- rbind(C.plusplus, C.plusminus)
  stacked.C.plusplus.minusplus <- rbind(C.plusplus, C.minusplus)
  stacked.C.plusplus.minusminus <- rbind(C.plusplus, C.minusminus)
  stacked.C.plusminus.minusplus <- rbind(C.plusminus, C.minusplus)
  stacked.C.plusminus.minusminus <- rbind(C.plusminus, C.minusminus)
  stacked.C.minusminus.minusplus <- rbind(C.minusminus, C.minusplus)
  
  if(converged==1){
    # DTR (+1,+1) vs. (+1,-1)
    est.diff.plusplus.plusminus <- D %*% exp(stacked.C.plusplus.plusminus %*% est.beta)
    list.est.diff.plusplus.plusminus <- list(datagen.params=datagen.params,
                                             estimates = data.frame(est.diff.plusplus.plusminus = est.diff.plusplus.plusminus))
    # DTR (+1,+1) vs. (-1,+1)
    est.diff.plusplus.minusplus <- D %*% exp(stacked.C.plusplus.minusplus %*% est.beta)
    list.est.diff.plusplus.minusplus <- list(datagen.params=datagen.params,
                                             estimates = data.frame(est.diff.plusplus.minusplus = est.diff.plusplus.minusplus))
    # DTR (+1,+1) vs. (-1,-1)
    est.diff.plusplus.minusminus <- D %*% exp(stacked.C.plusplus.minusminus %*% est.beta)
    list.est.diff.plusplus.minusminus <- list(datagen.params=datagen.params,
                                              estimates = data.frame(est.diff.plusplus.minusminus = est.diff.plusplus.minusminus))
    # DTR (+1,-1) vs. (-1,+1)
    est.diff.plusminus.minusplus <- D %*% exp(stacked.C.plusminus.minusplus %*% est.beta)
    list.est.diff.plusminus.minusplus <- list(datagen.params=datagen.params,
                                              estimates = data.frame(est.diff.plusminus.minusplus = est.diff.plusminus.minusplus))
    # DTR (+1,-1) vs. (-1,-1)
    est.diff.plusminus.minusminus <- D %*% exp(stacked.C.plusminus.minusminus %*% est.beta)
    list.est.diff.plusminus.minusminus <- list(datagen.params=datagen.params,
                                               estimates = data.frame(est.diff.plusminus.minusminus = est.diff.plusminus.minusminus))
    # DTR (-1,-1) vs. (-1,+1)
    est.diff.minusminus.minusplus <- D %*% exp(stacked.C.minusminus.minusplus %*% est.beta)
    list.est.diff.minusminus.minusplus <- list(datagen.params=datagen.params,
                                               estimates = data.frame(est.diff.minusminus.minusplus = est.diff.minusminus.minusplus))
  }else{
    list.est.diff.plusplus.plusminus <- list(datagen.params=datagen.params,
                                             estimates = data.frame(NULL))
    list.est.diff.plusplus.minusplus <- list(datagen.params=datagen.params,
                                             estimates = data.frame(NULL))
    list.est.diff.plusplus.minusminus <- list(datagen.params=datagen.params,
                                              estimates = data.frame(NULL))
    list.est.diff.plusminus.minusplus <- list(datagen.params=datagen.params,
                                              estimates = data.frame(NULL))
    list.est.diff.plusminus.minusminus <- list(datagen.params=datagen.params,
                                               estimates = data.frame(NULL))
    list.est.diff.minusminus.minusplus <- list(datagen.params=datagen.params,
                                               estimates = data.frame(NULL))
  }
  
  list.est.diff <- list(plusplus.plusminus = list.est.diff.plusplus.plusminus,
                        plusplus.minusplus = list.est.diff.plusplus.minusplus,
                        plusplus.minusminus = list.est.diff.plusplus.minusminus,
                        plusminus.minusplus = list.est.diff.plusminus.minusplus,
                        plusminus.minusminus = list.est.diff.plusminus.minusminus,
                        minusminus.minusplus = list.est.diff.minusminus.minusplus)
  
  return(list.est.diff)
}



EstimateStdErrLinearCombo <- function(list.df, L, list.C){
  
  datagen.params <- list.df$datagen.params
  est.beta <- list.df$estimates$est.beta
  converged <- list.df$estimates$converged
  est.cov.beta <- list.df$estimates$est.cov.beta
  est.cov.beta <- as.matrix(est.cov.beta)
  
  C.plusplus <- list.C$C.plusplus
  C.plusminus <- list.C$C.plusminus
  C.minusplus <- list.C$C.minusplus
  C.minusminus <- list.C$C.minusminus
  
  if(converged==1){
    
    u.plusplus <- exp(C.plusplus %*% est.beta)
    u.plusminus <- exp(C.plusminus %*% est.beta)
    u.minusplus <- exp(C.minusplus %*% est.beta)
    u.minusminus <- exp(C.minusminus %*% est.beta)
    
    U.plusplus <- diag(c(u.plusplus))
    U.plusminus <- diag(c(u.plusminus))
    U.minusplus <- diag(c(u.minusplus))
    U.minusminus <- diag(c(u.minusminus))
    
    # DTR (+1,+1)
    est.cov.plusplus <- (L %*% U.plusplus %*% C.plusplus) %*% est.cov.beta %*% t(L %*% U.plusplus %*% C.plusplus) 
    est.stderr.plusplus <- sqrt(est.cov.plusplus)
    est.stderr.plusplus <- list(datagen.params=datagen.params, estimates=data.frame(est.stderr.plusplus=est.stderr.plusplus))
    
    # DTR (+1,-1)
    est.cov.plusminus <- (L %*% U.plusminus %*% C.plusminus) %*% est.cov.beta %*% t(L %*% U.plusminus %*% C.plusminus) 
    est.stderr.plusminus <- sqrt(est.cov.plusminus)
    est.stderr.plusminus <- list(datagen.params=datagen.params, estimates=data.frame(est.stderr.plusminus=est.stderr.plusminus))
    
    # DTR (-1,+1)
    est.cov.minusplus <- (L %*% U.minusplus %*% C.minusplus) %*% est.cov.beta %*% t(L %*% U.minusplus %*% C.minusplus) 
    est.stderr.minusplus <- sqrt(est.cov.minusplus)
    est.stderr.minusplus <- list(datagen.params=datagen.params, estimates=data.frame(est.stderr.minusplus=est.stderr.minusplus))
    
    # DTR (-1,-1)
    est.cov.minusminus <- (L %*% U.minusminus %*% C.minusminus) %*% est.cov.beta %*% t(L %*% U.minusminus %*% C.minusminus) 
    est.stderr.minusminus <- sqrt(est.cov.minusminus)
    est.stderr.minusminus <- list(datagen.params=datagen.params, estimates=data.frame(est.stderr.minusminus=est.stderr.minusminus))
    
  }else{
    est.stderr.plusplus <- list(datagen.params=datagen.params, estimates=data.frame(NULL))
    est.stderr.plusminus <- list(datagen.params=datagen.params, estimates=data.frame(NULL))
    est.stderr.minusplus <- list(datagen.params=datagen.params, estimates=data.frame(NULL))
    est.stderr.minusminus <- list(datagen.params=datagen.params, estimates=data.frame(NULL))
  }
  
  list.est.stderr <- list(plusplus = est.stderr.plusplus,
                          plusminus = est.stderr.plusminus,
                          minusplus = est.stderr.minusplus,
                          minusminus = est.stderr.minusminus)
  
  return(list.est.stderr)
}

EstimateStdErrDiffs <- function(list.df, D, list.C){
  
  datagen.params <- list.df$datagen.params
  est.beta <- list.df$estimates$est.beta
  converged <- list.df$estimates$converged
  est.cov.beta <- list.df$estimates$est.cov.beta
  est.cov.beta <- as.matrix(est.cov.beta)
  
  C.plusplus <- list.C$C.plusplus
  C.plusminus <- list.C$C.plusminus
  C.minusplus <- list.C$C.minusplus
  C.minusminus <- list.C$C.minusminus
  
  stacked.C.plusplus.plusminus <- rbind(C.plusplus, C.plusminus)
  stacked.C.plusplus.minusplus <- rbind(C.plusplus, C.minusplus)
  stacked.C.plusplus.minusminus <- rbind(C.plusplus, C.minusminus)
  stacked.C.plusminus.minusplus <- rbind(C.plusminus, C.minusplus)
  stacked.C.plusminus.minusminus <- rbind(C.plusminus, C.minusminus)
  stacked.C.minusminus.minusplus <- rbind(C.minusminus, C.minusplus)
  
  if(converged==1){
    
    u.plusplus.plusminus <- exp(stacked.C.plusplus.plusminus %*% est.beta)
    u.plusplus.minusplus <- exp(stacked.C.plusplus.minusplus %*% est.beta)
    u.plusplus.minusminus <- exp(stacked.C.plusplus.minusminus %*% est.beta)
    u.plusminus.minusplus <- exp(stacked.C.plusminus.minusplus %*% est.beta)
    u.plusminus.minusminus <- exp(stacked.C.plusminus.minusminus %*% est.beta)
    u.minusminus.minusplus <- exp(stacked.C.minusminus.minusplus %*% est.beta)
    
    U.plusplus.plusminus <- diag(c(u.plusplus.plusminus))
    U.plusplus.minusplus <- diag(c(u.plusplus.minusplus))
    U.plusplus.minusminus <- diag(c(u.plusplus.minusminus))
    U.plusminus.minusplus <- diag(c(u.plusminus.minusplus))
    U.plusminus.minusminus <- diag(c(u.plusminus.minusminus))
    U.minusminus.minusplus<- diag(c(u.minusminus.minusplus))
    
    # DTR (+1,+1) vs. (+1,-1)
    est.cov.diff.plusplus.plusminus <- (D %*% U.plusplus.plusminus %*% stacked.C.plusplus.plusminus) %*% est.cov.beta %*% t(D %*% U.plusplus.plusminus %*% stacked.C.plusplus.plusminus) 
    est.stderr.diff.plusplus.plusminus <- sqrt(est.cov.diff.plusplus.plusminus)
    est.stderr.diff.plusplus.plusminus <- list(datagen.params=datagen.params,
                                               estimates=data.frame(est.stderr.diff.plusplus.plusminus = est.stderr.diff.plusplus.plusminus))
    # DTR (+1,+1) vs. (-1,+1)
    est.cov.diff.plusplus.minusplus <- (D %*% U.plusplus.minusplus %*% stacked.C.plusplus.minusplus) %*% est.cov.beta %*% t(D %*% U.plusplus.minusplus %*% stacked.C.plusplus.minusplus) 
    est.stderr.diff.plusplus.minusplus <- sqrt(est.cov.diff.plusplus.minusplus)
    est.stderr.diff.plusplus.minusplus <- list(datagen.params=datagen.params,
                                               estimates=data.frame(est.stderr.diff.plusplus.minusplus = est.stderr.diff.plusplus.minusplus))
    # DTR (+1,+1) vs. (-1,-1)
    est.cov.diff.plusplus.minusminus <- (D %*% U.plusplus.minusminus %*% stacked.C.plusplus.minusminus) %*% est.cov.beta %*% t(D %*% U.plusplus.minusminus %*% stacked.C.plusplus.minusminus) 
    est.stderr.diff.plusplus.minusminus <- sqrt(est.cov.diff.plusplus.minusminus)
    est.stderr.diff.plusplus.minusminus <- list(datagen.params=datagen.params,
                                                estimates=data.frame(est.stderr.diff.plusplus.minusminus = est.stderr.diff.plusplus.minusminus))
    # DTR (+1,-1) vs. (-1,+1)
    est.cov.diff.plusminus.minusplus <- (D %*% U.plusminus.minusplus %*% stacked.C.plusminus.minusplus) %*% est.cov.beta %*% t(D %*% U.plusminus.minusplus %*% stacked.C.plusminus.minusplus) 
    est.stderr.diff.plusminus.minusplus <- sqrt(est.cov.diff.plusminus.minusplus)
    est.stderr.diff.plusminus.minusplus <- list(datagen.params=datagen.params,
                                                estimates=data.frame(est.stderr.diff.plusminus.minusplus = est.stderr.diff.plusminus.minusplus))
    # DTR (+1,-1) vs. (-1,-1)
    est.cov.diff.plusminus.minusminus <- (D %*% U.plusminus.minusminus %*% stacked.C.plusminus.minusminus) %*% est.cov.beta %*% t(D %*% U.plusminus.minusminus %*% stacked.C.plusminus.minusminus) 
    est.stderr.diff.plusminus.minusminus <- sqrt(est.cov.diff.plusminus.minusminus)
    est.stderr.diff.plusminus.minusminus <- list(datagen.params=datagen.params,
                                                 estimates=data.frame(est.stderr.diff.plusminus.minusminus = est.stderr.diff.plusminus.minusminus))
    # DTR (-1,-1) vs. (-1,+1)
    est.cov.diff.minusminus.minusplus <- (D %*% U.minusminus.minusplus %*% stacked.C.minusminus.minusplus) %*% est.cov.beta %*% t(D %*% U.minusminus.minusplus %*% stacked.C.minusminus.minusplus) 
    est.stderr.diff.minusminus.minusplus <- sqrt(est.cov.diff.minusminus.minusplus)
    est.stderr.diff.minusminus.minusplus <- list(datagen.params=datagen.params,
                                                 estimates=data.frame(est.stderr.diff.minusminus.minusplus = est.stderr.diff.minusminus.minusplus))
  }else{
    est.stderr.diff.plusplus.plusminus <- list(datagen.params=datagen.params,
                                               estimates=data.frame(NULL))
    est.stderr.diff.plusplus.minusplus <- list(datagen.params=datagen.params,
                                               estimates=data.frame(NULL))
    est.stderr.diff.plusplus.minusminus <- list(datagen.params=datagen.params,
                                                estimates=data.frame(NULL))
    est.stderr.diff.plusminus.minusplus <- list(datagen.params=datagen.params,
                                                estimates=data.frame(NULL))
    est.stderr.diff.plusminus.minusminus <- list(datagen.params=datagen.params,
                                                 estimates=data.frame(NULL))
    est.stderr.diff.minusminus.minusplus <- list(datagen.params=datagen.params,
                                                 estimates=data.frame(NULL))
  }
  
  list.est.stderr.diff <- list(plusplus.plusminus = est.stderr.diff.plusplus.plusminus,
                               plusplus.minusplus = est.stderr.diff.plusplus.minusplus,
                               plusplus.minusminus = est.stderr.diff.plusplus.minusminus,
                               plusminus.minusplus = est.stderr.diff.plusminus.minusplus,
                               plusminus.minusminus = est.stderr.diff.plusminus.minusminus,
                               minusminus.minusplus = est.stderr.diff.minusminus.minusplus)
  
  return(list.est.stderr.diff)
}

ReshapeList <- function(x){
  
  datagen.params <- x[[1]]$datagen.params
  estimates <- x[[1]]$estimates
  
  if(length(estimates)>0){
    colnames(estimates) <- "estimates"
    outdf <- data.frame(datagen.params = datagen.params,
                        estimates = estimates)
  }else{
    outdf <- NULL
  }
  
  return(outdf)
}


