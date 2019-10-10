library(dplyr)
library(assertthat)  # All functions will require assertthat
library(rootSolve)
library(mvtnorm)

AnalyzeData <- function(df.replicated.observed.Yit, tot.time, rand.time){
  
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
  
  fo.int <- "1"
  fo.plus <- paste("I(ActualTime==", 2:rand.time, " & A1==1)", sep="")
  fo.minus <- paste("I(ActualTime==", 2:rand.time, " & A1==-1)", sep="")
  fo.plusplus <- paste("I(ActualTime==", (rand.time+1):tot.time, " & A1==1 & A2==1)", sep="") 
  fo.plusminus <- paste("I(ActualTime==", (rand.time+1):tot.time, " & A1==1 & A2==-1)", sep="") 
  fo.minusplus <- paste("I(ActualTime==", (rand.time+1):tot.time, " & A1==-1 & A2==1)", sep="") 
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
  
  model <- geem(formula = fo,
                data = df.replicated.observed.Yit, 
                id = id,
                waves = wave, # No missing data 
                family = poisson,
                corstr = "independence",
                weights = KnownWeight,
                scale.fix = TRUE)
  
  if(model$converged == TRUE){
    # Calculate estimates of the mean per DTR and time
    est.beta <- (model$beta)
    coefnames <- (model$coefnames)
    est.cov.beta <- (model$var)
    converged <- 1*(model$converged==TRUE)
    est <- list(est.beta = est.beta, 
                coefnames = coefnames,
                est.cov.beta = est.cov.beta, 
                converged = converged)
  }else{
    est <- list(est.beta=NA,
                coefnames=NA,
                est.cov.beta=NA,
                converged=0)
  }
  
  return(est)
}

CalcMarginalParams <- function(means, prop.zeros, cutoff, tot.time, rand.time){
  
  # Args:
  #   means: ADDLATER
  #   prop.zeros: ADDLATER
  #   cutoff: ADDLATER
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  #ADDLATER
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  idx.time.1 <- which(colnames(means)=="time.1")
  idx.tot.time <- tot.time + (idx.time.1-1)
  idx.rand.time <- rand.time + (idx.time.1-1)
  means[,"seq"] <- as.character(means[,"seq"])
  prop.zeros[,"seq"] <- as.character(prop.zeros[,"seq"])
  dtr.names <- c("plusplus","plusminus","minusplus","minusminus")
  
  sigma2 <- means
  sigma2[,idx.time.1:idx.tot.time] <- NA 
  for(i in 1:nrow(means)){
    use.mu <- as.numeric(means[i,idx.time.1:idx.tot.time])
    use.prop.zeros <- as.numeric(prop.zeros[i,idx.time.1:idx.tot.time])
    sigma2[,idx.time.1:idx.tot.time] <- mapply(FUN=GetVariance, 
                                               input.mu = use.mu, 
                                               input.prop.zeros = use.prop.zeros)
  }
  
  # Calculate probabilities of response
  # Calculate proportion of responders to A1=+1 using cutoff, mean and variance
  # in outcome at rand.time for treatment sequences beginning with A1=+1
  use.var <- sigma2[sigma2$seq=="plus.r",paste("time.",rand.time,sep="")]
  use.mean <- means[means$seq=="plus.r",paste("time.",rand.time,sep="")]
  p <- pnbinom(q = cutoff, size = 1/use.var, mu = use.mean)
  remove(use.var, use.mean)
  
  # Calculate proportion of responders to A1=-1 using cutoff, mean and variance
  # in outcome at rand.time for treatment sequences beginning with A1=-1
  use.var <- sigma2[sigma2$seq=="minus.r",paste("time.",rand.time,sep="")]
  use.mean <- means[means$seq=="minus.r",paste("time.",rand.time,sep="")]
  q <- pnbinom(q = cutoff, size = 1/use.var, mu = use.mean)
  remove(use.var, use.mean)
  
  # True value of gammas
  true.gamma <- means
  true.gamma[,idx.time.1:idx.tot.time] <- NA 
  true.gamma[,idx.time.1] <- log(means[,idx.time.1])
  true.gamma[,(idx.time.1+1):idx.tot.time] <- log(means[,(idx.time.1+1):idx.tot.time]) - log(means[,idx.time.1])
  
  # True value of betas
  true.beta <- matrix(rep(NA_real_,4*tot.time), byrow=TRUE, ncol=tot.time)
  colnames(true.beta) <- paste("time.",1:tot.time,sep="")
  true.beta <- data.frame(DTR = dtr.names, true.beta)
  
  true.beta[,idx.time.1] <- true.gamma[1:4, idx.time.1]
  
  g.plus <- true.gamma[true.gamma$seq=="plus.r", (idx.time.1+1):idx.rand.time]
  g.minus <- true.gamma[true.gamma$seq=="minus.r", (idx.time.1+1):idx.rand.time]
  
  true.beta[true.beta$DTR=="plusplus", (idx.time.1+1):idx.rand.time] <- g.plus
  true.beta[true.beta$DTR=="plusminus", (idx.time.1+1):idx.rand.time] <- g.plus
  true.beta[true.beta$DTR=="minusplus", (idx.time.1+1):idx.rand.time] <- g.minus
  true.beta[true.beta$DTR=="minusminus", (idx.time.1+1):idx.rand.time] <- g.minus
  
  g.plus.r <- true.gamma[true.gamma$seq=="plus.r", (idx.rand.time+1):idx.tot.time]
  g.plus.nr.plus <- true.gamma[true.gamma$seq=="plus.nr.plus", (idx.rand.time+1):idx.tot.time]
  g.plus.nr.minus <- true.gamma[true.gamma$seq=="plus.nr.minus", (idx.rand.time+1):idx.tot.time]
  
  g.minus.r <- true.gamma[true.gamma$seq=="minus.r", (idx.rand.time+1):idx.tot.time]
  g.minus.nr.plus <- true.gamma[true.gamma$seq=="minus.nr.plus", (idx.rand.time+1):idx.tot.time]
  g.minus.nr.minus <- true.gamma[true.gamma$seq=="minus.nr.minus", (idx.rand.time+1):idx.tot.time]
  
  true.beta[true.beta$DTR=="plusplus", (idx.rand.time+1):idx.tot.time] <- log(p*exp(g.plus.r) + (1-p)*exp(g.plus.nr.plus))
  true.beta[true.beta$DTR=="plusminus", (idx.rand.time+1):idx.tot.time] <- log(p*exp(g.plus.r) + (1-p)*exp(g.plus.nr.minus))
  true.beta[true.beta$DTR=="minusplus", (idx.rand.time+1):idx.tot.time] <- log(q*exp(g.minus.r) + (1-q)*exp(g.minus.nr.plus))
  true.beta[true.beta$DTR=="minusminus", (idx.rand.time+1):idx.tot.time] <- log(q*exp(g.minus.r) + (1-q)*exp(g.minus.nr.minus))
  
  # True value of marginal means
  true.marginalmeans <- matrix(rep(NA_real_,4*tot.time), byrow=TRUE, ncol=tot.time)
  colnames(true.marginalmeans) <- paste("time.",1:tot.time,sep="")
  true.marginalmeans <- data.frame(DTR = dtr.names, true.marginalmeans)
  true.marginalmeans[,idx.time.1] <- exp(true.beta[,idx.time.1])
  true.marginalmeans[,(idx.time.1+1):idx.tot.time] <- exp(true.beta[,(idx.time.1+1):idx.tot.time])
  
  out.all <- list(true.beta = true.beta, 
                  true.gamma = true.gamma, 
                  true.marginalmeans = true.marginalmeans)
  
  return(out.all)
}


