library(dplyr)
library(assertthat)  # All functions will require assertthat
library(rootSolve)
library(mvtnorm)

CalcTrueMarginalParams <- function(means, prop.zeros, cutoff, tot.time, rand.time){
  
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
  seq.names <- c("plus.r", "plus.nr.plus", "plus.nr.minus",
                 "minus.r", "minus.nr.plus", "minus.nr.minus")
  dtr.names <- c("plusplus","plusminus","minusplus","minusminus")
  
  blank.conditional.df <- data.frame(seq = seq.names,
                                     matrix(rep(NA, 6*input.tot.time), nrow=6, 
                                            dimnames = list(c(NULL),
                                                            c(paste("time.",1:tot.time,sep=""))
                                            )
                                     )
  )
  
  blank.marginal.df <- data.frame(DTR = dtr.names,
                                  matrix(rep(NA, 4*input.tot.time), nrow=4, 
                                         dimnames = list(c(NULL),
                                                         c(paste("time.",1:tot.time,sep=""))
                                         )
                                  )
  )
  
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
  true.gamma <- blank.conditional.df
  
  true.gamma[,idx.time.1] <- log(means[,idx.time.1])
  true.gamma[,(idx.time.1+1):idx.tot.time] <- log(means[,(idx.time.1+1):idx.tot.time]) - log(means[,idx.time.1])
  
  # True value of conditional means
  true.conditionalmeans <- blank.conditional.df
  
  true.conditionalmeans[,idx.time.1] <- exp(true.gamma[,idx.time.1])
  true.conditionalmeans[,(idx.time.1+1):idx.tot.time] <- exp(true.gamma[,idx.time.1] + true.gamma[,(idx.time.1+1):idx.tot.time])
  
  # True value of betas
  true.beta <- blank.marginal.df
  
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
  true.marginalmeans <- blank.marginal.df
  
  true.marginalmeans[,idx.time.1] <- exp(true.beta[,idx.time.1])
  true.marginalmeans[,(idx.time.1+1):idx.tot.time] <- exp(true.beta[,idx.time.1] + true.beta[,(idx.time.1+1):idx.tot.time])
  
  out.all <- list(true.beta = true.beta, 
                  true.gamma = true.gamma, 
                  true.conditionalmeans = true.conditionalmeans,
                  true.marginalmeans = true.marginalmeans)
  
  return(out.all)
}

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

MeltBeta <- function(df.rectangle, tot.time, rand.time){
  
  # Args:
  #   df.rectangle: ADDLATER
  #   tot.time: ADDLATER
  #   rand.time: ADDLATER
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  #ADDLATER
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  idx.time.1 <- which(colnames(df.rectangle)=="time.1")
  idx.tot.time <- tot.time + (idx.time.1-1)
  idx.rand.time <- rand.time + (idx.time.1-1)
  
  mat.column <- c(df.rectangle[df.rectangle$DTR=="plusplus", idx.time.1],
                  df.rectangle[df.rectangle$DTR=="plusplus", (idx.time.1+1):idx.rand.time],
                  df.rectangle[df.rectangle$DTR=="minusplus", (idx.time.1+1):idx.rand.time],
                  df.rectangle[df.rectangle$DTR=="plusplus", (idx.rand.time+1):idx.tot.time],
                  df.rectangle[df.rectangle$DTR=="plusminus", (idx.rand.time+1):idx.tot.time],
                  df.rectangle[df.rectangle$DTR=="minusplus", (idx.rand.time+1):idx.tot.time],
                  df.rectangle[df.rectangle$DTR=="minusminus", (idx.rand.time+1):idx.tot.time])
  
  mat.column <- as.matrix(unlist(mat.column))
  return(mat.column)
}

MeltC <- function(df.rectangle, tot.time, rand.time){
  
  # Args:
  #   df.rectangle: ADDLATER
  #   tot.time: ADDLATER
  #   rand.time: ADDLATER
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  #ADDLATER
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  idx.time.1 <- which(colnames(df.rectangle)=="time.1")
  idx.tot.time <- tot.time + (idx.time.1-1)
  idx.rand.time <- rand.time + (idx.time.1-1)
  
  mat.row.plusplus <- c(df.rectangle[df.rectangle$DTR=="plusplus", idx.time.1],
                        df.rectangle[df.rectangle$DTR=="plusplus", (idx.time.1+1):idx.rand.time],
                        rep(0, idx.rand.time-idx.time.1),
                        df.rectangle[df.rectangle$DTR=="plusplus", (idx.rand.time+1):idx.tot.time],
                        rep(0, idx.tot.time-idx.rand.time),
                        rep(0, idx.tot.time-idx.rand.time),
                        rep(0, idx.tot.time-idx.rand.time))
  
  mat.row.plusminus <- c(df.rectangle[df.rectangle$DTR=="plusplus", idx.time.1],
                         df.rectangle[df.rectangle$DTR=="plusplus", (idx.time.1+1):idx.rand.time],
                         rep(0, idx.rand.time-idx.time.1),
                         rep(0, idx.tot.time-idx.rand.time),
                         df.rectangle[df.rectangle$DTR=="plusminus", (idx.rand.time+1):idx.tot.time],
                         rep(0, idx.tot.time-idx.rand.time),
                         rep(0, idx.tot.time-idx.rand.time))
  
  mat.row.minusplus <- c(df.rectangle[df.rectangle$DTR=="plusplus", idx.time.1],
                         rep(0, idx.rand.time-idx.time.1),
                         df.rectangle[df.rectangle$DTR=="minusplus", (idx.time.1+1):idx.rand.time],
                         rep(0, idx.tot.time-idx.rand.time),
                         rep(0, idx.tot.time-idx.rand.time),
                         df.rectangle[df.rectangle$DTR=="minusplus", (idx.rand.time+1):idx.tot.time],
                         rep(0, idx.tot.time-idx.rand.time))
  
  mat.row.minusminus <- c(df.rectangle[df.rectangle$DTR=="plusplus", idx.time.1],
                          rep(0, idx.rand.time-idx.time.1),
                          df.rectangle[df.rectangle$DTR=="minusplus", (idx.time.1+1):idx.rand.time],
                          rep(0, idx.tot.time-idx.rand.time),
                          rep(0, idx.tot.time-idx.rand.time),
                          rep(0, idx.tot.time-idx.rand.time),
                          df.rectangle[df.rectangle$DTR=="minusminus", (idx.rand.time+1):idx.tot.time])
  
  mat.row.plusplus <- t(as.matrix(unlist(mat.row.plusplus)))
  mat.row.plusminus <- t(as.matrix(unlist(mat.row.plusminus)))
  mat.row.minusplus <- t(as.matrix(unlist(mat.row.minusplus)))
  mat.row.minusminus <- t(as.matrix(unlist(mat.row.minusminus)))
  
  mat.out <- rbind(mat.row.plusplus,
                   mat.row.plusminus,
                   mat.row.minusplus,
                   mat.row.minusminus)
  
  row.names(mat.out) <- c("plusplus", "plusminus", "minusplus", "minusminus")
  colnames(mat.out) <- NULL
  
  return(mat.out)
}


