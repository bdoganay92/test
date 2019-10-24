library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
  
WeightAndReplicate <- function(DataLongFormat, tot.time){
  
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
  DummyRowsNotToReplicate <- RowsNotToReplicate
  DummyRowsNotToReplicate$t <- DummyRowsNotToReplicate$t + nwaves 
  DummyRowsNotToReplicate$Yit <- NA
  DummyRowsNotToReplicate$KnownWeight1 <- 0
  DummyRowsNotToReplicate$KnownWeight2 <- 0
  DummyRowsNotToReplicate$KnownWeight <- 0
  
  # We keep the same subject ID to show that we don't really have all those
  # new participants. So we have to distinguish the new observations somehow,
  # and so we treat them as new waves of data on the same person.
  # Create the final analysis dataset including replicates.
  data.for.analysis <- rbind(PlusOnePseudodata, 
                             MinusOnePseudodata, 
                             RowsNotToReplicate, 
                             DummyRowsNotToReplicate)
  data.for.analysis <- data.for.analysis[order(data.for.analysis$id,data.for.analysis$t),] 
  
  colnames(data.for.analysis) <- c("id", "wave", "A1", "A2", "R", "Y", 
                                   "KnownWeight1", "KnownWeight2", "KnownWeight", "ActualTime")
  
  return(data.for.analysis)
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
    est.beta <- as.matrix(model$beta)
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







