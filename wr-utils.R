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

