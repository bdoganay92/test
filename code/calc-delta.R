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

# Note that 
#   - list.df.potential
# need to be specified prior to running the code below

# -----------------------------------------------------------------------------
# Specify contrasts of interest
# -----------------------------------------------------------------------------
# Create C matrix
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
C.plusplus <- list.C$C.plusplus
C.plusminus <- list.C$C.plusminus
C.minusplus <- list.C$C.minusplus
C.minusminus <- list.C$C.minusminus

# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in AUC
for(i in 1:input.tot.time){
  if(input.tot.time==2){
    L.AUC <- (1/2)*t(eCol(1,input.tot.time)) + (1/2)*t(eCol(input.tot.time,input.tot.time))
  }else if(input.tot.time>2 & i==1){
    L.AUC <- (1/2)*t(eCol(1,input.tot.time))
  }else if(input.tot.time>2 & i==input.tot.time){
    L.AUC <- L.AUC+(1/2)*t(eCol(input.tot.time,input.tot.time))
  }else{
    L.AUC <- L.AUC+t(eCol(i,input.tot.time))
  }
}
D.AUC <- cbind(L.AUC,-L.AUC)

# Difference in change score
L.change.score <- -t(eCol(input.rand.time, input.tot.time)) + t(eCol(input.tot.time, input.tot.time))
D.change.score <- cbind(L.change.score, -L.change.score)

# -----------------------------------------------------------------------------
# Calculate delta
# -----------------------------------------------------------------------------
ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "path.output_data",
                    "list.df.potential",
                    "L.AUC",
                    "L.eos.means",
                    "L.change.score"))
clusterEvalQ(cl,
             {
               library(dplyr)
               library(assertthat)
               library(rootSolve)
               library(mvtnorm)
               library(geeM)
               source(file.path(path.code, "input-utils.R"))
               source(file.path(path.code, "datagen-utils.R"))
               source(file.path(path.code, "analysis-utils.R"))
             })

list.delta.eos.means <- parLapply(cl=cl, list.df.potential, CalcDeltaj, L=L.eos.means)
list.delta.AUC <- parLapply(cl=cl, list.df.potential, CalcDeltaj, L=L.AUC)
list.delta.change.score <- parLapply(cl=cl, list.df.potential, CalcDeltaj, L=L.change.score)
stopCluster(cl)

###############################################################################
# Aggregate results for end-of-study means
###############################################################################
list.delta.eos.means.plusplus.plusminus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.plusminus"])})
list.delta.eos.means.plusplus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.minusplus"])})
list.delta.eos.means.plusplus.minusminus <- lapply(list.delta.eos.means, function(x){return(x["plusplus.minusminus"])})
list.delta.eos.means.plusminus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["plusminus.minusplus"])})
list.delta.eos.means.plusminus.minusminus <- lapply(list.delta.eos.means, function(x){return(x["plusminus.minusminus"])})
list.delta.eos.means.minusminus.minusplus <- lapply(list.delta.eos.means, function(x){return(x["minusminus.minusplus"])})

delta.eos.means.plusplus.plusminus <- lapply(list.delta.eos.means.plusplus.plusminus, ReshapeList)
delta.eos.means.plusplus.plusminus <- bind_rows(delta.eos.means.plusplus.plusminus)
delta.eos.means.plusplus.plusminus <- delta.eos.means.plusplus.plusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusplus.minusplus <- lapply(list.delta.eos.means.plusplus.minusplus, ReshapeList)
delta.eos.means.plusplus.minusplus <- bind_rows(delta.eos.means.plusplus.minusplus)
delta.eos.means.plusplus.minusplus <- delta.eos.means.plusplus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusplus.minusminus <- lapply(list.delta.eos.means.plusplus.minusminus, ReshapeList)
delta.eos.means.plusplus.minusminus <- bind_rows(delta.eos.means.plusplus.minusminus)
delta.eos.means.plusplus.minusminus <- delta.eos.means.plusplus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusminus.minusplus <- lapply(list.delta.eos.means.plusminus.minusplus, ReshapeList)
delta.eos.means.plusminus.minusplus <- bind_rows(delta.eos.means.plusminus.minusplus)
delta.eos.means.plusminus.minusplus <- delta.eos.means.plusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.plusminus.minusminus <- lapply(list.delta.eos.means.plusminus.minusminus, ReshapeList)
delta.eos.means.plusminus.minusminus <- bind_rows(delta.eos.means.plusminus.minusminus)
delta.eos.means.plusminus.minusminus <- delta.eos.means.plusminus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.eos.means.minusminus.minusplus <- lapply(list.delta.eos.means.minusminus.minusplus, ReshapeList)
delta.eos.means.minusminus.minusplus <- bind_rows(delta.eos.means.minusminus.minusplus)
delta.eos.means.minusminus.minusplus <- delta.eos.means.minusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

list.delta.eos.means <- list(delta.eos.means.plusplus.plusminus=delta.eos.means.plusplus.plusminus,
                             delta.eos.means.plusplus.minusplus=delta.eos.means.plusplus.minusplus,
                             delta.eos.means.plusplus.minusminus=delta.eos.means.plusplus.minusminus,
                             delta.eos.means.plusminus.minusplus=delta.eos.means.plusminus.minusplus,
                             delta.eos.means.plusminus.minusminus=delta.eos.means.plusminus.minusminus,
                             delta.eos.means.minusminus.minusplus=delta.eos.means.minusminus.minusplus)

###############################################################################
# Aggregate results for AUC
###############################################################################
list.delta.AUC.plusplus.plusminus <- lapply(list.delta.AUC, function(x){return(x["plusplus.plusminus"])})
list.delta.AUC.plusplus.minusplus <- lapply(list.delta.AUC, function(x){return(x["plusplus.minusplus"])})
list.delta.AUC.plusplus.minusminus <- lapply(list.delta.AUC, function(x){return(x["plusplus.minusminus"])})
list.delta.AUC.plusminus.minusplus <- lapply(list.delta.AUC, function(x){return(x["plusminus.minusplus"])})
list.delta.AUC.plusminus.minusminus <- lapply(list.delta.AUC, function(x){return(x["plusminus.minusminus"])})
list.delta.AUC.minusminus.minusplus <- lapply(list.delta.AUC, function(x){return(x["minusminus.minusplus"])})


delta.AUC.plusplus.plusminus <- lapply(list.delta.AUC.plusplus.plusminus, ReshapeList)
delta.AUC.plusplus.plusminus <- bind_rows(delta.AUC.plusplus.plusminus)
delta.AUC.plusplus.plusminus <- delta.AUC.plusplus.plusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusplus.minusplus <- lapply(list.delta.AUC.plusplus.minusplus, ReshapeList)
delta.AUC.plusplus.minusplus <- bind_rows(delta.AUC.plusplus.minusplus)
delta.AUC.plusplus.minusplus <- delta.AUC.plusplus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusplus.minusminus <- lapply(list.delta.AUC.plusplus.minusminus, ReshapeList)
delta.AUC.plusplus.minusminus <- bind_rows(delta.AUC.plusplus.minusminus)
delta.AUC.plusplus.minusminus <- delta.AUC.plusplus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusminus.minusplus <- lapply(list.delta.AUC.plusminus.minusplus, ReshapeList)
delta.AUC.plusminus.minusplus <- bind_rows(delta.AUC.plusminus.minusplus)
delta.AUC.plusminus.minusplus <- delta.AUC.plusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.plusminus.minusminus <- lapply(list.delta.AUC.plusminus.minusminus, ReshapeList)
delta.AUC.plusminus.minusminus <- bind_rows(delta.AUC.plusminus.minusminus)
delta.AUC.plusminus.minusminus <- delta.AUC.plusminus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.AUC.minusminus.minusplus <- lapply(list.delta.AUC.minusminus.minusplus, ReshapeList)
delta.AUC.minusminus.minusplus <- bind_rows(delta.AUC.minusminus.minusplus)
delta.AUC.minusminus.minusplus <- delta.AUC.minusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

list.delta.AUC <- list(delta.AUC.plusplus.plusminus=delta.AUC.plusplus.plusminus,
                       delta.AUC.plusplus.minusplus=delta.AUC.plusplus.minusplus,
                       delta.AUC.plusplus.minusminus=delta.AUC.plusplus.minusminus,
                       delta.AUC.plusminus.minusplus=delta.AUC.plusminus.minusplus,
                       delta.AUC.plusminus.minusminus=delta.AUC.plusminus.minusminus,
                       delta.AUC.minusminus.minusplus=delta.AUC.minusminus.minusplus)

###############################################################################
# Aggregate results for change score
###############################################################################
list.delta.change.score.plusplus.plusminus <- lapply(list.delta.change.score, function(x){return(x["plusplus.plusminus"])})
list.delta.change.score.plusplus.minusplus <- lapply(list.delta.change.score, function(x){return(x["plusplus.minusplus"])})
list.delta.change.score.plusplus.minusminus <- lapply(list.delta.change.score, function(x){return(x["plusplus.minusminus"])})
list.delta.change.score.plusminus.minusplus <- lapply(list.delta.change.score, function(x){return(x["plusminus.minusplus"])})
list.delta.change.score.plusminus.minusminus <- lapply(list.delta.change.score, function(x){return(x["plusminus.minusminus"])})
list.delta.change.score.minusminus.minusplus <- lapply(list.delta.change.score, function(x){return(x["minusminus.minusplus"])})

delta.change.score.plusplus.plusminus <- lapply(list.delta.change.score.plusplus.plusminus, ReshapeList)
delta.change.score.plusplus.plusminus <- bind_rows(delta.change.score.plusplus.plusminus)
delta.change.score.plusplus.plusminus <- delta.change.score.plusplus.plusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.change.score.plusplus.minusplus <- lapply(list.delta.change.score.plusplus.minusplus, ReshapeList)
delta.change.score.plusplus.minusplus <- bind_rows(delta.change.score.plusplus.minusplus)
delta.change.score.plusplus.minusplus <- delta.change.score.plusplus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.change.score.plusplus.minusminus <- lapply(list.delta.change.score.plusplus.minusminus, ReshapeList)
delta.change.score.plusplus.minusminus <- bind_rows(delta.change.score.plusplus.minusminus)
delta.change.score.plusplus.minusminus <- delta.change.score.plusplus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.change.score.plusminus.minusplus <- lapply(list.delta.change.score.plusminus.minusplus, ReshapeList)
delta.change.score.plusminus.minusplus <- bind_rows(delta.change.score.plusminus.minusplus)
delta.change.score.plusminus.minusplus <- delta.change.score.plusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.change.score.plusminus.minusminus <- lapply(list.delta.change.score.plusminus.minusminus, ReshapeList)
delta.change.score.plusminus.minusminus <- bind_rows(delta.change.score.plusminus.minusminus)
delta.change.score.plusminus.minusminus <- delta.change.score.plusminus.minusminus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

delta.change.score.minusminus.minusplus <- lapply(list.delta.change.score.minusminus.minusplus, ReshapeList)
delta.change.score.minusminus.minusplus <- bind_rows(delta.change.score.minusminus.minusplus)
delta.change.score.minusminus.minusplus <- delta.change.score.minusminus.minusplus %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(delta = mean(estimates, na.rm=TRUE))

list.delta.change.score <- list(delta.change.score.plusplus.plusminus=delta.change.score.plusplus.plusminus,
                                delta.change.score.plusplus.minusplus=delta.change.score.plusplus.minusplus,
                                delta.change.score.plusplus.minusminus=delta.change.score.plusplus.minusminus,
                                delta.change.score.plusminus.minusplus=delta.change.score.plusminus.minusplus,
                                delta.change.score.plusminus.minusminus=delta.change.score.plusminus.minusminus,
                                delta.change.score.minusminus.minusplus=delta.change.score.minusminus.minusplus)

###############################################################################
# Aggregate all results
###############################################################################
delta.eos.means <- bind_rows(list.delta.eos.means)
delta.AUC <- bind_rows(list.delta.AUC)
delta.change.score <- bind_rows(list.delta.change.score)

delta.eos.means$pair <- 1:6
delta.AUC$pair <- 1:6
delta.change.score$pair <- 1:6

