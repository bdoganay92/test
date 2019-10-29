library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

# -----------------------------------------------------------------------------
# Reshape list of estimates
# -----------------------------------------------------------------------------
list.est.u.plusplus <- lapply(list.est.u.plusplus, ReshapeList)
list.est.u.plusminus <- lapply(list.est.u.plusminus, ReshapeList)
list.est.u.minusplus <- lapply(list.est.u.minusplus, ReshapeList)
list.est.u.minusminus <- lapply(list.est.u.minusminus, ReshapeList)

list.est.eos.means.plusplus <- lapply(list.est.eos.means.plusplus, ReshapeList)
list.est.eos.means.plusminus <- lapply(list.est.eos.means.plusminus, ReshapeList)
list.est.eos.means.minusplus <- lapply(list.est.eos.means.minusplus, ReshapeList)
list.est.eos.means.minusminus <- lapply(list.est.eos.means.minusminus, ReshapeList)

list.est.AUC.plusplus <- lapply(list.est.AUC.plusplus, ReshapeList)
list.est.AUC.plusminus <- lapply(list.est.AUC.plusminus, ReshapeList)
list.est.AUC.minusplus <- lapply(list.est.AUC.minusplus, ReshapeList)
list.est.AUC.minusminus <- lapply(list.est.AUC.minusminus, ReshapeList)

list.est.diff.eos.means.plusplus.plusminus <- lapply(list.est.diff.eos.means.plusplus.plusminus, ReshapeList)
list.est.diff.eos.means.plusplus.minusplus <- lapply(list.est.diff.eos.means.plusplus.minusplus, ReshapeList)
list.est.diff.eos.means.plusplus.minusminus <- lapply(list.est.diff.eos.means.plusplus.minusminus, ReshapeList)
list.est.diff.eos.means.plusminus.minusplus <- lapply(list.est.diff.eos.means.plusminus.minusplus, ReshapeList)
list.est.diff.eos.means.plusminus.minusminus <- lapply(list.est.diff.eos.means.plusminus.minusminus, ReshapeList)
list.est.diff.eos.means.minusminus.minusplus <- lapply(list.est.diff.eos.means.minusminus.minusplus, ReshapeList)

list.est.diff.AUC.plusplus.plusminus <- lapply(list.est.diff.AUC.plusplus.plusminus, ReshapeList)
list.est.diff.AUC.plusplus.minusplus <- lapply(list.est.diff.AUC.plusplus.minusplus, ReshapeList)
list.est.diff.AUC.plusplus.minusminus <- lapply(list.est.diff.AUC.plusplus.minusminus, ReshapeList)
list.est.diff.AUC.plusminus.minusplus <- lapply(list.est.diff.AUC.plusminus.minusplus, ReshapeList)
list.est.diff.AUC.plusminus.minusminus <- lapply(list.est.diff.AUC.plusminus.minusminus, ReshapeList)
list.est.diff.AUC.minusminus.minusplus <- lapply(list.est.diff.AUC.minusminus.minusplus, ReshapeList)

# -----------------------------------------------------------------------------
# Evaluate estimates
# -----------------------------------------------------------------------------

###############################################################################
# Evaluate estimates: bias of estimated betas
###############################################################################
list.out <- lapply(list.df.est.beta, 
                   function(x, 
                            list.true.vals = list(true.beta = beta.vec),
                            tot.time = input.tot.time,
                            rand.time = input.rand.time){
                     datagen.params <- x$datagen.params
                     converged <- x$estimates$converged
                     est.beta <- x$estimates$est.beta
                     
                     if(converged==1){
                       true.beta <- as.numeric(list.true.vals$true.beta)
                       bias <- true.beta - est.beta
                       outdf <- data.frame(datagen.params = datagen.params,
                                           betaj = 1:(4*tot.time-2*rand.time-1),
                                           bias=bias)
                     }else{
                       bias <- NA
                       outdf <- data.frame(datagen.params = datagen.params,
                                           betaj = 1:(4*tot.time-2*rand.time-1),
                                           bias=NA)
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, betaj) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE),
            n.not.converged=sum(is.na(bias)))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.betas <- list(bias.betas=grouped.out)

###############################################################################
# Evaluate estimates: bias of estimated means of time-specific outcomes by DTR
###############################################################################
list.all.est.bias.means <- list()

# DTR (+1,+1) -----------------------------------------------------------------
list.out <- lapply(list.est.u.plusplus,
                   function(x, 
                            list.true.vals = list(true.val = u.plusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                       bias <- list.true.vals$true.val - x$estimates
                       colnames(bias) <- "bias"
                       outdf$bias <- bias
                       outdf$measurement.occasion = 1:tot.time
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, measurement.occasion) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.plusplus=grouped.out))

# DTR (+1,-1) -----------------------------------------------------------------
list.out <- lapply(list.est.u.plusminus,
                   function(x, 
                            list.true.vals = list(true.val = u.plusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     outdf$measurement.occasion = 1:tot.time
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, measurement.occasion) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.plusminus=grouped.out))

# DTR (-1,+1) -----------------------------------------------------------------
list.out <- lapply(list.est.u.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = u.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     outdf$measurement.occasion = 1:tot.time
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, measurement.occasion) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.minusplus=grouped.out))

# DTR (-1,-1) -----------------------------------------------------------------
list.out <- lapply(list.est.u.minusminus,
                   function(x, 
                            list.true.vals = list(true.val = u.minusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     outdf$measurement.occasion = 1:tot.time
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, measurement.occasion) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.minusminus=grouped.out))

###############################################################################
# Evaluate estimates: bias of estimated quantities of scientific interest
###############################################################################
list.all.est.bias.AUC <- list()

# DTR (+1,+1) -----------------------------------------------------------------
list.out <- lapply(list.est.AUC.plusplus,
                   function(x, 
                            list.true.vals = list(true.val = AUC.plusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.AUC <- append(list.all.est.bias.AUC, list(bias.AUC.plusplus=grouped.out))

# DTR (+1,-1) -----------------------------------------------------------------
list.out <- lapply(list.est.AUC.plusminus,
                   function(x, 
                            list.true.vals = list(true.val = AUC.plusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.AUC <- append(list.all.est.bias.AUC, list(bias.AUC.plusminus=grouped.out))

# DTR (-1,+1) -----------------------------------------------------------------
list.out <- lapply(list.est.AUC.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = AUC.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.AUC <- append(list.all.est.bias.AUC, list(bias.AUC.minusplus=grouped.out))


# DTR (-1,-1) -----------------------------------------------------------------
list.out <- lapply(list.est.AUC.minusminus,
                   function(x, 
                            list.true.vals = list(true.val = AUC.minusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.AUC <- append(list.all.est.bias.AUC, list(bias.AUC.minusminus=grouped.out))

###############################################################################
# Evaluate estimates: differences in bias of estimated quantities of scientific 
# interest: End-of-study means
###############################################################################

list.all.est.bias.diffs <- list()

# DTR (+1,+1) vs. (+1, -1) ----------------------------------------------------
list.out <- lapply(list.est.diff.eos.means.plusplus.plusminus,
                   function(x, 
                            list.true.vals = list(true.val = diff.eos.means.plusplus.plusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.eos.means.plusplus.plusminus=grouped.out))

# DTR (+1,+1) vs. (-1, +1) ----------------------------------------------------
list.out <- lapply(list.est.diff.eos.means.plusplus.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = diff.eos.means.plusplus.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.eos.means.plusplus.minusplus=grouped.out))

# DTR (+1,+1) vs. (-1, -1) ----------------------------------------------------
list.out <- lapply(list.est.diff.eos.means.plusplus.minusminus,
                   function(x, 
                            list.true.vals = list(true.val = diff.eos.means.plusplus.minusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.eos.means.plusplus.minusminus=grouped.out))

# DTR (+1,-1) vs. (-1,+1) ----------------------------------------------------
list.out <- lapply(list.est.diff.eos.means.plusminus.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = diff.eos.means.plusminus.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.eos.means.plusminus.minusplus=grouped.out))

# DTR (+1,-1) vs. (-1,-1) ----------------------------------------------------
list.out <- lapply(list.est.diff.eos.means.plusminus.minusminus,
                   function(x, 
                            list.true.vals = list(true.val = diff.eos.means.plusminus.minusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.eos.means.plusminus.minusminus=grouped.out))

# DTR (-1,-1) vs. (-1,+1) ----------------------------------------------------
list.out <- lapply(list.est.diff.eos.means.minusminus.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = diff.eos.means.minusminus.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.eos.means.minusminus.minusplus=grouped.out))

# Combine all
list.all.est.bias.diffs.eos.means <- list.all.est.bias.diffs

###############################################################################
# Evaluate estimates: differences in bias of estimated quantities of scientific 
# interest: AUC
###############################################################################

list.all.est.bias.diffs <- list()

# DTR (+1,+1) vs. (+1, -1) ----------------------------------------------------
list.out <- lapply(list.est.diff.AUC.plusplus.plusminus,
                   function(x, 
                            list.true.vals = list(true.val = diff.AUC.plusplus.plusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.AUC.plusplus.plusminus=grouped.out))

# DTR (+1,+1) vs. (-1, +1) ----------------------------------------------------
list.out <- lapply(list.est.diff.AUC.plusplus.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = diff.AUC.plusplus.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.AUC.plusplus.minusplus=grouped.out))

# DTR (+1,+1) vs. (-1, -1) ----------------------------------------------------
list.out <- lapply(list.est.diff.AUC.plusplus.minusminus,
                   function(x, 
                            list.true.vals = list(true.val = diff.AUC.plusplus.minusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.AUC.plusplus.minusminus=grouped.out))

# DTR (+1,-1) vs. (-1,+1) ----------------------------------------------------
list.out <- lapply(list.est.diff.AUC.plusminus.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = diff.AUC.plusminus.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.AUC.plusminus.minusplus=grouped.out))

# DTR (+1,-1) vs. (-1,-1) ----------------------------------------------------
list.out <- lapply(list.est.diff.AUC.plusminus.minusminus,
                   function(x, 
                            list.true.vals = list(true.val = diff.AUC.plusminus.minusminus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.AUC.plusminus.minusminus=grouped.out))

# DTR (-1,-1) vs. (-1,+1) ----------------------------------------------------
list.out <- lapply(list.est.diff.AUC.minusminus.minusplus,
                   function(x, 
                            list.true.vals = list(true.val = diff.AUC.minusminus.minusplus),
                            tot.time = input.tot.time){
                     
                     if(!is.null(x)){outdf <- x
                     bias <- list.true.vals$true.val - x$estimates
                     colnames(bias) <- "bias"
                     outdf$bias <- bias
                     }else{
                       outdf <- NULL
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.diffs <- append(list.all.est.bias.diffs, list(bias.diff.AUC.minusminus.minusplus=grouped.out))

# Combine all
list.all.est.bias.diffs.AUC <- list.all.est.bias.diffs

# -----------------------------------------------------------------------------
# Aggregate results
# -----------------------------------------------------------------------------
list.all <- list(betas = list.all.est.bias.betas,
                 means = list.all.est.bias.means,
                 AUC = list.all.est.bias.AUC,
                 diffs.eos.means = list.all.est.bias.diffs.eos.means,
                 diffs.AUC = list.all.est.bias.diffs.AUC
                 )
list.all <- lapply(list.all, bind_rows)




