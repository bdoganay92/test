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

list.all.est.bias.means <- list()

# -----------------------------------------------------------------------------
# Evaluate estimates: bias of estimated betas
# -----------------------------------------------------------------------------
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

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.betas=grouped.out))

# -----------------------------------------------------------------------------
# Evaluate estimates: bias of estimated means of time-specific outcomes by DTR
# -----------------------------------------------------------------------------

###############################################################################
# DTR (+1,+1)
###############################################################################
list.out <- lapply(list.est.u.plusplus, 
                   function(x, 
                            list.true.vals = list(true.val = u.plusplus),
                            tot.time = input.tot.time){
                     datagen.params <- x[[1]]$datagen.params
                     est <- x[[1]]$estimates
                     true.val <- list.true.vals$true.val
                     
                     if(nrow(est)>0){
                       bias <- true.val - est
                       colnames(bias) <- "bias"
                       
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = bias)
                     }else{
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = NA)
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, seq) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.plusplus=grouped.out))

###############################################################################
# DTR (+1,-1)
###############################################################################
list.out <- lapply(list.est.u.plusminus, 
                   function(x, 
                            list.true.vals = list(true.val = u.plusminus),
                            tot.time = input.tot.time){
                     datagen.params <- x[[1]]$datagen.params
                     est <- x[[1]]$estimates
                     true.val <- list.true.vals$true.val
                     
                     if(nrow(est)>0){
                       bias <- true.val - est
                       colnames(bias) <- "bias"
                       
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = bias)
                     }else{
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = NA)
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, seq) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.plusminus=grouped.out))

###############################################################################
# DTR (-1,+1)
###############################################################################
list.out <- lapply(list.est.u.minusplus, 
                   function(x, 
                            list.true.vals = list(true.val = u.minusplus),
                            tot.time = input.tot.time){
                     datagen.params <- x[[1]]$datagen.params
                     est <- x[[1]]$estimates
                     true.val <- list.true.vals$true.val
                     
                     if(nrow(est)>0){
                       bias <- true.val - est
                       colnames(bias) <- "bias"
                       
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = bias)
                     }else{
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = NA)
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, seq) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.minusplus=grouped.out))

###############################################################################
# DTR (-1,-1) 
###############################################################################
list.out <- lapply(list.est.u.minusminus, 
                   function(x, 
                            list.true.vals = list(true.val = u.minusminus),
                            tot.time = input.tot.time){
                     datagen.params <- x[[1]]$datagen.params
                     est <- x[[1]]$estimates
                     true.val <- list.true.vals$true.val
                     
                     if(nrow(est)>0){
                       bias <- true.val - est
                       colnames(bias) <- "bias"
                       
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = bias)
                     }else{
                       outdf <- data.frame(datagen.params = datagen.params,
                                           seq = 1:tot.time,
                                           bias = NA)
                     }
                     return(outdf)
                   })

out <- bind_rows(list.out)
grouped.out <- out %>% 
  group_by(datagen.params.N, datagen.params.rho, seq) %>% 
  summarise(est.bias=mean(bias, na.rm=TRUE))
grouped.out <- round(grouped.out, digits = 3)

list.all.est.bias.means <- append(list.all.est.bias.means, list(bias.u.minusminus=grouped.out))


