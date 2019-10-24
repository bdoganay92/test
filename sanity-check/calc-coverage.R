library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")




# -----------------------------------------------------------------------------
# Estimate the standard error of differences in AUC from 
# time 1 to input.tot.time for pairs of DTRs: Dexp[C^{(a1,a2)}beta]
# -----------------------------------------------------------------------------
list.est.stderr.AUC.diff.plusplus.plusminus <- list()
list.est.stderr.AUC.diff.plusplus.minusplus <- list()
list.est.stderr.AUC.diff.plusplus.minusminus <- list()
list.est.stderr.AUC.diff.plusminus.minusplus <- list()
list.est.stderr.AUC.diff.plusminus.minusminus <- list()
list.est.stderr.AUC.diff.minusminus.minusplus <- list()

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

for(i in idx.nsim){
  if(list.df.est.beta[[i]]$converged==1){
    est.cov.beta <- list.df.est.beta[[i]]$est.cov.beta
    est.cov.beta <- as.matrix(est.cov.beta)
    # DTR (+1,+1) vs. (+1,-1)
    est.cov.AUC.diff.plusplus.plusminus <- (D.AUC %*% U.plusplus.plusminus %*% stacked.C.plusplus.plusminus) %*% est.cov.beta %*% t(D.AUC %*% U.plusplus.plusminus %*% stacked.C.plusplus.plusminus) 
    est.stderr.AUC.diff.plusplus.plusminus <- sqrt(est.cov.AUC.diff.plusplus.plusminus)
    est.stderr.AUC.diff.plusplus.plusminus <- list(data.frame(est.stderr.AUC.diff.plusplus.plusminus = est.stderr.AUC.diff.plusplus.plusminus))
    # DTR (+1,+1) vs. (-1,+1)
    est.cov.AUC.diff.plusplus.minusplus <- (D.AUC %*% U.plusplus.minusplus %*% stacked.C.plusplus.minusplus) %*% est.cov.beta %*% t(D.AUC %*% U.plusplus.minusplus %*% stacked.C.plusplus.minusplus) 
    est.stderr.AUC.diff.plusplus.minusplus <- sqrt(est.cov.AUC.diff.plusplus.minusplus)
    est.stderr.AUC.diff.plusplus.minusplus <- list(data.frame(est.stderr.AUC.diff.plusplus.minusplus = est.stderr.AUC.diff.plusplus.minusplus))
    # DTR (+1,+1) vs. (-1,-1)
    est.cov.AUC.diff.plusplus.minusminus <- (D.AUC %*% U.plusplus.minusminus %*% stacked.C.plusplus.minusminus) %*% est.cov.beta %*% t(D.AUC %*% U.plusplus.minusminus %*% stacked.C.plusplus.minusminus) 
    est.stderr.AUC.diff.plusplus.minusminus <- sqrt(est.cov.AUC.diff.plusplus.minusminus)
    est.stderr.AUC.diff.plusplus.minusminus <- list(data.frame(est.stderr.AUC.diff.plusplus.minusminus = est.stderr.AUC.diff.plusplus.minusminus))
    # DTR (+1,-1) vs. (-1,+1)
    est.cov.AUC.diff.plusminus.minusplus <- (D.AUC %*% U.plusminus.minusplus %*% stacked.C.plusminus.minusplus) %*% est.cov.beta %*% t(D.AUC %*% U.plusminus.minusplus %*% stacked.C.plusminus.minusplus) 
    est.stderr.AUC.diff.plusminus.minusplus <- sqrt(est.cov.AUC.diff.plusminus.minusplus)
    est.stderr.AUC.diff.plusminus.minusplus <- list(data.frame(est.stderr.AUC.diff.plusminus.minusplus = est.stderr.AUC.diff.plusminus.minusplus))
    # DTR (+1,-1) vs. (-1,-1)
    est.cov.AUC.diff.plusminus.minusminus <- (D.AUC %*% U.plusminus.minusminus %*% stacked.C.plusminus.minusminus) %*% est.cov.beta %*% t(D.AUC %*% U.plusminus.minusminus %*% stacked.C.plusminus.minusminus) 
    est.stderr.AUC.diff.plusminus.minusminus <- sqrt(est.cov.AUC.diff.plusminus.minusminus)
    est.stderr.AUC.diff.plusminus.minusminus <- list(data.frame(est.stderr.AUC.diff.plusminus.minusminus = est.stderr.AUC.diff.plusminus.minusminus))
    # DTR (-1,-1) vs. (-1,+1)
    est.cov.AUC.diff.minusminus.minusplus <- (D.AUC %*% U.minusminus.minusplus %*% stacked.C.minusminus.minusplus) %*% est.cov.beta %*% t(D.AUC %*% U.minusminus.minusplus %*% stacked.C.minusminus.minusplus) 
    est.stderr.AUC.diff.minusminus.minusplus <- sqrt(est.cov.AUC.diff.minusminus.minusplus)
    est.stderr.AUC.diff.minusminus.minusplus <- list(data.frame(est.stderr.AUC.diff.minusminus.minusplus = est.stderr.AUC.diff.minusminus.minusplus))
  }else{
    list.est.stderr.AUC.diff.plusplus.plusminus <- list(data.frame(NULL))
    list.est.stderr.AUC.diff.plusplus.minusplus <- list(data.frame(NULL))
    list.est.stderr.AUC.diff.plusplus.minusminus <- list(data.frame(NULL))
    list.est.stderr.AUC.diff.plusminus.minusplus <- list(data.frame(NULL))
    list.est.stderr.AUC.diff.plusminus.minusminus <- list(data.frame(NULL))
    list.est.stderr.AUC.diff.minusminus.minusplus <- list(data.frame(NULL))
  }
  list.est.stderr.AUC.diff.plusplus.plusminus <- append(list.est.stderr.AUC.diff.plusplus.plusminus, 
                                                        est.stderr.AUC.diff.plusplus.plusminus)
  list.est.stderr.AUC.diff.plusplus.minusplus <- append(list.est.stderr.AUC.diff.plusplus.minusplus,
                                                        est.stderr.AUC.diff.plusplus.minusplus)
  list.est.stderr.AUC.diff.plusplus.minusminus <- append(list.est.stderr.AUC.diff.plusplus.minusminus,
                                                         est.stderr.AUC.diff.plusplus.minusminus)
  list.est.stderr.AUC.diff.plusminus.minusplus <- append(list.est.stderr.AUC.diff.plusminus.minusplus,
                                                         est.stderr.AUC.diff.plusminus.minusplus)
  list.est.stderr.AUC.diff.plusminus.minusminus <- append(list.est.stderr.AUC.diff.plusminus.minusminus,
                                                          est.stderr.AUC.diff.plusminus.minusminus)
  list.est.stderr.AUC.diff.minusminus.minusplus <- append(list.est.stderr.AUC.diff.minusminus.minusplus,
                                                          est.stderr.AUC.diff.minusminus.minusplus)
}



est.stderr.AUC.diff.plusplus.minusminus <- do.call(rbind,list.est.stderr.AUC.diff.plusplus.minusminus)
est.AUC.diff.plusplus.minusminus <- do.call(rbind,list.est.AUC.diff.plusplus.minusminus)

UB <- est.AUC.diff.plusplus.minusminus + 1.96*est.stderr.AUC.diff.plusplus.minusminus
LB <- est.AUC.diff.plusplus.minusminus - 1.96*est.stderr.AUC.diff.plusplus.minusminus

coverage <- ((as.numeric(AUC.diff.plusplus.minusminus)>LB) & (as.numeric(AUC.diff.plusplus.minusminus)<UB))
coverage <- mean(coverage)
print(coverage)


power <- (0>LB) & (0<UB)
power <- 1-mean(power)
print(power)







ncore <- detectCores()
cl <- makeCluster(ncore - 1)
clusterSetRNGStream(cl, 102399)
clusterExport(cl, c("path.code",
                    "path.input_data",
                    "list.gridx",
                    "L.AUC",
                    "L.eos.means"))
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

list.delta.AUC <- parLapply(cl=cl, list.df.potential, CalcDeltaj, L=L.AUC)
list.delta.eos.means <- parLapply(cl=cl, list.df.potential, CalcDeltaj, L=L.eos.means)

stopCluster(cl)



delta.AUC <- bind_cols(list.delta.AUC)
delta.AUC <- rowMeans(delta.AUC)
delta.AUC <- t(delta.AUC)


delta.eos.means <- bind_cols(list.delta.eos.means)
delta.eos.means <- rowMeans(delta.eos.means)
delta.eos.means <- t(delta.eos.means)


print(delta.AUC)
print(delta.eos.means)



