###############################################################################
# Calculate the value of the parameters gamma_j in our model for 
# log(E{Y_t^{s}})
###############################################################################
gamma.vec <- rep(NA,6*input.tot.time-4*input.rand.time-1)

# Take subset of input.means corresponding to each treatment sequence
# Treatment sequence by time 1
input.means.time.1 <- input.means[input.means$seq=="plus.r","time.1"]
# Treatment sequence by times 2 to input.rand.time
input.means.plus <- input.means[input.means$seq=="plus.r",paste("time.",2:input.rand.time,sep="")]
input.means.minus <- input.means[input.means$seq=="minus.r",paste("time.",2:input.rand.time,sep="")]
# Treatment sequence by time input.rand.time+1 to input.tot.time
input.means.plus.r <- input.means[input.means$seq=="plus.r",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.minus.r <- input.means[input.means$seq=="minus.r",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.plus.nr.plus <- input.means[input.means$seq=="plus.nr.plus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.plus.nr.minus <- input.means[input.means$seq=="plus.nr.minus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.minus.nr.plus <- input.means[input.means$seq=="minus.nr.plus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]
input.means.minus.nr.minus <- input.means[input.means$seq=="minus.nr.minus",paste("time.",(input.rand.time+1):input.tot.time,sep="")]

# Specify index of gamma_j parameters corresponding to each treatment sequence
# Treatment sequence by time 1
gamma.vec[1] <- log(input.means.time.1)
# Treatment sequence by times 2 to input.rand.time
gamma.idx.plus <- 2:input.rand.time
gamma.idx.minus <- (input.rand.time+1):(2*input.rand.time-1)
gamma.vec[gamma.idx.plus] <- log(input.means.plus) - gamma.vec[1]
gamma.vec[gamma.idx.minus] <- log(input.means.minus) - gamma.vec[1]
# Treatment sequence by time input.rand.time+1 to input.tot.time
gamma.idx.plus.r <- (2*input.rand.time):(input.tot.time+input.rand.time-1)
gamma.idx.minus.r <- (input.tot.time+input.rand.time):(2*input.tot.time-1)
gamma.idx.plus.nr.plus <- (2*input.tot.time):(3*input.tot.time-input.rand.time-1)
gamma.idx.plus.nr.minus <- (3*input.tot.time-input.rand.time):(4*input.tot.time-2*input.rand.time-1)
gamma.idx.minus.nr.plus <- (4*input.tot.time-2*input.rand.time):(5*input.tot.time-3*input.rand.time-1)
gamma.idx.minus.nr.minus <- (5*input.tot.time-3*input.rand.time):(6*input.tot.time-4*input.rand.time-1)
gamma.vec[gamma.idx.plus.r] <- log(input.means.plus.r) - gamma.vec[1]
gamma.vec[gamma.idx.minus.r] <- log(input.means.minus.r) - gamma.vec[1]
gamma.vec[gamma.idx.plus.nr.plus] <- log(input.means.plus.nr.plus) - gamma.vec[1]
gamma.vec[gamma.idx.plus.nr.minus] <- log(input.means.plus.nr.minus) - gamma.vec[1]
gamma.vec[gamma.idx.minus.nr.plus] <- log(input.means.minus.nr.plus) - gamma.vec[1]
gamma.vec[gamma.idx.minus.nr.minus] <- log(input.means.minus.nr.minus) - gamma.vec[1]
#gamma.vec <- bind_cols(gamma.vec)
#gamma.vec <- t(gamma.vec)
gamma.vec <- as.matrix(gammaa.vec)
row.names(gamma.vec) <- paste("gamma",1:(6*input.tot.time-4*input.rand.time-1),sep=".")

# Calculate response probabilities
mat.sigma2 <- matrix(rep(NA, 6*input.tot.time), nrow=6)
mat.sigma2 <- data.frame(seq = c("plus.r",
                                 "plus.nr.plus",
                                 "plus.nr.minus",
                                 "minus.r",
                                 "minus.nr.plus",
                                 "minus.nr.minus"),
                         mat.sigma2, row.names=NULL)
colnames(mat.sigma2) <- c("seq",paste("time.",1:input.tot.time,sep=""))

for(i in 1:6){
  for(j in 2:(input.tot.time+1)){
    mat.sigma2[i,j] <- SolveForSigmaSquared(input.mu = input.means[i,j], 
                                            input.prop.zeros = input.prop.zeros[i,j])
  }
}

remove(i,j)

###############################################################################
# Calculate proportion of responders to a1 using cutoff, mean and variance
# in outcome at rand.time for treatment sequences beginning with a1
###############################################################################
# Treatment sequences beginning with a1=+1
use.sigma2 <- mat.sigma2[mat.sigma2$seq=="plus.r",paste("time.",input.rand.time,sep="")]
use.mean <- input.means[input.means$seq=="plus.r",paste("time.",input.rand.time,sep="")]
p <- pnbinom(q = input.cutoff, size = 1/use.sigma2, mu = use.mean)
# Treatment sequences beginning with a1=-1
use.sigma2 <- mat.sigma2[mat.sigma2$seq=="minus.r",paste("time.",input.rand.time,sep="")]
use.mean <- input.means[input.means$seq=="minus.r",paste("time.",input.rand.time,sep="")]
q <- pnbinom(q = input.cutoff, size = 1/use.sigma2, mu = use.mean)
remove(use.sigma2, use.mean)

###############################################################################
# Calculate the value of the parameters beta_j in our model for 
# log(E{Y_t^{(a_1,a_2)}})
###############################################################################
beta.vec <- as.matrix(rep(NA,4*input.tot.time-2*input.rand.time-1))
row.names(beta.vec) <- paste("beta",1:(4*input.tot.time-2*input.rand.time-1),sep=".")
# DTR from time 1 to input.rand.time
beta.vec[1:(2*input.rand.time-1)] <- gamma.vec[1:(2*input.rand.time-1)] 
# DTR from time input.rand.time+1 to input.tot.time
idx.plusplus <- (2*input.rand.time):(input.tot.time+input.rand.time-1)
idx.plusminus <- (2*input.rand.time):(input.tot.time+input.rand.time-1)
idx.minusplus <- (input.tot.time+input.rand.time):(2*input.tot.time-1)
idx.minusminus <- (input.tot.time+input.rand.time):(2*input.tot.time-1)

mm.plusplus <- p*exp(gamma.vec[idx.plusplus])+(1-p)*exp(gamma.vec[idx.plusplus - 2*input.rand.time+2*input.tot.time])
mm.plusminus <- p*exp(gamma.vec[idx.plusminus])+(1-p)*exp(gamma.vec[idx.plusminus - 3*input.rand.time+3*input.tot.time])
mm.minusplus <- q*exp(gamma.vec[idx.minusplus])+(1-q)*exp(gamma.vec[idx.minusplus - 3*input.rand.time+3*input.tot.time])
mm.minusminus <- q*exp(gamma.vec[idx.minusminus])+(1-q)*exp(gamma.vec[idx.minusminus - 4*input.rand.time+4*input.tot.time])

beta.vec[idx.plusplus] <- log(mm.plusplus)
beta.vec[idx.plusminus+input.tot.time-input.rand.time] <- log(mm.plusminus)
beta.vec[idx.minusplus+input.tot.time-input.rand.time] <- log(mm.minusplus)
beta.vec[idx.minusminus+2*input.tot.time-2*input.rand.time] <- log(mm.minusminus)

