library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")

source(file.path(path.code, "input-utils.R"))
source(file.path(path.code, "datagen-utils.R"))

###############################################################################
# No manual inputs after this line
###############################################################################

# Calculate dimensions of correlation matrices among four groups
corrdim.01 <- 2*tot.time - 1
corrdim.02 <- 3*tot.time - rand.time - 1
corrdim.03 <- 3*tot.time - rand.time - 1
corrdim.04 <- 4*tot.time - 2*rand.time - 1

# Create correlation matrix for each group
list.corrmat <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
list.corrmat$group.01 <- ExchangeableMat(m = corrdim.01, rho = rho)
list.corrmat$group.02 <- ExchangeableMat(m = corrdim.02, rho = rho)
list.corrmat$group.03 <- ExchangeableMat(m = corrdim.03, rho = rho)
list.corrmat$group.04 <- ExchangeableMat(m = corrdim.04, rho = rho)

# input.means contains mean outcome under each treatment sequence
# from time 1 until tot.time
list.mu <- ReadInput(input.df = input.means, tot.time = tot.time, rand.time = rand.time)

# input.prop.zeros contains proportion of zeros in the outcome under each 
# treatment sequence from time 1 until tot.time
list.tau <- ReadInput(input.df = input.prop.zeros, tot.time = tot.time, rand.time = rand.time)

# Under the working assumption that the outcome under each treatment sequence
# from time 1 until tot.time is negative binomial distributed, we use means
# in input.means and proportion of zeros in input.prop.zeros to calculate the
# variance in the outcome under each treatment sequence from time 1 until
# tot.time
list.sigma2 <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
list.sigma2$group.01 <- GetVarianceByGroup(all.mu = list.mu$group.01, all.tau = list.tau$group.01)
list.sigma2$group.02 <- GetVarianceByGroup(all.mu = list.mu$group.02, all.tau = list.tau$group.02)
list.sigma2$group.03 <- GetVarianceByGroup(all.mu = list.mu$group.03, all.tau = list.tau$group.03)
list.sigma2$group.04 <- GetVarianceByGroup(all.mu = list.mu$group.04, all.tau = list.tau$group.04)

# Calculate proportion of responders to A1=+1 using cutoff, mean and variance
# in outcome at rand.time for treatment sequences beginning with A1=+1
use.var <- tail(list.sigma2$group.01$plus, n=1)
use.mean <- tail(as.numeric(list.mu$group.01$plus), n=1)
p <- pnbinom(q = cutoff, size = 1/use.var, mu = use.mean)
remove(use.var, use.mean)

# Calculate proportion of responders to A1=-1 using cutoff, mean and variance
# in outcome at rand.time for treatment sequences beginning with A1=-1
use.var <- tail(list.sigma2$group.01$minus, n=1)
use.mean <- tail(as.numeric(list.mu$group.01$minus), n=1)
q <- pnbinom(q = cutoff, size = 1/use.var, mu = use.mean)
remove(use.var, use.mean)

# Calculate number of individuals belonging to each group
n4 <- min(ceiling(N*(1-p)), ceiling(N*(1-q)))

n.per.group <- solve(a = matrix(c(1,1,1,
                                  1,1,0,
                                  1,0,1), 
                                ncol = 3, byrow = TRUE),
                     b = matrix(c(N-n4,
                                  N*p,
                                  N*q), 
                                ncol = 1, byrow = TRUE)
)

n1 <- ceiling(n.per.group[1])
n2 <- ceiling(n.per.group[2])
n3 <- ceiling(n.per.group[3])

n.generated <- n1+n2+n3+n4

###############################################################################
# Checks in between to ensure that calculations are performed as expected
###############################################################################

assert_that(abs((n1+n2)/N - p) < 0.01, msg = "Emprical proportion of responders is incorrect")
assert_that(abs((n1+n3)/N - q) < 0.01, msg = "Emprical proportion of responders is incorrect")
assert_that(abs((n3+n4)/N - (1-p)) < 0.01, msg = "Emprical proportion of non-responders is incorrect")
assert_that(abs((n2+n4)/N - (1-q)) < 0.01, msg = "Emprical proportion of non-responders is incorrect")

# -----------------------------------------------------------------------------
# Simulate potential outcomes
# -----------------------------------------------------------------------------

# Generate U matrix
list.U <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
list.U$group.01 <- ByGroupGenerateU(n.group = n1, corrdim = corrdim.01, corrmat = list.corrmat$group.01)
list.U$group.02 <- ByGroupGenerateU(n.group = n2, corrdim = corrdim.02, corrmat = list.corrmat$group.02)
list.U$group.03 <- ByGroupGenerateU(n.group = n3, corrdim = corrdim.03, corrmat = list.corrmat$group.03)
list.U$group.04 <- ByGroupGenerateU(n.group = n4, corrdim = corrdim.04, corrmat = list.corrmat$group.04)

# In preparation for obtaining Y, obtain subsets of elements of U matrix and 
# rearrange into list
list.U$group.01 <- ByGroupMatrixToList(mat = list.U$group.01, group = 1, rand.time = rand.time, tot.time = tot.time)
list.U$group.02 <- ByGroupMatrixToList(mat = list.U$group.02, group = 2, rand.time = rand.time, tot.time = tot.time)
list.U$group.03 <- ByGroupMatrixToList(mat = list.U$group.03, group = 3, rand.time = rand.time, tot.time = tot.time)
list.U$group.04 <- ByGroupMatrixToList(mat = list.U$group.04, group = 4, rand.time = rand.time, tot.time = tot.time)

# Generate Y matrix
list.Y <- list(group.01=NULL, group.02=NULL, group.03=NULL, group.04=NULL)
list.Y$group.01 <- ByGroupGenerateY(list.U=list.U, group=1, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)
list.Y$group.02 <- ByGroupGenerateY(list.U=list.U, group=2, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)
list.Y$group.03 <- ByGroupGenerateY(list.U=list.U, group=3, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)
list.Y$group.04 <- ByGroupGenerateY(list.U=list.U, group=4, list.mu=list.mu, list.sigma2=list.sigma2, cutoff=cutoff)

# Reshape Y from list to data frame
list.Y$group.01 <- ByGroupToLongData(Y.group = list.Y$group.01, n.group=n1, group=1, tot.time=tot.time, rand.time=rand.time)
list.Y$group.02 <- ByGroupToLongData(Y.group = list.Y$group.02, n.group=n2, group=2, tot.time=tot.time, rand.time=rand.time)
list.Y$group.03 <- ByGroupToLongData(Y.group = list.Y$group.03, n.group=n3, group=3, tot.time=tot.time, rand.time=rand.time)
list.Y$group.04 <- ByGroupToLongData(Y.group = list.Y$group.04, n.group=n4, group=4, tot.time=tot.time, rand.time=rand.time)

list.Y$group.02$id <- n1 + list.Y$group.02$id
list.Y$group.03$id <- n1 + n2 + list.Y$group.03$id
list.Y$group.04$id <- n1 + n2 + n3 + list.Y$group.04$id

# Finally, we have a data frame with the potential outcomes
df.potential.Yit <- bind_rows(list.Y)
df.potential.Yit <- df.potential.Yit %>% arrange(id, desc(A1), desc(A2), t)

