################################################################################
# Calculate the range of possible values for n4 (the variable k below)
# given values for N (total sample size), p (proportion of individuals
# who would respond to A1=+1), q (proportion of individuals who would respond 
# to A1=-1)
################################################################################

# Inputs to the calculation
# path.output_data <- Sys.getenv("path.output_data")
# this.folder <- "autoregressive"
# N <- 500
# p <- .40
# q <- .40

# max.possible.value is the maximum possible value n4 (the number of 
# individuals who would not respond to A1=+1 and A1=-1)
max.possible.value <- min(c(N*(1-p), N*(1-q)))

# Note that while we know the maximum possible value of n4, 
# we still need to determine the minimum possible value of n4.
# To determine the minimum possible value of n4,
# we will solve the system of equations below for n1, n2, n3, 
# given a particular value of n4 not exceeding max.possible.value
# If we find that a particular value for n4 leads to any of n1, n2, n3 being
# negative, then we will not consider that particular value of n4 to be 'valid'.
#
#
# The matrices below corresponds to the following system of equations below.
# Here, potential 'valid' values of k range between 
# 0, 1, 2, 3, ..., max.possible.value
#
# n1 + n2 + n3 + n4 = N
# n1 + n2           = Np
# n1      + n3      = Nq
#                n4 = k
#

all.possible.values <- seq(from = 0, to = max.possible.value, by = 1)

list.all.results <- list()

for(idx.k in 1:length(all.possible.values)){
  
  k <- all.possible.values[idx.k]
  
  A <- matrix(c(1,1,1,1,
                1,1,0,0,
                1,0,1,0,
                0,0,0,1),
              byrow=TRUE,
              nrow=4)
  
  b <- as.matrix(c(N,
                   N*p,
                   N*q,
                   k))
  
  x <- solve(A, b)
  
  result <- data.frame(n4 = k,
                       n1 = x[1,],
                       n2 = x[2,],
                       n3 = x[3,])
  
  list.all.results <- append(list.all.results, list(result))
}

all.results <- do.call(rbind, list.all.results)

# Which is the smallest 'valid' value of n4?
idx.pos <- ((all.results[["n1"]] >= 0) & (all.results[["n2"]] >= 0) & (all.results[["n3"]] >= 0))
idx.pos <- which(idx.pos)
min.idx.pos <- min(idx.pos)
min.possible.value <- all.results[min.idx.pos,"n4"]

# Print results
print(min.possible.value)

# Save results
write.csv(all.results, 
          file = file.path(path.output_data, 
                           this.folder, 
                           "sim_vary_n4/solved.nj.csv"), 
          row.names = FALSE)

