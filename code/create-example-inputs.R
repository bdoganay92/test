path.input_data <- Sys.getenv("path.input_data")

# Read in starter file for means
input.means <- read.csv(file.path(path.input_data, "input_means.csv"))

###############################################################################
# Vary the means
###############################################################################
d <- -2

new.input.means <- input.means

new.input.means$time.3[4:5] <- new.input.means$time.3[4:5] + 0.35*d
new.input.means$time.4[4:5] <- new.input.means$time.4[4:5] + 0.75*d
new.input.means$time.5[4:5] <- new.input.means$time.5[4:5] + 0.90*d
new.input.means$time.6[4:5] <- new.input.means$time.6[4:5] + d

write.csv(new.input.means, file.path(path.input_data, "input_means_d_-2.csv"), row.names=FALSE)

###############################################################################
# Vary the means
###############################################################################
d <- -1

new.input.means <- input.means

new.input.means$time.3[4:5] <- new.input.means$time.3[4:5] + 0.35*d
new.input.means$time.4[4:5] <- new.input.means$time.4[4:5] + 0.75*d
new.input.means$time.5[4:5] <- new.input.means$time.5[4:5] + 0.90*d
new.input.means$time.6[4:5] <- new.input.means$time.6[4:5] + d

write.csv(new.input.means, file.path(path.input_data, "input_means_d_-1.csv"), row.names=FALSE)

###############################################################################
# Vary the means
###############################################################################
d <- 0

new.input.means <- input.means

new.input.means$time.3[4:5] <- new.input.means$time.3[4:5] + 0.35*d
new.input.means$time.4[4:5] <- new.input.means$time.4[4:5] + 0.75*d
new.input.means$time.5[4:5] <- new.input.means$time.5[4:5] + 0.90*d
new.input.means$time.6[4:5] <- new.input.means$time.6[4:5] + d

write.csv(new.input.means, file.path(path.input_data, "input_means_d_0.csv"), row.names=FALSE)

###############################################################################
# Vary the means
###############################################################################
d <- 1

new.input.means <- input.means

new.input.means$time.3[4:5] <- new.input.means$time.3[4:5] + 0.35*d
new.input.means$time.4[4:5] <- new.input.means$time.4[4:5] + 0.75*d
new.input.means$time.5[4:5] <- new.input.means$time.5[4:5] + 0.90*d
new.input.means$time.6[4:5] <- new.input.means$time.6[4:5] + d

write.csv(new.input.means, file.path(path.input_data, "input_means_d_1.csv"), row.names=FALSE)

###############################################################################
# Vary the means
###############################################################################
d <- 2

new.input.means <- input.means

new.input.means$time.3[4:5] <- new.input.means$time.3[4:5] + 0.35*d
new.input.means$time.4[4:5] <- new.input.means$time.4[4:5] + 0.75*d
new.input.means$time.5[4:5] <- new.input.means$time.5[4:5] + 0.90*d
new.input.means$time.6[4:5] <- new.input.means$time.6[4:5] + d

write.csv(new.input.means, file.path(path.input_data, "input_means_d_2.csv"), row.names=FALSE)




