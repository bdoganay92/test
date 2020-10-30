# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder <- "sim_study_main"

# Results using Method 1
load(file = file.path(.path.output_data, .this.folder, "power_method_01.RData"))
power.table.method01 <- power.table
remove(power.table)

# Results using Method 2
load(file = file.path(.path.output_data, .this.folder, "power_method_02.RData"))
power.table.method02 <- power.table
remove(power.table)

# Calculated values of the ratio sqrt(hat{V0})/sqrt(hat{V1})
load(file = file.path(.path.output_data, .this.folder, "ratio.RData"))

###############################################################################
# Plot results for Method 1 and overlay results for Method 2 in the same plot
###############################################################################

op <- par() # save default settings
par(mfrow = c(1,2), pty="m")

# Power: difference in end-of-study means
plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2)
abline(h = 1, lty=3)
title(main = "Difference in End-of-Study Means")

lines(power.table.method01$N, power.table.method01$power.eos.means.truth, type="o", col = "black", lwd=3)
lines(power.table.method01$N, power.table.method01$power.eos.means.below.truth, type="o", col = "tomato", lwd=3)
lines(power.table.method01$N, power.table.method01$power.eos.means.above.truth, type="o", col = "cornflowerblue", lwd=3)
lines(power.table.method02$N, power.table.method02$power.eos.means.method02, type="o", col = "seagreen", lwd=2, lty=3)

legend("bottomright", 
       c("method 1: truth", "method 1: below truth", "method 1: above truth", "method 2"), 
       pch=21,
       lwd=c(3,3,3,3),
       lty=c(1,1,1,3),
       col = c("black", "tomato","cornflowerblue","seagreen"),
       pt.bg = c("black", "tomato","cornflowerblue","seagreen"),
       pt.lwd=c(3,3,3,3),
       cex = 0.70)

# Power: difference in AUC
plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2)
abline(h = 1, lty=3)
title(main = "Difference in AUC")

lines(power.table.method01$N, power.table.method01$power.AUC.truth, type="o", col = "black", lwd=3)
lines(power.table.method01$N, power.table.method01$power.AUC.below.truth, type="o", col = "tomato", lwd=3)
lines(power.table.method01$N, power.table.method01$power.AUC.above.truth, type="o", col = "cornflowerblue", lwd=3)
lines(power.table.method02$N, power.table.method02$power.AUC.method02, type="o", col = "seagreen", lwd=2, lty=3)

legend("bottomright", 
       c("method 1: truth", "method 1: below truth", "method 1: above truth", "method 2"), 
       pch=21,
       lwd=c(3,3,3,3),
       lty=c(1,1,1,3),
       col = c("black", "tomato","cornflowerblue","seagreen"),
       pt.bg = c("black", "tomato","cornflowerblue","seagreen"),
       pt.lwd=c(3,3,3,3),
       cex = 0.70)

par(op)


###############################################################################
# Plot ratio of sqrt(hat{V0})/sqrt(hat{V1})
###############################################################################

op <- par() # save default settings
par(mfrow = c(1,2), pty="m")

# Ratio: difference in end-of-study means
plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(0.5,2.2),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Ratio: estimated SE under H0 divided by estimated SE under Ha ")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(0.6, 2.2, 0.20))
abline(h = 1, lty=2)
title(main = "Difference in End-of-Study Means")

lines(ratio.table$N, ratio.table$ratio.truth.eos.means, type="o", col = "black", lwd=3)
lines(ratio.table$N, ratio.table$ratio.below.truth.eos.means, type="o", col = "tomato", lwd=3)
lines(ratio.table$N, ratio.table$ratio.above.truth.eos.means, type="o", col = "cornflowerblue", lwd=3)

legend("bottomright", 
       c("ratio: truth", "ratio: below truth", "ratio: above truth"), 
       pch=21,
       lwd=c(3,3,3),
       lty=c(1,1,1),
       col = c("black", "tomato","cornflowerblue"),
       pt.bg = c("black", "tomato","cornflowerblue"),
       pt.lwd=c(3,3,3),
       cex = 0.70)

# Ratio: difference in AUC
plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(0.5,2.2),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Ratio: estimated SE under H0 divided by estimated SE under Ha ")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(0.6, 2.2, 0.20))
abline(h = 1, lty=2)
title(main = "Difference in AUC")

lines(ratio.table$N, ratio.table$ratio.truth.AUC, type="o", col = "black", lwd=3)
lines(ratio.table$N, ratio.table$ratio.below.truth.AUC, type="o", col = "tomato", lwd=3)
lines(ratio.table$N, ratio.table$ratio.above.truth.AUC, type="o", col = "cornflowerblue", lwd=3)

legend("bottomright", 
       c("ratio: truth", "ratio: below truth", "ratio: above truth"), 
       pch=21,
       lwd=c(3,3,3),
       lty=c(1,1,1),
       col = c("black", "tomato","cornflowerblue"),
       pt.bg = c("black", "tomato","cornflowerblue"),
       pt.lwd=c(3,3,3),
       cex = 0.70)

par(op)

###############################################################################
# Plot the elicited EDTR trajectories
###############################################################################
.path.code <- Sys.getenv("path.code")
source(file.path(.path.code,"input-utils.R"))
source(file.path(.path.code,"datagen-utils.R"))
source(file.path(.path.code,"analysis-utils.R"))
source(file.path(.path.code, "geemMod.r"))
environment(geemMod) <- asNamespace("geeM")

# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder.alternative <- "sim_study_main/sim_results_alternative"
.this.folder.null1 <- "sim_study_main/sim_results_null/truth"
.this.folder.null2 <- "sim_study_main/sim_results_null/below_truth"
.this.folder.null3 <- "sim_study_main/sim_results_null/above_truth"

# Begin steps -----------------------------------------------------------------
input.means <- read.csv(file.path(.path.output_data, .this.folder.alternative, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(.path.output_data, .this.folder.alternative, "input_prop_zeros.csv"))
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

# Specify L and D matrices for contrasts of interest 
# Below, these are specified for end-of-study means and AUC

# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in AUCs
# This specification assumes that measurement occasions will be 1-month apart
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

# Check truth
# The next two lines will yield the following quantities
#   - diff.eos.means.plusplus.minusplus
#   - diff.AUC.plusplus.minusplus
source(file.path(.path.code, "calc-truth-beta.R"))
source(file.path(.path.code, "calc-truth-contrasts.R"))

# Plot EDTR mean trajectories
plotdat <- data.frame(month = seq(1,input.tot.time,1),
                      mu.plusplus = u.plusplus,
                      mu.plusminus = u.plusminus,
                      mu.minusplus = u.minusplus,
                      mu.minusminus = u.minusminus)

plot(-1, 
     type="n",
     xlim = c(1,input.tot.time),
     ylim = c(0,6),
     xaxt="n",
     yaxt="n",
     xlab = "Month",
     ylab = "Mean Past-Month No. of Cocaine-Use Days")

axis(1, at = seq(1,input.tot.time,1))
axis(2, at = seq(0, 6, 0.5))

title(main = paste("EDTR Mean Trajectories\n",
                   "Diff in EOS means = ", round(diff.eos.means.plusplus.minusplus, 2), 
                   "   ",
                   "Diff in AUC = ", round(diff.AUC.plusplus.minusplus, 2),
                   sep=""))

lines(plotdat$month, plotdat$mu.plusplus, type = "o", lwd=7, col="lightblue", lty=1)
lines(plotdat$month, plotdat$mu.minusplus, type = "o", lwd=7, col="darkgoldenrod", lty=1)

# Begin steps -----------------------------------------------------------------
input.means <- read.csv(file.path(.path.output_data, .this.folder.null1, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(.path.output_data, .this.folder.null1, "input_prop_zeros.csv"))
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

# Specify L and D matrices for contrasts of interest 
# Below, these are specified for end-of-study means and AUC

# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in AUCs
# This specification assumes that measurement occasions will be 1-month apart
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

# Check truth
# The next two lines will yield the following quantities
#   - diff.eos.means.plusplus.minusplus
#   - diff.AUC.plusplus.minusplus
source(file.path(.path.code, "calc-truth-beta.R"))
source(file.path(.path.code, "calc-truth-contrasts.R"))

plotdat <- data.frame(month = seq(1,input.tot.time,1),
                      mu.plusplus = u.plusplus,
                      mu.plusminus = u.plusminus,
                      mu.minusplus = u.minusplus,
                      mu.minusminus = u.minusminus)

# Add lines to existing plot of EDTR mean trajectories
lines(plotdat$month, plotdat$mu.plusplus, type = "o", lwd=1, col="black", lty=2)

# Begin steps -----------------------------------------------------------------
input.means <- read.csv(file.path(.path.output_data, .this.folder.null2, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(.path.output_data, .this.folder.null2, "input_prop_zeros.csv"))
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

# Specify L and D matrices for contrasts of interest 
# Below, these are specified for end-of-study means and AUC

# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in AUCs
# This specification assumes that measurement occasions will be 1-month apart
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

# Check truth
# The next two lines will yield the following quantities
#   - diff.eos.means.plusplus.minusplus
#   - diff.AUC.plusplus.minusplus
source(file.path(.path.code, "calc-truth-beta.R"))
source(file.path(.path.code, "calc-truth-contrasts.R"))

plotdat <- data.frame(month = seq(1,input.tot.time,1),
                      mu.plusplus = u.plusplus,
                      mu.plusminus = u.plusminus,
                      mu.minusplus = u.minusplus,
                      mu.minusminus = u.minusminus)

# Add lines to existing plot of EDTR mean trajectories
lines(plotdat$month, plotdat$mu.plusplus, type = "o", lwd=1, col= "grey", lty=2)

# Begin steps -----------------------------------------------------------------
input.means <- read.csv(file.path(.path.output_data, .this.folder.null3, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(.path.output_data, .this.folder.null3, "input_prop_zeros.csv"))
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

# Specify L and D matrices for contrasts of interest 
# Below, these are specified for end-of-study means and AUC

# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in AUCs
# This specification assumes that measurement occasions will be 1-month apart
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

# Check truth
# The next two lines will yield the following quantities
#   - diff.eos.means.plusplus.minusplus
#   - diff.AUC.plusplus.minusplus
source(file.path(.path.code, "calc-truth-beta.R"))
source(file.path(.path.code, "calc-truth-contrasts.R"))

plotdat <- data.frame(month = seq(1,input.tot.time,1),
                      mu.plusplus = u.plusplus,
                      mu.plusminus = u.plusminus,
                      mu.minusplus = u.minusplus,
                      mu.minusminus = u.minusminus)

# Add lines to existing plot of EDTR mean trajectories
lines(plotdat$month, plotdat$mu.plusplus, type = "o", lwd=1, col= "darkgrey", lty=2)

# Add legend
legend("topleft", 
       c("Under H0: truth", "Under H0: below truth", "Under H0: above truth","Under Ha: EDTR (+1,+1)", "Under Ha: EDTR (-1,+1)"), 
       pch=21,
       lwd=c(3,3,3,6,6),
       col = c("black", "grey", "darkgrey", "cornflowerblue", "darkgoldenrod"),
       pt.bg = c("black", "grey", "darkgrey", "cornflowerblue", "darkgoldenrod"),
       pt.lwd=c(3,3,3,6,6),
       cex = 0.80)


par(op)


