###############################################################################
# Plot bias
###############################################################################
.path.code <- Sys.getenv("path.code")

# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder.1 <- "sim_size_test/low_zeros"
.this.folder.2 <- "sim_size_test/moderate_zeros"
.this.folder.3 <- "sim_size_test/high_zeros"

load(file = file.path(.path.output_data, .this.folder.1, "power.RData"))
df1 <- power.table
print(df1)

load(file = file.path(.path.output_data, .this.folder.2, "power.RData"))
df2 <- power.table
print(df2)

load(file = file.path(.path.output_data, .this.folder.3, "power.RData"))
df3 <- power.table
print(df3)

###############################################################################
# Difference in estimates of end-of-study means and AUC
###############################################################################

op <- par() # save default settings
par(mfcol = c(3,2), pty="m")

plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(.02, .08, 0.01))
abline(h = .05, lty=2)

title(main = "Difference in End-of-Study Means (Low Zeros)")
points(df1$N, df1$power.eos.means, pch=21, bg = "cornflowerblue", col="black")

plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(.02, .08, 0.01))
abline(h = .05, lty=2)

title(main = "Difference in End-of-Study Means (Moderate Zeros)")
points(df2$N, df2$power.eos.means, pch=21, bg = "cornflowerblue", col="black")

plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(.02, .08, 0.01))
abline(h = .05, lty=2)

title(main = "Difference in End-of-Study Means (High Zeros)")
points(df3$N, df3$power.eos.means, pch=21, bg = "cornflowerblue", col="black")

plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(.02, .08, 0.01))
abline(h = .05, lty=2)

title(main = "Difference in AUC (Low Zeros)")
points(df1$N, df1$power.AUC, pch=21, bg = "cornflowerblue", col="black")

plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(.02, .08, 0.01))
abline(h = .05, lty=2)

title(main = "Difference in AUC (Moderate Zeros)")
points(df2$N, df2$power.AUC, pch=21, bg = "cornflowerblue", col="black")

plot(-1, 
     type="n",
     xlim = c(100, 500),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error")

axis(1, at = seq(100, 500, 100))
axis(2, at = seq(.02, .08, 0.01))
abline(h = .05, lty=2)

title(main = "Difference in AUC (High Zeros)")
points(df3$N, df3$power.AUC, pch=21, bg = "cornflowerblue", col="black")

par(op)


