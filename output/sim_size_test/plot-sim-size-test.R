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
# Difference in estimates of end-of-study means
###############################################################################

path.output_data <- Sys.getenv("path.output_data")

jpeg(file.path(path.output_data, "sim_size_test/plot_empirical_type_one_error_rate_eos_means.jpeg"), width = 400, height = 900)

op <- par() # save default settings
par(mfcol = c(3,1), pty="m")
par(mar = c(6, 6, 1.2, 1) + 0.1)  # Bottom, left, top, right

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error Rate",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(.02, .08, 0.01), cex.axis=2, lwd = 5)
abline(h = .05, lty=2, lwd=3)
abline(h = .04, lty=2, lwd=3)
abline(h = .06, lty=2, lwd=3)

title(main = "Low Zeros", cex.main = 2)
points(df1$N, df1$power.eos.means, pch=21, bg = "cornflowerblue", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error Rate",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(.02, .08, 0.01), cex.axis=2, lwd = 5)
abline(h = .05, lty=2, lwd=3)
abline(h = .04, lty=2, lwd=3)
abline(h = .06, lty=2, lwd=3)

title(main = "Moderate Zeros", cex.main = 2)
points(df2$N, df2$power.eos.means, pch=21, bg = "cornflowerblue", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error Rate",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(.02, .08, 0.01), cex.axis=2, lwd = 5)
abline(h = .05, lty=2, lwd=3)
abline(h = .04, lty=2, lwd=3)
abline(h = .06, lty=2, lwd=3)

title(main = "High Zeros", cex.main = 2)
points(df3$N, df3$power.eos.means, pch=21, bg = "cornflowerblue", col="black", cex=3)

dev.off()
par(op)

###############################################################################
# Difference in estimates of AUC
###############################################################################

path.output_data <- Sys.getenv("path.output_data")

jpeg(file.path(path.output_data, "sim_size_test/plot_empirical_type_one_error_rate_AUC.jpeg"), width = 400, height = 900)

op <- par() # save default settings
par(mfcol = c(3,1), pty="m")
par(mar = c(6, 6, 1.2, 1) + 0.1)  # Bottom, left, top, right


plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error Rate",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(.02, .08, 0.01), cex.axis=2, lwd = 5)
abline(h = .05, lty=2, lwd=3)
abline(h = .04, lty=2, lwd=3)
abline(h = .06, lty=2, lwd=3)


title(main = "Low Zeros", cex.main = 2)
points(df1$N, df1$power.AUC, pch=21, bg = "cornflowerblue", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error Rate",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(.02, .08, 0.01), cex.axis=2, lwd = 5)
abline(h = .05, lty=2, lwd=3)
abline(h = .04, lty=2, lwd=3)
abline(h = .06, lty=2, lwd=3)

title(main = "Moderate Zeros", cex.main = 2)
points(df2$N, df2$power.AUC, pch=21, bg = "cornflowerblue", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(.02,.08),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Empirical Type-I Error Rate",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(.02, .08, 0.01), cex.axis=2, lwd = 5)
abline(h = .05, lty=2, lwd=3)
abline(h = .04, lty=2, lwd=3)
abline(h = .06, lty=2, lwd=3)


title(main = "High Zeros", cex.main = 2)
points(df3$N, df3$power.AUC, pch=21, bg = "cornflowerblue", col="black", cex=3)


dev.off()

par(op)

