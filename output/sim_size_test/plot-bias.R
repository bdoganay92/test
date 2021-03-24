###############################################################################
# Plot bias
###############################################################################
.path.code <- Sys.getenv("path.code")

# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder.1 <- "sim_size_test/low_zeros"
.this.folder.2 <- "sim_size_test/moderate_zeros"
.this.folder.3 <- "sim_size_test/high_zeros"

# Output of calc-bias.R
load(file = file.path(.path.output_data, .this.folder.1, "bias.RData"))
df1 <- .df.vary.params

# Output of calc-bias.R
load(file = file.path(.path.output_data, .this.folder.2, "bias.RData"))
df2 <- .df.vary.params

# Output of calc-bias.R
load(file = file.path(.path.output_data, .this.folder.3, "bias.RData"))
df3 <- .df.vary.params

###############################################################################
# Difference in estimates of end-of-study means
###############################################################################

path.output_data <- Sys.getenv("path.output_data")

jpeg(file.path(path.output_data, "sim_size_test/plot_bias_est_deltaq_eos_means.jpeg"), width = 400, height = 900)

op <- par() # save default settings
par(mfcol = c(3,1), pty="m")
par(mar = c(6, 6, 1.2, 1) + 0.1)  # Bottom, left, top, right


plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "Low Zeros", cex.main = 2)
points(df1$N, df1$bias.diff.eos.means, pch=21, bg = "darkgoldenrod", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "Moderate Zeros", cex.main = 2)
points(df2$N, df2$bias.diff.eos.means, pch=21, bg = "darkgoldenrod", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "High Zeros", cex.main = 2)
points(df3$N, df3$bias.diff.eos.means, pch=21, bg = "darkgoldenrod", col="black", cex=3)

dev.off()

par(op)

###############################################################################
# Difference in estimates of AUC
###############################################################################

jpeg(file.path(path.output_data, "sim_size_test/plot_bias_est_deltaq_AUC.jpeg"), width = 400, height = 900)

op <- par() # save default settings
par(mfcol = c(3,1), pty="m")
par(mar = c(6, 6, 1.2, 1) + 0.1)  # Bottom, left, top, right


plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)


title(main = "Low Zeros", cex.main = 2)
points(df1$N, df1$bias.diff.AUC, pch=21, bg = "darkgoldenrod", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "Moderate Zeros", cex.main = 2)
points(df2$N, df2$bias.diff.AUC, pch=21, bg = "darkgoldenrod", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "High Zeros", cex.main = 2)
points(df3$N, df3$bias.diff.AUC, pch=21, bg = "darkgoldenrod", col="black", cex=3)

dev.off()

par(op)



###############################################################################
# Standard error of difference in estimates of end-of-study means
###############################################################################

path.output_data <- Sys.getenv("path.output_data")

jpeg(file.path(path.output_data, "sim_size_test/plot_bias_est_stderr_eos_means.jpeg"), width = 400, height = 900)

op <- par() # save default settings
par(mfcol = c(3,1), pty="m")
par(mar = c(6, 6, 1.2, 1) + 0.1)  # Bottom, left, top, right


plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "Low Zeros", cex.main = 2)
points(df1$N, df1$bias.stderr.est.diff.eos.means, pch=21, bg = "darkgoldenrod", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "Moderate Zeros", cex.main = 2)
points(df2$N, df2$bias.stderr.est.diff.eos.means, pch=21, bg = "darkgoldenrod", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "High Zeros", cex.main = 2)
points(df3$N, df3$bias.stderr.est.diff.eos.means, pch=21, bg = "darkgoldenrod", col="black", cex=3)

dev.off()
par(op)


###############################################################################
# Standard error of difference in estimates of AUC
###############################################################################

path.output_data <- Sys.getenv("path.output_data")

jpeg(file.path(path.output_data, "sim_size_test/plot_bias_est_stderr_AUC.jpeg"), width = 400, height = 900)

op <- par() # save default settings
par(mfcol = c(3,1), pty="m")
par(mar = c(6, 6, 1.2, 1) + 0.1)  # Bottom, left, top, right

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "Low Zeros", cex.main = 2)
points(df1$N, df1$bias.stderr.est.diff.AUC, pch=21, bg = "darkgoldenrod", col="black", cex=3)


plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)


title(main = "Moderate Zeros", cex.main = 2)
points(df2$N, df2$bias.stderr.est.diff.AUC, pch=21, bg = "darkgoldenrod", col="black", cex=3)

plot(-1, 
     type="n",
     xlim = c(100, 600),
     ylim = c(-1,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Bias",
     cex.lab = 2,
     frame.plot = FALSE)

axis(1, at = seq(100, 600, 100), cex.axis=2, lwd = 5)
axis(2, at = seq(-1, 1, 0.50), cex.axis=2, lwd = 5)
abline(h = 0, lty=2, lwd=3)

title(main = "High Zeros", cex.main = 2)
points(df3$N, df3$bias.stderr.est.diff.AUC, pch=21, bg = "darkgoldenrod", col="black", cex=3)

dev.off()

par(op)



