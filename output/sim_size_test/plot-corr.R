path.output_data <- Sys.getenv("path.output_data")


##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_size_test/low_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

jpeg(file.path(path.output_data, "sim_size_test/plot_correspondence_between_rho_and_tau_mean.jpeg"), width = 800, height = 800)

par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right

# Set up plot region
plot(x=-2, 
     type="n", 
     xlim=c(-.05,1), 
     ylim=c(-.05,1), 
     xaxt="n", 
     yaxt="n", 
     xlab="rho", 
     ylab="tau",
     cex.lab = 2.5,
     frame.plot = FALSE)

axis(1, seq(0, 1, 0.20), cex.axis=2.5, lwd = 5)
axis(2, seq(0,1, 0.20), cex.axis=2.5, lwd = 5)

collect.correlation.tau$diff <- collect.correlation.tau$tau.max - collect.correlation.tau$tau.min
print(max(collect.correlation.tau$diff))

# Begin drawing lines corresponding to tau
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="l", col="darkgreen", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, pch=21, bg = "darkgreen", col="black", cex=2.5)

lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="l", col="darkgreen", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, pch=24, bg = "darkgreen", col="black", cex=2.5)


##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_size_test/moderate_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

collect.correlation.tau$diff <- collect.correlation.tau$tau.max - collect.correlation.tau$tau.min
print(max(collect.correlation.tau$diff))

# Begin drawing lines corresponding to tau
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="l", col="darkblue", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, pch=21, bg = "darkblue", col="black", cex=2.5)

lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="l", col="darkblue", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, pch=24, bg = "darkblue", col="black", cex=2.5)


##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_size_test/high_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

collect.correlation.tau$diff <- collect.correlation.tau$tau.max - collect.correlation.tau$tau.min
print(max(collect.correlation.tau$diff))

# Begin drawing lines corresponding to tau
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="l", col="darkred", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, pch=21, bg = "darkred", col="black", cex=2.5)

lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="l", col="darkred", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, pch=24, bg = "darkred", col="black", cex=2.5)


##########################################################################################
# Finally, add legend
##########################################################################################
#title(main = "Across Scenarios 1-3, relationship between rho and tau_max, and rho and tau_min")

legend(-0.05, 1,
       legend=c("tau_max (Low Zeros)",  
                "tau_max (Moderate Zeros)", 
                "tau_max (High Zeros)", 
                "tau_min (Low Zeros)",  
                "tau_min (Moderate Zeros)", 
                "tau_min (High Zeros)"), 
       col=c("darkgreen","darkblue","darkred","darkgreen","darkblue","darkred"), 
       lty=c(3,3,3,3,3,3,2), 
       pch=c(21,21,21,24,24,24),
       pt.bg = c("darkgreen","darkblue","darkred","darkgreen","darkblue","darkred"),
       pt.cex = rep(3,6),
       cex = 1.7)


text(0.07+0.60, 0.04, labels = "Low Zeros (when rho=0.80: tau_max=0.73, tau_min=0.67)", cex = 1.3, col = "red")
text(0.07+0.60, 0.00, labels = "Moderate Zeros (when rho=0.80: tau_max=0.71, tau_min=0.61)", cex = 1.3, col = "red")
text(0.07+0.60, -0.04, labels = "High Zeros (when rho=0.80: tau_max=0.71, tau_min=0.48)", cex = 1.3, col = "red")

dev.off()
