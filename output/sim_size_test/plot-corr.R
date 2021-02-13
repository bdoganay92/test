path.output_data <- Sys.getenv("path.output_data")


##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_size_test/low_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

jpeg(file.path(path.output_data, "sim_size_test/plot_correspondence_between_rho_and_tau_mean.jpeg"), width = 800, height = 800)

# Set up plot region
plot(x=-2, 
     type="n", 
     xlim=c(-.05,1), 
     ylim=c(-.05,1), 
     xaxt="n", 
     yaxt="n", 
     xlab="rho", 
     ylab="tau")

axis(1, seq(0, 1, 0.10))
axis(2, seq(0,1, 0.10))

collect.correlation.tau$diff <- collect.correlation.tau$tau.max - collect.correlation.tau$tau.min
print(max(collect.correlation.tau$diff))

# Begin drawing lines corresponding to tau.mean
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="l", col="darkgreen", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, pch=21, bg = "darkgreen", col="black", cex=1.7)

lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="l", col="darkgreen", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, pch=24, bg = "darkgreen", col="black", cex=1)


##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_size_test/moderate_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

collect.correlation.tau$diff <- collect.correlation.tau$tau.max - collect.correlation.tau$tau.min
print(max(collect.correlation.tau$diff))

# Begin drawing lines corresponding to tau.mean
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="l", col="darkblue", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, pch=21, bg = "darkblue", col="black", cex=1.7)

lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="l", col="darkblue", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, pch=24, bg = "darkblue", col="black", cex=1)


##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_size_test/high_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

collect.correlation.tau$diff <- collect.correlation.tau$tau.max - collect.correlation.tau$tau.min
print(max(collect.correlation.tau$diff))

# Begin drawing lines corresponding to tau.mean
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="l", col="darkred", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, pch=21, bg = "darkred", col="black", cex=1.7)

lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="l", col="darkred", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, pch=24, bg = "darkred", col="black", cex=1)


##########################################################################################
# Finally, add legend
##########################################################################################
title(main = "Across Scenarios 1-3, relationship between rho and tau_max, and rho and tau_min")

legend("topleft", 
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
       cex = 1)


text(0.07+0.70, 0.2, labels = "Low Zeros (rho=0.80, tau_max=0.73, tau_min=0.67)", cex = 0.8, col = "red")
text(0.07+0.70, 0.15, labels = "Moderate Zeros (rho=0.80, tau_max=0.71, tau_min=0.61)", cex = 0.8, col = "red")
text(0.07+0.70, 0.10, labels = "High Zeros (rho=0.80, tau_max=0.71, tau_min=0.48)", cex = 0.8, col = "red")

dev.off()
