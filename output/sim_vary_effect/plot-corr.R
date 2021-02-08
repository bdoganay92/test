path.output_data <- Sys.getenv("path.output_data")

##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_low_corr/sim_results_0"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

jpeg(file.path(path.output_data, this.folder, "plot_correspondence_between_rho_and_tau_mean.jpeg"), width = 800, height = 800)

# Set up plot region
plot(x=-2, 
     type="n", 
     xlim=c(-.05,1), 
     ylim=c(-.05,1), 
     xaxt="n", 
     yaxt="n", 
     xlab="rho", 
     ylab="tau_mean")

abline(a=0,b=1,lty=2)
axis(1, seq(0, 1, 0.10))
axis(2, seq(0,1, 0.10))

# Begin drawing lines corresponding to tau.mean
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.mean, type="l", col="darkgreen", lwd=2, lty=3)
points(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.mean, pch=21, bg = "darkgreen", col="black", cex=1.7)

##########################################################################################
# Draw guide lines for targeted value of tau_mean
##########################################################################################
abline(v=.15,lty=1,col="black")
abline(h=.10,lty=1,col="black")

##########################################################################################
# Finally, add legend
##########################################################################################

title(main = "Correspondence between rho and tau_mean")

legend("topleft", 
       legend=c("Dashed 45 degree diagonal line", 
                "Solid horizontal or vertical lines to indicate which value of rho will yield approximately tau_mean=0.1"), 
       col=c("black","black"), 
       lty=c(2,1), 
       lwd=c(1,1),
       cex = 1)

dev.off()



