path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, "sim_results_d_0","correspondence_between_rho_and_tau.RData"))

# Get x-axis and y-axis limits of plots
rho.min.plotval <- min(collect.correlation.tau$datagen.params.rho)
rho.max.plotval <- max(collect.correlation.tau$datagen.params.rho)
tau.min.plotval <- min(collect.correlation.tau$tau.min)
tau.max.plotval <- max(collect.correlation.tau$tau.max)

# Set up plot region
plot(x=-2, 
     type="n", 
     xlim=c(rho.min.plotval,rho.max.plotval), 
     ylim=c(tau.min.plotval,tau.max.plotval), 
     xaxt="n", 
     yaxt="n", 
     xlab="rho", 
     ylab="tau")

abline(a=0,b=1,lty=2)
axis(1, seq(round(rho.min.plotval, 1), round(rho.max.plotval, 1), 0.10))
axis(2, seq(round(tau.min.plotval, 1), round(tau.max.plotval, 1), 0.10))

# Begin drawing lines corresponding to tau.mean, tau.min, and tau.max
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="o", col="darkgreen", lwd=2, lty=3)
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.mean, type="o", col="blue", lwd=2, lty=1)
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="o", col="red", lwd=2, lty=4)

# Finally, add legend
legend("topleft", 
       legend=c("tau_min", "tau_mean", "tau_max", "rho=tau"), 
       col=c("darkgreen","blue","red","black"), 
       lty=c(3,1,4,2), 
       lwd=c(3,3,3,1))

