path.output_data <- Sys.getenv("path.output_data")


##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_study_supp/sim_size_test/low_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

# Set up plot region
plot(x=-2, 
     type="n", 
     xlim=c(-.05,1), 
     ylim=c(-.05,1), 
     xaxt="n", 
     yaxt="n", 
     xlab="rho", 
     ylab="tau")

abline(a=0,b=1,lty=2)
axis(1, seq(0, 1, 0.10))
axis(2, seq(0,1, 0.10))

# Begin drawing lines corresponding to tau.mean
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.mean, type="o", col="darkgreen", lwd=2, lty=3)

##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_study_supp/sim_size_test/mid_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

# Begin drawing lines corresponding to tau.mean
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.mean, type="o", col="darkblue", lwd=2, lty=3)

##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################
this.folder <- "sim_study_supp/sim_size_test/high_zeros"
load(file.path(path.output_data, this.folder,"correspondence_between_rho_and_tau.RData"))

# Begin drawing lines corresponding to tau.mean
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.mean, type="o", col="darkred", lwd=2, lty=3)

##########################################################################################
# Draw guide lines for targeted value of tau_mean
##########################################################################################
abline(v=.80,lty=1,col="black")
abline(v=.85,lty=1,col="black")
abline(v=.90,lty=1,col="black")
abline(h=.70,lty=1,col="black")

##########################################################################################
# Finally, add legend
##########################################################################################
title(main = "Correspondence between rho and tau_mean")

legend("topleft", 
       legend=c("low zeros", "mid zeros", "high zeros", "45 degree line"), 
       col=c("darkgreen","blue","red","black"), 
       lty=c(3,1,4,2), 
       lwd=c(3,3,3,1))


