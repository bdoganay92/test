path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, "correspondence_between_rho_and_tau.RData"))

plot(x=-2, type="n", xlim=c(-1,1), ylim=c(-1,1), xaxt="n", yaxt="n", xlab="rho", ylab="tau")
abline(a=0,b=1,lty=2)
axis(1, seq(-1,1,0.10))
axis(2, seq(-1,1,0.10))

lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.min, type="o", col="darkgreen", lwd=1.5, lty=3)
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.mean, type="o", col="blue", lwd=1.5, lty=1)
lines(x = collect.correlation.tau$datagen.params.rho, y = collect.correlation.tau$tau.max, type="o", col="red", lwd=1.5, lty=4)

legend("topleft", legend=c("tau_min", "tau_mean", "tau_max", "rho=tau"), col=c("darkgreen","blue","red","black"), lty=c(3,1,4,2), lwd=c(2,2,2,1))

