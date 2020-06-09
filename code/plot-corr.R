.path.output_data <- Sys.getenv("path.output_data")
.collect.all.corr <- list()

# Setting all.names=FALSE in the call to ls() removes all variables in the
# current environment except those beginning with a "."

.alld <- c(-2,-1.5,-1,-0.5,0,0.5,1)

for(i in 1:length(.alld)){
  load(file.path(.path.output_data, paste("corr_d_", .alld[i], ".RData", sep="")))
  simulated.correlation$d <- d
  .collect.all.corr <- append(.collect.all.corr, list(simulated.correlation))
  rm(list = ls(all.names = FALSE))
}

# Combine all results into one plot
.collect.all.corr <- lapply(.collect.all.corr, function(x){
  x$simulated.corr <- round(x$simulated.corr, 3)
  return(x)
})

plot(x=-2, type="n", xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="rho", ylab="tau")
abline(a=0,b=1,lty=2)
axis(1, seq(0,1,0.10))
axis(2, seq(0,1,0.10))
for(i in 1:length(.collect.all.corr)){
  plotdat <- .collect.all.corr[[i]]
  lines(x = plotdat$datagen.params.rho, y = plotdat$simulated.corr, type="o")
}

