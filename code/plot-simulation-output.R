library(dplyr)

path.output_data <- Sys.getenv("path.output_data")
this.folder <- "autoregressive"

these.dirs <- list.dirs(path = file.path(path.output_data, 
                                         this.folder, 
                                         "sim_size_test"), 
                        recursive = FALSE)
cnt.scenarios <- length(these.dirs)
idx.dirs <- 1

dat.power <- read.csv(file.path(these.dirs[idx.dirs], "power.csv"))
all.N <- unique(dat.power$N)

par(mar = c(6, 6, 1.2, 1) + 0.1)  # Bottom, left, top, right

# plot(-1, 
#      type="n",
#      xlim = c(0, 0.9),
#      ylim = c(-.60,.05),
#      xaxt="n",
#      yaxt="n",
#      xlab = "rho",
#      ylab = "Bias",
#      cex.lab = 2,
#      frame.plot = FALSE)

plot(-1, 
     type="n",
     xlim = c(0, 0.9),
     ylim = c(-.30,.30),
     xaxt="n",
     yaxt="n",
     xlab = "rho",
     ylab = "Power",
     cex.lab = 2,
     frame.plot = FALSE)

color10.palette <- c("firebrick1","cornflowerblue",
                     "aquamarine2","darkseagreen",
                     "goldenrod3", "lightpink2",
                    "burlywood4","plum3",
                    "steelblue3","grey50")

# for(idx.plot in 1:10){
#   
#   dat.current <- dat.power %>%
#     select(N, rho, bias.stderr.est.diff.AUC) %>%
#     filter(N==all.N[idx.plot])
#   
#   points(dat.current$rho, 
#          dat.current$bias.stderr.est.diff.AUC, 
#          pch=21, 
#          bg = color10.palette[idx.plot], 
#          col="black", 
#          cex=3)
# }

for(idx.plot in 1:10){
  
  dat.current <- dat.power %>%
    select(N, rho, power.diff.AUC) %>%
    filter(N==all.N[idx.plot])
  
  points(dat.current$rho, 
         dat.current$power.diff.AUC, 
         pch=21, 
         bg = color10.palette[idx.plot], 
         col="black", 
         cex=1)
}

abline(h = 0.05, lty=2, lwd=3)

abline(h = 0, lty=2, lwd=3)
axis(1, at = seq(0, 0.8, 0.1), cex.axis=2, lwd = 5)
axis(2, at = c(-.15, -.10, -.05, 0, .05), cex.axis=2, lwd = 5)


# What will be plotted?
# In the most severe cases (N=100, 150, 200, 250), rho did not matter






