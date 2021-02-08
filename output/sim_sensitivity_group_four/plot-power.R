# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
df.deltaQ <- read.csv(file.path(.path.output_data,"sim_sensitivity_group_four","truth-delta-Q.csv"))
plot.this.rho <- 0.80

jpeg(file.path(.path.output_data, "sim_sensitivity_group_four", "plot_sensitivty_power_eos_means.jpeg"), width = 930, height = 900)

op <- par() # save default settings
par(mar = c(5, 4, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
palette <- c("darkgoldenrod","forestgreen","firebrick3","lightcoral","mistyrose1",
             "bisque3","lightblue4","steelblue4","wheat3","grey30")

plot(-1, 
     type="n",
     xlim = c(100, 230),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "Number of individuals in Group 4 (n4)",
     ylab = "Power")

axis(1, at = seq(100, 190, 10))
axis(2, at = seq(0, 1, 0.10))

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
        .this.folder.alternative <- paste("sim_sensitivity_group_four/sim_results_", idx, sep="")
        load(file = file.path(.path.output_data, .this.folder.alternative, "power.RData"))
        plotdat <- .df.vary.params
        points(plotdat$n4, plotdat$power.eos.means, pch=21, bg = palette[1+idx], col="black", cex=2)       
}

abline(h = 0.80, lty=2)

text(110,1,paste("rho=",plot.this.rho), col="red",cex=2)

for(idx in 0:9){
        if(idx<=2){
                text(210,0.05+idx/18,paste("Scenario ",1+idx,": delta_EOS=",df.deltaQ$truth.delta.eos.means[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx>=3 & idx <=4){
                text(210,0.05+idx/12,paste("Scenario ",1+idx,": delta_EOS=",df.deltaQ$truth.delta.eos.means[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==5){
                text(210,0.05+idx/11,paste("Scenario ",1+idx,": delta_EOS=",df.deltaQ$truth.delta.eos.means[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==6){
                text(210,0.05+idx/10.5,paste("Scenario ",1+idx,": delta_EOS=",df.deltaQ$truth.delta.eos.means[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==7){
                text(210,0.05+idx/10.5,paste("Scenario ",1+idx,": delta_EOS=",df.deltaQ$truth.delta.eos.means[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==8){
                text(210,0.05+idx/11,paste("Scenario ",1+idx,": delta_EOS=",df.deltaQ$truth.delta.eos.means[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==9){
                text(210,0.05+idx/11,paste("Scenario ",1+idx,": delta_EOS=",df.deltaQ$truth.delta.eos.means[idx+1],sep=""), col = palette[1+idx], cex=1)
        }
}


dev.off()


jpeg(file.path(.path.output_data, "sim_sensitivity_group_four", "plot_sensitivty_power_AUC.jpeg"), width = 930, height = 900)

op <- par() # save default settings
par(mar = c(5, 4, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
palette <- c("darkgoldenrod","forestgreen","firebrick3","lightcoral","mistyrose1",
             "bisque3","lightblue4","steelblue4","wheat3","grey30")

plot(-1, 
     type="n",
     xlim = c(100, 230),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "Number of individuals in Group 4 (n4)",
     ylab = "Power")

axis(1, at = seq(100, 190, 10))
axis(2, at = seq(0, 1, 0.10))

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
        .this.folder.alternative <- paste("sim_sensitivity_group_four/sim_results_", idx, sep="")
        load(file = file.path(.path.output_data, .this.folder.alternative, "power.RData"))
        plotdat <- .df.vary.params
        points(plotdat$n4, plotdat$power.AUC, pch=21, bg = palette[1+idx], col="black", cex=2)       
}

abline(h = 0.80, lty=2)

text(110,1,paste("rho=",plot.this.rho), col="red",cex=2)

for(idx in 0:9){
        if(idx<=2){
                text(210,0.05+idx/18,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==3){
                text(210,0.05+idx/12,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==4){
                text(210,0.05+idx/10,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==5){
                text(210,0.05+idx/9,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==6){
                text(210,0.05+idx/9,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==7){
                text(210,0.05+idx/9,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==8){
                text(210,0.05+idx/9.5,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }else if(idx==9){
                text(210,0.05+idx/10,paste("Scenario ",1+idx,": delta_AUC=",df.deltaQ$truth.delta.AUC[idx+1],sep=""), col = palette[1+idx], cex=1)
        }
}


dev.off()

