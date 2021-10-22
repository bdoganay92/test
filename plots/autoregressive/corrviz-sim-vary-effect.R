library(ggplot2)
library(ggcorrplot)
library(dplyr)

path.output_data <- Sys.getenv("path.output_data")
path.plots <- Sys.getenv("path.plots")
path.code <- Sys.getenv("path.code")
this.folder <- "autoregressive"
this.subfolder <- "corrviz_sim_vary_effect"

for(i in 1:10){
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  load(file.path(path.output_data, this.folder, this.scenario, "collect_seq_cormat.RData"))
  
  for(j in 1:13){
    if(collect.seq.cormat[[j]]$datagen.params.rho == 0.20){
      list.low <- collect.seq.cormat[[j]]
    }else if(collect.seq.cormat[[j]]$datagen.params.rho == 0.40){
      list.moderate <- collect.seq.cormat[[j]]
    }else if(collect.seq.cormat[[j]]$datagen.params.rho == 0.60){
      list.high <- collect.seq.cormat[[j]]
    }else{
      next
    }
  }
  
  all.list <- list(list.low, list.moderate, list.high)
  all.nam <- c("low", "moderate", "high")
  
  for(k in 1:3){
    curr.list <- all.list[[k]]
    curr.nam <- all.nam[k]
    source(file.path(path.plots, this.folder, "get-corrviz.R"))
  }
}

