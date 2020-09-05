path.output_data <- Sys.getenv("path.output_data")
this.folder <- "sim_results_d_0"

list.all.results <- list()

load(file.path(path.output_data, this.folder, "Nrequired_rho_0.3.RData"))

current.results <- data.frame(rho = 0.3, 
                              N.required.eos.means = N.required.eos.means,
                              N.required.AUC = N.required.AUC)

list.all.results <- append(list.all.results, list(current.results))

load(file.path(path.output_data, this.folder, "Nrequired_rho_0.6.RData"))

current.results <- data.frame(rho = 0.6, 
                              N.required.eos.means = N.required.eos.means,
                              N.required.AUC = N.required.AUC)

list.all.results <- append(list.all.results, list(current.results))

load(file.path(path.output_data, this.folder, "Nrequired_rho_0.8.RData"))

current.results <- data.frame(rho = 0.8, 
                              N.required.eos.means = N.required.eos.means,
                              N.required.AUC = N.required.AUC)

list.all.results <- append(list.all.results, list(current.results))

df.all.results <- do.call(rbind, list.all.results)

write.csv(df.all.results, file.path(path.output_data, this.folder, "display-calc-N.csv"))

