###############################################################################
# Calculate value of dispersion parameter
###############################################################################
.path.code <- Sys.getenv("path.code")
source(file.path(.path.code,"input-utils.R"))
source(file.path(.path.code,"datagen-utils.R"))
source(file.path(.path.code,"analysis-utils.R"))
source(file.path(.path.code, "geemMod.r"))
environment(geemMod) <- asNamespace("geeM")

# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder.1 <- "sim_size_test/low_zeros"
.this.folder.2 <- "sim_size_test/moderate_zeros"
.this.folder.3 <- "sim_size_test/high_zeros"

# Begin steps -----------------------------------------------------------------
input.means <- read.csv(file.path(.path.output_data, .this.folder.1, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(.path.output_data, .this.folder.1, "input_prop_zeros.csv"))
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

input.dispersion.param <- input.prop.zeros

for(i in 1:6){
  for(j in 2:7){
    input.dispersion.param[i,j] <- SolveForSigmaSquared(input.mu = input.means[i,j], input.prop.zeros = input.prop.zeros[i,j])
  }
}


input.dispersion.param.1 <- input.dispersion.param
input.means.1 <- input.means
input.prop.zeros.1 <- input.prop.zeros

write.csv(input.dispersion.param, file.path(.path.output_data, .this.folder.1, "calculated_dispersion_param.csv"), row.names = FALSE)

# Begin steps -----------------------------------------------------------------
input.means <- read.csv(file.path(.path.output_data, .this.folder.2, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(.path.output_data, .this.folder.2, "input_prop_zeros.csv"))
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

input.dispersion.param <- input.prop.zeros

for(i in 1:6){
  for(j in 2:7){
    input.dispersion.param[i,j] <- SolveForSigmaSquared(input.mu = input.means[i,j], input.prop.zeros = input.prop.zeros[i,j])
  }
}

input.dispersion.param.2 <- input.dispersion.param
input.means.2 <- input.means
input.prop.zeros.2 <- input.prop.zeros

write.csv(input.dispersion.param, file.path(.path.output_data, .this.folder.2, "calculated_dispersion_param.csv"), row.names = FALSE)

# Begin steps -----------------------------------------------------------------
input.means <- read.csv(file.path(.path.output_data, .this.folder.3, "input_means.csv"))
input.prop.zeros  <- read.csv(file.path(.path.output_data, .this.folder.3, "input_prop_zeros.csv"))
this.pair <- 2
input.rand.time <- 2
input.tot.time <- 6
input.cutoff <- 0

# Check that input data is in the correct format
CheckInputData(input.df = input.means, rand.time = input.rand.time, tot.time = input.tot.time)
CheckInputData(input.df = input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)

input.dispersion.param <- input.prop.zeros

for(i in 1:6){
  for(j in 2:7){
    input.dispersion.param[i,j] <- SolveForSigmaSquared(input.mu = input.means[i,j], input.prop.zeros = input.prop.zeros[i,j])
  }
}

input.dispersion.param.3 <- input.dispersion.param
input.means.3 <- input.means
input.prop.zeros.3 <- input.prop.zeros

write.csv(input.dispersion.param, file.path(.path.output_data, .this.folder.3, "calculated_dispersion_param.csv"), row.names = FALSE)



