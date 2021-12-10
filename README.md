# About CountSMART

Longitudinal count data having an excessive number of zeros (EZ) are often collected in a variety of health domains. This repository contains code to estimate sample size needed to compare dynamic treatment regimens using longitudinal count outcomes with EZ from a Sequential Multiple Assignment Randomized Trial (SMART). A pair of dynamic treatment regimens embedded in a planned SMART (aka. 'EDTRs') can be compared using differences in end-of-study means, or more generally, differences in a weighted average of means across various time points, which we denote as <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$">; _Q_ is simply shorthand for 'quantity', e.g., <img src="https://render.githubusercontent.com/render/math?math=$\Delta_{EOS}$"> denotes the quantity _difference in end-of-study means_.

CountSMART is about a Monte Carlo simulation-based approach developed to estimate sample size required to attain power of <img src="https://render.githubusercontent.com/render/math?math=$1-\eta$"> to the test of the null <img src="https://render.githubusercontent.com/render/math?math=$H_0:\Delta_Q=0$"> against the alternative <img src="https://render.githubusercontent.com/render/math?math=$H_a:\Delta_Q\neq0$"> at type-I error <img src="https://render.githubusercontent.com/render/math?math=$\alpha$">. 

# About this repository

This repository contains code implementing CountSMART methodology and simulation studies examining the validity of the approach.

## 1. Setting up this repository

### 1.1 Packages used in the project

1. The collection of packages and their version numbers used for this repository are recorded in the renv.lock file. The package, renv, can facilitate installation of these packages in the machine of end-users of this repository. See renv package documentation here for more details: https://rstudio.github.io/renv/articles/renv.html

### 1.2 Tell R where to pull code from from and where to push data to

1. Create a new R file named 'paths.R' and save this file within the root directory of the repository (usually where the .Rproj file is located).
2. Within 'paths.R', set the value of the following variables below by replacing the three dots '...' with the appropriate directory. 

* path.output_data = ".../output"
* path.code = ".../code"
* path.plots = ".../plots"

Note that 'paths.R' is included in the '.gitignore' file, preventing any user-specific directories from being displayed in the repository. Also, since 'paths.R' is included in the '.gitignore' file, a new 'paths.R' file would need to be created by each end-user of the repository.

## 2. The `code` folder

### 2.1 Collection of functions for input-checking, simulation, and data analysis

| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[input-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/input-utils.R) | Contains a function for checking validity of time-specific means and proportion of zeros provided as inputs to the sample size estimation procedure.
[datagen-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/datagen-utils.R) | Collection of functions to generate potential outcomes and observed outcomes.
[analysis-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/analysis-utils.R) | Collection of functions to 'analyze' data from a SMART.

### 2.2 Collection of functions for executing calculations

| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[calc-covmat.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-covmat.R) | Calculate estimated covariance matrix.
[calc-corr-params-curve.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-corr-params-curve.R) | Implement simulation to estimate relationship between <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> and <img src="https://render.githubusercontent.com/render/math?math=$\tau_{MAX}$"> and the relationship between <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> and <img src="https://render.githubusercontent.com/render/math?math=$\tau_{MIN}$">.
[calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) | Calculate true value of parameters in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R) | Calculate true value of <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$"> in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[plot-truth-deltaQ.R](https://github.com/jamieyap/CountSMART/tree/master/code/plot-truth-deltaQ.R) | Wrapper for [calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) and [calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R). Visualize true mean trajectory of each dynamic treatment regimen embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[geemMod.R](https://github.com/jamieyap/CountSMART/tree/master/code/geemMod.R) | Modification of the `geem.R` script from the R package `geeM`: setting the additional argument `fullmat=TRUE` allows custom specification of working correlation matrix for each participant-time.

## 3. The `output` folder

### Results using an autoregressive structure
| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[create-scenarios-ar.R](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/create-scenarios-ar.R) | A script to create simulation study scenarios.
[calculate-dispersion-param.R](https://github.com/jamieyap/CountSMART/blob/master/output/autoregressive/calculate-dispersion-param.R) | A script to calculate the value of the negative binomial dispersion parameter in the different simulation scenarios.
[simulation-study-pipeline-ar.R](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/simulation-study-pipeline-ar.R) | A script to document and run steps in the simulation study pipeline.
[sim_size_test](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_size_test) | A directory containing a collection of scripts to execute simulation studies concerning empirical type-I error rate.
[sim_vary_effect](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_vary_effect) | A directory containing a collection of scripts to execute simulation studies  investigating how power changes as specific choices of <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$"> are increased across a grid of total sample sizes N=100, 150, 200, …, 550
[sim_vary_n4](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_vary_n4) | A directory containing a collection of scripts to execute simulation studies investigating whether power is sensitive to a violation in our working assumption on the number of individuals who would not respond to either first-stage intervention option.
[sim_vary_eta](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_vary_eta) | A directory containing a collection of scripts to execute simulation studies investigating whether power is sensitive to the actual value of <img src="https://render.githubusercontent.com/render/math?math=$\eta$">, given fixed value of <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> and N.

### Results using an exchangeable structure
| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[create-scenarios-exch.R](https://github.com/jamieyap/CountSMART/tree/master/output/exchangeable/create-scenarios-exch.R) | A script to create simulation study scenarios.
[calculate-dispersion-param.R](https://github.com/jamieyap/CountSMART/blob/master/output/exchangeable/calculate-dispersion-param.R) | A script to calculate the value of the negative binomial dispersion parameter in the different simulation scenarios.
[simulation-study-pipeline-exch.R](https://github.com/jamieyap/CountSMART/tree/master/output/exchangeable/simulation-study-pipeline-ar.R) | A script to document and run steps in the simulation study pipeline.
[sim_vary_effect](https://github.com/jamieyap/CountSMART/tree/master/output/exchangeable/sim_vary_effect) | A directory containing a collection of scripts to execute simulation studies  investigating how power changes as specific choices of <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$"> are increased across a grid of total sample sizes N=100, 150, 200, …, 550

## 4. The `plots` folder

### Plot results using an autoregressive structure
| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[data-viz-pipeline-ar.R](https://github.com/jamieyap/CountSMART/tree/master/plots/autoregressive/data-viz-pipeline-ar.R) | A script to document and run steps in the data visualization pipeline.
[plot-sim-size-test.R](https://github.com/jamieyap/CountSMART/tree/master/plots/autoregressive/plot-sim-size-test.R) | A script to plot results in [sim_size_test](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_size_test)
[plot-sim-vary-effect.R](https://github.com/jamieyap/CountSMART/tree/master/plots/autoregressive/plot-sim-vary-effect.R) | A script to plot results in [sim_vary_effect](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_vary_effect)
[plot-sim-vary-n4.R](https://github.com/jamieyap/CountSMART/tree/master/plots/autoregressive/plot-sim-vary-n4.R) | A script to plot results in [sim_vary_n4](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_vary_n4)
[plot-sim-vary-eta.R](https://github.com/jamieyap/CountSMART/tree/master/plots/autoregressive/plot-sim-vary-eta.R) | A script to plot results in [sim_vary_eta](https://github.com/jamieyap/CountSMART/tree/master/output/autoregressive/sim_vary_eta)

### Plot results using an exchangeable structure
| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[data-viz-pipeline-exch.R](https://github.com/jamieyap/CountSMART/tree/master/plots/exchangeable/data-viz-pipeline-exch.R) | A script to document and run steps in the data visualization pipeline.
[plot-sim-vary-effect.R](https://github.com/jamieyap/CountSMART/tree/master/plots/exchangeable/plot-sim-vary-effect.R) | A script to plot results in [sim_vary_effect](https://github.com/jamieyap/CountSMART/tree/master/output/exchangeable/sim_vary_effect)

