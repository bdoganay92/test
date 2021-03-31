[![DOI](https://zenodo.org/badge/212328991.svg)](https://zenodo.org/badge/latestdoi/212328991)

# About CountSMART

Longitudinal count data having an excessive number of zeros (EZ) are often collected in a variety of health domains. This repository contains code to estimate sample size needed to compare dynamic treatment regimens using longitudinal count outcomes with EZ from a Sequential Multiple Assignment Randomized Trial (SMART). A pair of dynamic treatment regimens embedded in a planned SMART (aka. 'EDTRs') can be compared using differences in end-of-study means, or more generally, differences in a weighted average of means across various time points, which we denote as <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$">; _Q_ is simply shorthand for 'quantity', e.g., <img src="https://render.githubusercontent.com/render/math?math=$\Delta_{EOS}$"> denotes the quantity _difference in end-of-study means_.

CountSMART is about a Monte Carlo simulation-based approach developed to estimate sample size required to attain power of <img src="https://render.githubusercontent.com/render/math?math=$1-\eta$"> to the test of the null <img src="https://render.githubusercontent.com/render/math?math=$H_0:\Delta_Q=0$"> against the alternative <img src="https://render.githubusercontent.com/render/math?math=$H_a:\Delta_Q\neq0$"> at type-I error <img src="https://render.githubusercontent.com/render/math?math=$\alpha$">. 

# About this repository

This repository contains code implementing CountSMART methodology (the `code` folder) and simulation studies examining the validity of the approach (the `output` folder).

## 1. The `code` folder

### 1.1 Collection of functions for input-checking, simulation, and data analysis

| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[input-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/input-utils.R) | Contains a function for checking validity of time-specific means and proportion of zeros provided as inputs to the sample size estimation procedure.
[datagen-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/datagen-utils.R) | Collection of functions to generate potential outcomes and observed outcomes.
[analysis-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/analysis-utils.R) | Collection of functions to 'analyze' data from a SMART.

### 1.2 Collection of functions for executing calculations

| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[calc-covmat.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-covmat.R) | Calculate estimated covariance matrix.
[calc-params-curve.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-params-curve.R) | Implement simulation to estimate relationship between <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> and <img src="https://render.githubusercontent.com/render/math?math=$\tau_{MAX}$"> and the relationship between <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> and <img src="https://render.githubusercontent.com/render/math?math=$\tau_{MIN}$">.
[calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) | Calculate true value of parameters in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R) | Calculate true value of <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$"> in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[calc-truth-deltaQ.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-deltaQ.R) | Wrapper for [calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) and [calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R). Visualize true mean trajectory of each dynamic treatment regimen embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[geemMod.R](https://github.com/jamieyap/CountSMART/tree/master/code/geemMod.R) | Modification of the `geem.R` script from the R package `geeM`: setting the additional argument `fullmat=TRUE` allows custom specification of working correlation matrix for each participant-time.

## 2. The `output` folder
| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[sim_size_test](https://github.com/jamieyap/CountSMART/tree/master/output/sim_size_test) | A directory containing a collection of scripts to execute simulation studies concerning empirical type-I error rate.
[sim_sensitivity_group_four](https://github.com/jamieyap/CountSMART/tree/master/output/sim_sensitivity_group_four) | A directory containing a collection of scripts to execute simulation studies investigating whether power is sensitive to a violation in our working assumption on the number of individuals who would not respond to either first-stage intervention option.
[sim_vary_effect](https://github.com/jamieyap/CountSMART/tree/master/output/sim_vary_effect) | A directory containing a collection of scripts to execute simulation studies  investigating how power changes as specific choices of <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$"> are increased across a grid of total sample sizes N=100, 150, 200, …, 550, 600.

