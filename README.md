# About CountSMART

Longitudinal count data having an excessive number of zeros (EZ) are often collected in a variety of health domains. This repository contains code to estimate sample size needed to compare dynamic treatment regimens using longitudinal count outcomes with EZ from a Sequential Multiple Assignment Randomized Trial (SMART). A pair of dynamic treatment regimens embedded in a planned SMART (aka. 'EDTRs') can be compared using differences in end-of-study means, or more generally, differences in a weighted average of means across various time points, which we denote as <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$">.

CountSMART is about a Monte Carlo simulation-based approach developed to estimate sample size required to attain power of <img src="https://render.githubusercontent.com/render/math?math=$1-\eta$"> to the test of the null <img src="https://render.githubusercontent.com/render/math?math=$H_0:\Delta_Q=0$"> against the alternative <img src="https://render.githubusercontent.com/render/math?math=$H_a:\Delta_Q\neq0$"> at type-I error <img src="https://render.githubusercontent.com/render/math?math=$\alpha$">. 

# About this repository

This repository contains code implementing CountSMART methodology (the `code` folder), simulation studies examining the validity of the approach (the `output` folder), and examples of how to apply CountSMART methodology to planning SMARTs with longitudinal count outcomes with EZ (the `examples` folder).

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
[calc-bias.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-bias.R) | Calculate bias in estimates of means and estimates of standard errors.
[calc-covmat.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-covmat.R) | Calculate estimated covariance matrix.
[calc-estimates.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-estimates.R) | Calculate estimates of parameters in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART.
[calc-params-curve.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-params-curve.R) | Implement simulation to estimate relationship between <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> and <img src="https://render.githubusercontent.com/render/math?math=$\tau_{AVE}$">
[calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) | Calculate true value of parameters in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R) | Calculate true value of <img src="https://render.githubusercontent.com/render/math?math=$\Delta_Q$"> in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[calc-truth-deltaQ.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-deltaQ.R) | Wrapper for [calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) and [calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R). Visualize true mean trajectory of each dynamic treatment regimen embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[geemMod.R](https://github.com/jamieyap/CountSMART/tree/master/code/geemMod.R) | Modification of the `geem.R` script from the R package `geeM`: setting the additional argument `fullmat=TRUE` allows custom specification of working correlation matrix for each participant-time.

## 2. Contents of the `output` folder
| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[sim_study_main](https://github.com/jamieyap/CountSMART/tree/master/output/sim_study_main) | A directory containing a collection of scripts to execute the main simulation studies.
[sim_study_supp](https://github.com/jamieyap/CountSMART/tree/master/output/sim_study_supp) | A directory containing a collection of scripts to execute supplementary simulation studies.

## 3. The `examples` folder
| <img height=0 width=350> File Name <img height=0 width=350> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[example_background.pdf](https://github.com/jamieyap/CountSMART/tree/master/examples/example_background.pdf) | Describe SMART design used in illustrative examples in [example_choosing_rho.pdf](https://github.com/jamieyap/CountSMART/tree/master/examples/example_choosing_rho.pdf) and [example_power_calc.pdf](https://github.com/jamieyap/CountSMART/tree/master/examples/example_power_calc.pdf).
[example_choosing_rho.pdf](https://github.com/jamieyap/CountSMART/tree/master/examples/example_choosing_rho.pdf) | An example illustrating how to employ code in this repository to estimate relationship between <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> and <img src="https://render.githubusercontent.com/render/math?math=$\tau_{AVE}$"> and select the value of <img src="https://render.githubusercontent.com/render/math?math=$\rho$"> to use in [example_power_calc.pdf](https://github.com/jamieyap/CountSMART/tree/master/examples/example_power_calc.pdf)
[example_power_calc.pdf](https://github.com/jamieyap/CountSMART/tree/master/examples/example_power_calc.pdf) | An example illustrating how to employ code in this repository to calculate power.


