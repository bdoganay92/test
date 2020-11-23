# About CountSMART

Longitudinal count data having an excessive number of zeros (EZ) are often collected in a variety of health domains. This repository contains code to estimate sample size needed to compare dynamic treatment regimens using longitudinal count outcomes with EZ from a Sequential Multiple Assignment Randomized Trial (SMART). 

<br/>
A pair of dynamic treatment regimens $(a_1', a_2')$ and $(a_1'', a_2'')$ embedded in a planned SMART (aka. 'EDTRs') can be compared using differences in end-of-study means, or more generally, differences in a weighted average of means across various time points. More formally, these EDTRs can be compared using the criterion $\Delta_Q = \sum_{j=1}^T l_j \mu_{t_j}^{(a_1', a_2')} - \sum_{j=1}^T l_j \mu_{t_j}^{(a_1', a_2')}$, where $\mu_{t_j}^{(a_1, a_2)}$ represents the mean of a (potential) count outcome under $(a_1, a_2)$ at time $t_j$. 

<br/>
This repository contains code implementing a Monte Carlo simulation-based approach to estimate sample size required to attain power of $1-\eta$ to the test of the null $H_0:\Delta_Q=0$ against the alternative $H_a:\Delta_Q\neq0$ at type-I error $\alpha$. 

# About this repository

## 1. The `code` folder

### 1.1 Collection of functions for input-checking, simulation, and data analysis

| <img height=0 width=380> File Name <img height=0 width=380> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[input-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/input-utils.R) | Contains a function for checking validity of time-specific means and proportion of zeros provided as inputs to the sample size estimation procedure.
[datagen-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/datagen-utils.R) | Collection of functions to generate potential outcomes and observed outcomes.
[analysis-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/analysis-utils.R) | Collection of functions to 'analyze' data from a SMART.

### 1.2 Collection of functions for executing calculations

| <img height=0 width=380> File Name <img height=0 width=380> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[calc-bias.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-bias.R) | Calculate bias in estimates of means and estimates of standard errors.
[calc-covmat.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-covmat.R) | Calculate estimated covariance matrix.
[calc-estimates.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-estimates.R) | Calculate estimates of parameters in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART.
[calc-params-curve.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-params-curve.R) | Implement simulation to estimate relationship between $\rho$ and $\tau_{AVE}$
[calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) | Calculate true value of parameters in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R) | Calculate true value of$\Delta_Q$ in a model for the mean trajectory of dynamic treatment regimens embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[calc-truth-deltaQ.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-deltaQ.R) | Wrapper for [calc-truth-beta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-beta.R) and [calc-truth-contrasts.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-truth-contrasts.R). Visualize true mean trajectory of each dynamic treatment regimen embedded in a SMART, implied by inputs provided to Monte Carlo simulation.
[geemMod.R](https://github.com/jamieyap/CountSMART/tree/master/code/geemMod.R) | Modification of the `geem.R` script from the R package `geeM`: setting the additional argument `fullmat=TRUE` allows custom specification of working correlation matrix for each participant-time.

## 2. Contents of the `output` folder
| <img height=0 width=380> File Name <img height=0 width=380> | <img height=0 width=800> Brief Description <img height=0 width=800> |
|:------------------------------------------:|:--------------------------------------------------------------------------------------------------|
[sim_study_main](https://github.com/jamieyap/CountSMART/tree/master/output/sim_study_main) | A directory containing a collection of scripts to execute the main simulation studies.
[sim_study_supp](https://github.com/jamieyap/CountSMART/tree/master/output/sim_study_supp) | A directory containing a collection of scripts to execute supplementary simulation studies.

