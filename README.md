# About CountSMART

This repository contains code to estimate sample size needed to compare dynamic treatment regimens using longitudinal count outcomes from a Sequential Multiple Assignment Randomized Trial (SMART) via simulation.

# Contents of the `code` folder

File | Description
------------------------ | -------------------------
[input-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/input-utils.R) | Contains a function for checking validity of time-specific means and proportion of zeros provided as inputs to the sample size estimation procedure. [input_means.csv](https://github.com/jamieyap/CountSMART/tree/master/run-examples/dat-example-01a/input_means.csv) and [input_prop_zeros.csv](https://github.com/jamieyap/CountSMART/tree/master/run-examples/dat-example-01a/input_prop_zeros.csv)  provide examples of valid inputs.
[datagen-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/datagen-utils.R) | Collection of functions to generate potential outcomes and observed outcomes.
[analysis-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/analysis-utils.R) | Collection of functions to 'analyze' data from a SMART.
[params-curve.R](https://github.com/jamieyap/CountSMART/tree/master/code/params-curve.R) | Implements grid search to determine value of dependence parameter to use in simulation.
[calc-power.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-power.R) |  Implements power calculation algorithm.
[calc-N.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-N.R) |  Implements sample size estimation algorithm.
[calc-coverage.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-coverage.R) |  Calculates coverage of estimates.
