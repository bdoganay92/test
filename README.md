# CountSMART

## 0. About CountSMART

This repository contains code to estimate sample size needed to compare dynamic treatment regimens using longitudinal count outcomes from a Sequential Multiple Assignment Randomized Trial (SMART) via simulation.

## 1. About This Repository

This repository contains code for performing sample size estimation via simulation. Files corresponding to particular stages of the project are placed under the relevant header.

## 2. Before Proceeding

Questions about the material in this repository can be directed at https://github.com/jamieyap/CountSMART/issues by submitting a `New issue`. Prior to running the code, ensure that steps described in [prep.pdf](https://github.com/jamieyap/CountSMART/tree/master/run-examples/prep.pdf) have been performed.

## 3. Contents of `code` Folder

File | Description
------------------------ | -------------------------
[input-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/input-utils.R) | Contains a function for checking validity of time-specific means and proportion of zeros provided as inputs to the sample size estimation procedure. [input_means.csv](https://github.com/jamieyap/CountSMART/tree/master/run-examples/dat-example-01a/input_means.csv) and [input_prop_zeros.csv](https://github.com/jamieyap/CountSMART/tree/master/run-examples/dat-example-01a/input_prop_zeros.csv)  provide examples of valid inputs.
[datagen-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/datagen-utils.R) | Collection of functions to generate potential outcomes and observed outcomes.
[analysis-utils.R](https://github.com/jamieyap/CountSMART/tree/master/code/analysis-utils.R) | Collection of functions to 'analyze' data from a SMART.
[calibrate-params.R](https://github.com/jamieyap/CountSMART/tree/master/code/calibrate-params.R) | Implements calibration stage of the sample size estimation method.
[calc-delta.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-delta.R) |  Implements calculation of standardized effect size of the sample size estimation method.
[calc-power.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-power.R) |  Implements power calculation stage of the sample size estimation method.
[estimate-sample-size.R](https://github.com/jamieyap/CountSMART/tree/master/code/estimate-sample-size.R) | Calls [calc-power.R](https://github.com/jamieyap/CountSMART/tree/master/code/calc-power.R) to estimate sample size for various candidate sample sizes.

## 4. Contents of `run-examples` Folder

File | Description
------------------------ | -------------------------
[example-01a.R](https://github.com/jamieyap/CountSMART/tree/master/run-examples/example-01a.R) | Calls code in `code` folder to generate estimates presented in example 1 when tauMAX=0.70.

