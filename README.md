# Train Interpretable, Spline Based, Additive, Surrogate Models<img src="man/figures/logo.png" align="right" width="150"/>

[![Build Status](https://api.travis-ci.org/ModelOriented/xspliner.png)](https://travis-ci.org/ModelOriented/xspliner)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/xspliner/master.svg)](https://codecov.io/github/ModelOriented/xspliner?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/xspliner)](https://cran.r-project.org/package=xspliner)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/xspliner?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/xspliner)

## Overview

The `xspliner` package is a collection of tools for training interpretable surrogate ML models.

The package helps to build simple, interpretable models that inherits informations provided by more complicated ones - resulting model may be treated as explanation of provided black box, that was supplied prior to the algorithm.
Provided functionality offers graphical and statistical evaluation both for overall model and its components.

Key functions: 

* `xspline()` or `model_surrogate_xspliner()` for training surrogate model, 
* `plot_model_comparison()` or `plot` generic for visual predictions comparison of surrogate and original ML model,
* `plot_variable_transition()` or `plot` generic for graphical presentation of variables profiles and related information,
* `summary()` for statistical comparison of surrogate and original ML models,
* `print()` for getting details about surrogate model components.
 
The approach that stands behind surrogate model construction offered by `xspliner` sums up below graphics:

![](vignettes/xspliner.png)

More details can be found in  [xspliner's page](https://modeloriented.github.io/xspliner).

## Installation

```{r}
# the easiest way to get xspliner is to install it from CRAN:
install.packages("xspliner")

# Or the the development version from GitHub:
devtools::install_github("ModelOriented/xspliner")
```
