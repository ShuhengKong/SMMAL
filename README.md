
# SMMAL

SMMAL is an R package for estimating the Average Treatment Effect (ATE)
using semi-supervised learning (SSL), tailored for settings with limited
treatment/outcome labels but rich covariates and surrogate variables. It
enhances efficiency and robustness over supervised methods by leveraging
unlabeled data and supports high-dimensional models via cross-fitting,
flexible model fitting, and adaptive LASSO.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("ShuhengKong/SMMAL")
```

A github version can be found at this link:
<https://github.com/ShuhengKong/SMMAL>

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SMMAL)

# Load the example dataset included with the package
file_path <- system.file("extdata", "sample_data.rds", package = "SMMAL")
dat <- readRDS(file_path)

temp <- data.frame(dat$X)
temp[,] <- NA
# Estimate ATE using the SMMAL pipeline
output <- SMMAL(
  Y = dat$Y,
  A = dat$A,
  S = data.frame(dat$S),
  X = data.frame(dat$X),
  nfold = 5,
  cf_model = "bspline"
)

# View the results
print(output)
#> $est
#> [1] 0.09971294
#> 
#> $se
#> [1] 0.0296998
```

## SMMAL input files

| Column | Description |
|:---|:---|
| Y | Observed outcomes. Can be continuous or binary |
| A | Treatment indicator. Must be binary |
| S | Surrogates |
| X | Covariates |
| nfold | Number of cross-validation folds. Default is 5. |
| cf_model | The modeling method to use in cross-fitting. Default is “bspline”. Other values are “xgboost”,“randomforest” |

## SMMAL output

| Column | Description            |
|:-------|:-----------------------|
| est    | estimated value of ATE |
| se     | standard error of ATE  |
