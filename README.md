# SMMAL

<!-- badges: start -->
<!-- badges: end -->

SMMAL is an R package for estimating the Average Treatment Effect (ATE)
using semi-supervised learning (SSL), tailored for settings with limited
treatment/outcome labels but rich covariates and surrogate variables. It
enhances efficiency and robustness over supervised methods by leveraging
unlabeled data and supports high-dimensional models via cross-fitting,
flexible model fitting, and adaptive LASSO.

## Installation

A github version can be found at this link:
<https://github.com/ShuhengKong/SMMAL>

## Example

This is a basic example which shows you how to solve a common problem:

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
    #> [1] 0.09714911
    #> 
    #> $se
    #> [1] 0.02966695

## SMMAL input files

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr class="header">
<th>Column</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Y</td>
<td>Observed outcomes. Can be continuous or binary</td>
</tr>
<tr class="even">
<td>A</td>
<td>Treatment indicator. Must be binary</td>
</tr>
<tr class="odd">
<td>S</td>
<td>Surrogates</td>
</tr>
<tr class="even">
<td>X</td>
<td>Covariates</td>
</tr>
<tr class="odd">
<td>nfold</td>
<td>Number of cross-validation folds. Default is 5.</td>
</tr>
<tr class="even">
<td>cf_model</td>
<td>The modeling method to use in cross-fitting. Default is “bspline”.
Other values are “xgboost”,“randomforest”</td>
</tr>
</tbody>
</table>

## SMMAL output

<table>
<thead>
<tr class="header">
<th>Column</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>est</td>
<td>estimated value of ATE</td>
</tr>
<tr class="even">
<td>se</td>
<td>standard error of ATE</td>
</tr>
</tbody>
</table>
