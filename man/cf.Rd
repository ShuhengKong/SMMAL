% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf.R
\name{cf}
\alias{cf}
\title{Cross-Fitting with Model Selection and Log Loss Evaluation}
\usage{
cf(Y, X, K, R, foldid, cf_model, subset = rep(TRUE, length(Y)))
}
\arguments{
\item{Y}{Numeric or factor vector. Response variable. Can be binary (0/1) or continuous. Only labelled observations (R = 1) are used.}

\item{X}{Matrix or data frame. Covariates used for model training.}

\item{K}{Integer. Number of cross-fitting folds.}

\item{R}{Binary vector. 1 indicates labelled data; 0 indicates unlabelled. Used to filter training samples.}

\item{foldid}{Integer vector. Fold assignments for cross-fitting (length = full dataset).}

\item{cf_model}{Character string. Name of the model to use. One of \code{"xgboost"}, \code{"bspline"}, or \code{"randomforest"}.}

\item{subset}{Logical vector. Indicates which samples (among labelled) to include in training. Defaults to all \code{TRUE}.}
}
\value{
A list with:
\describe{
\item{models}{(Currently placeholder) Trained models per fold and tuning round.}
\item{predictions}{A list of length 5 (one per tuning round) of out-of-fold predictions.}
\item{log_losses}{Vector of log loss values for each tuning round.}
\item{Tuning_Parameter}{The hyperparameter varied across the 5 rounds (e.g., gamma, number of knots, nodesize).}
\item{best_rounds_index}{Index (1-5) of the round with the lowest log loss.}
\item{best_rounds_log_losses}{Minimum log loss achieved.}
\item{best_rounds_prediction}{Vector of out-of-fold predictions from the best tuning round.}
}
}
\description{
Trains and evaluates a predictive model using cross-fitting across \code{K} folds,
supporting multiple learner types. Returns out-of-fold predictions and log loss for each
tuning round to select the best-performing model.
}
\details{
The function supports three types of models:
\itemize{
\item{\strong{xgboost}}: Trains gradient-boosted trees with varying \code{gamma} across rounds.
\item{\strong{bspline}}: Trains logistic regression with B-spline basis features and different knot numbers.
\item{\strong{randomforest}}: Trains random forests with varying \code{nodesize}.
}
At each round, cross-fitting is applied to prevent overfitting. Out-of-fold predictions are collected,
and log loss is computed to identify the best hyperparameter setting.
}
