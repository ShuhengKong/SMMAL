% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf.R
\name{cf}
\alias{cf}
\title{Cross-Fitting with Model Selection and Log Loss Evaluation}
\usage{
cf(
  Y,
  X,
  nfold,
  R,
  foldid,
  cf_model,
  sub_set = rep(TRUE, length(Y)),
  custom_model_fun = NULL
)
}
\arguments{
\item{Y}{Numeric or factor vector. The response variable, either binary (0/1) or continuous.
Only labelled observations (where \code{R = 1}) are used.}

\item{X}{Matrix or data frame. Predictor variables used for model training.}

\item{nfold}{Integer. Number of cross-fitting folds.}

\item{R}{Binary vector. Indicator of labelled data: 1 = labelled, 0 = unlabelled.}

\item{foldid}{Integer vector. Fold assignments for cross-fitting (length equal to the full dataset).}

\item{cf_model}{Character string. Specifies the model type. Must be one of \code{"xgboost"}, \code{"bspline"}, or \code{"randomforest"}.}

\item{sub_set}{Logical vector. Indicates which labelled samples to include in training.}

\item{custom_model_fun}{A logical or function. If \code{NULL} or \code{FALSE}, bypasses adaptive-LASSO feature selection. Otherwise, enables two-stage tuning inside \code{compute_parameter()}.
Defaults to all \code{TRUE}.}
}
\value{
A list containing:
\describe{
\item{models}{(Currently a placeholder) List of trained models per fold and tuning round.}
\item{predictions}{List of out-of-fold predictions for each of the 5 tuning rounds.}
\item{log_losses}{Numeric vector of log loss values for each tuning round.}
\item{best_rounds_index}{Integer index (1–5) of the round achieving the lowest \code{log_loss}.}
\item{best_rounds_log_losses}{Minimum \code{log_loss} value achieved across rounds.}
\item{best_rounds_prediction}{Vector of out-of-fold predictions from the best tuning round.}
}
}
\description{
Trains and evaluates predictive models using cross-fitting across \code{nfold} folds,
supporting multiple learner types. Outputs out-of-fold predictions and computes \code{log_loss}
for each hyperparameter tuning round to select the best-performing model.
}
\details{
The function supports three learner types:
\itemize{
\item{\strong{xgboost}}: Gradient-boosted trees, tuning \code{gamma} across rounds.
\item{\strong{bspline}}: Logistic regression using B-spline basis expansions, tuning the number of knots.
\item{\strong{randomforest}}: Random forests, tuning \code{nodesize}.
}

Cross-fitting ensures that model evaluation is based on out-of-fold predictions,
reducing overfitting. \code{log_loss} is used as the evaluation metric to identify the
best hyperparameter setting.
}
\examples{
set.seed(123)
N <- 200
X <- matrix(rnorm(N * 5), nrow = N, ncol = 5)

# Simulate treatment assignment
A <- rbinom(N, 1, plogis(X[, 1] - 0.5 * X[, 2]))

# Simulate outcome
Y_full <- rbinom(N, 1, plogis(0.5 * X[, 1] - 0.25 * X[, 3]))

# Introduce some missingness to simulate semi-supervised data
Y <- Y_full
Y[sample(1:N, size = N/4)] <- NA  # 25\% missing

# Create R vector (labelled = 1, unlabelled = 0)
R <- ifelse(!is.na(Y), 1, 0)

# Cross-validation fold assignment
foldid <- sample(rep(1:5, length.out = N))

# Run cf with bspline model
result <- cf(Y = Y, X = X, nfold = 5, R = R, foldid = foldid, cf_model = "bspline")

# Examine output
print(result$log_losses)
print(result$best_rounds_index)

}
