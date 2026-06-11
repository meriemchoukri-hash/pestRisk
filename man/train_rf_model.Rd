\name{train_rf_model}
\alias{train_rf_model}
\title{Train a Random Forest SDM for pest risk modelling}
\usage{
train_rf_model(
  model_data,
  predictors = NULL,
  n_trees = 500L,
  test_frac = 0.25,
  balance = TRUE,
  seed = 42L,
  verbose = TRUE
)
}
\arguments{
\item{model_data}{\code{data.frame}. Output of
\code{\link{prepare_predictors}}$\code{model_data}. Must contain
a column \code{presence} (1/0) and predictor columns.}

\item{predictors}{\code{character vector}. Names of predictor columns.
If \code{NULL}, all columns except \code{lon}, \code{lat},
\code{presence} are used.}

\item{n_trees}{\code{integer}. Number of trees in the forest
(default \code{500}).}

\item{test_frac}{\code{numeric}. Fraction of data for the test set
(default \code{0.25}).}

\item{balance}{\code{logical}. If \code{TRUE} (default), downsample the
majority class to produce a balanced 1:1 training set.}

\item{seed}{\code{integer}. Random seed for reproducibility
(default \code{42}).}

\item{verbose}{\code{logical}. Print training progress (default \code{TRUE}).}
}
\value{
A named \code{list} with:
\describe{
  \item{\code{model}}{Fitted \code{randomForest} object.}
  \item{\code{importance}}{\code{data.frame} of variable importance
    (MeanDecreaseGini), sorted descending.}
  \item{\code{train_data}}{Training subset used.}
  \item{\code{test_data}}{Held-out test subset.}
  \item{\code{predictors}}{Names of predictor variables used.}
  \item{\code{type}}{\code{"rf"} tag for downstream functions.}
}
}
\description{
Trains a Random Forest classifier to predict the presence probability of
an agricultural pest from bioclimatic predictors. Handles class imbalance
via downsampling, performs stratified train/test split, and returns the
fitted model with variable importance scores.
}
\details{
Random Forest is well-suited for SDMs because it captures non-linear
relationships and feature interactions, is robust to noisy predictors,
and provides interpretable variable importance via MeanDecreaseGini.

Class imbalance (background >> presences) is corrected with
\code{balance = TRUE}: the majority class is downsampled to match the
minority class size in the training set, improving AUC and sensitivity.
}
\examples{
path       <- system.file("extdata", "spodoptera_model_data.csv",
                           package = "pestRisk")
model_data <- read.csv(path)

rf_out <- train_rf_model(model_data, n_trees = 100, verbose = TRUE)
print(rf_out$importance)
}
\references{
Breiman, L. (2001). Random Forests. \emph{Machine Learning} \strong{45},
5-32. \doi{10.1023/A:1010933404324}
}
