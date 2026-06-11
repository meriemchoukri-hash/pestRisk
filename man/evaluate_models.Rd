\name{evaluate_models}
\alias{evaluate_models}
\title{Evaluate SDM model performance}
\usage{
evaluate_models(
  ...,
  model_names = NULL,
  threshold = 0.5,
  plot_roc = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{...}{One or more model objects returned by \code{\link{train_rf_model}}
or \code{\link{train_maxent}}.}

\item{model_names}{\code{character vector}. Labels for each model.
Defaults to \code{"RandomForest"}, \code{"MaxEnt"}, etc.}

\item{threshold}{\code{numeric} in (0, 1). Decision threshold for binary
classification (default \code{0.5}).}

\item{plot_roc}{\code{logical}. Generate a ROC curve ggplot
(default \code{TRUE}).}

\item{verbose}{\code{logical}. Print performance table (default \code{TRUE}).}
}
\value{
A named \code{list}:
\describe{
  \item{\code{metrics}}{\code{data.frame} with columns \code{Model},
    \code{AUC}, \code{Accuracy}, \code{Sensitivity}, \code{Specificity},
    \code{F1}.}
  \item{\code{roc_plot}}{A \code{ggplot} ROC curve, or \code{NULL}.}
  \item{\code{confusion_matrices}}{Named list of confusion matrices.}
}
}
\description{
Evaluates one or more fitted SDM models on their held-out test sets.
Computes AUC, Accuracy, Sensitivity (Recall), Specificity, and F1-score.
Returns a performance table and optionally plots ROC curves for model
comparison.
}
\details{
For pest surveillance, \strong{Sensitivity is more important than
Specificity}: a false negative (missed invasion zone) is more costly
than a false positive (unnecessary monitoring effort).

AUC interpretation:
\tabular{ll}{
  > 0.9 \tab Excellent \cr
  0.8 – 0.9 \tab Good \cr
  0.7 – 0.8 \tab Fair \cr
  < 0.7 \tab Poor \cr
}
}
\examples{
path       <- system.file("extdata", "spodoptera_model_data.csv",
                           package = "pestRisk")
model_data <- read.csv(path)

rf_out <- train_rf_model(model_data, n_trees = 100, verbose = FALSE)
mx_out <- train_maxent(model_data, verbose = FALSE)

eval <- evaluate_models(rf_out, mx_out,
                         model_names = c("Random Forest", "MaxEnt"))
print(eval$metrics)
print(eval$roc_plot)
}
