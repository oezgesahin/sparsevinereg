#' Ppredictions with the vine copula based regression model
#'
#' @param object an object of class \code{sparsevinereg}.
#' @param data matrix or data frame of covariate values to use in predictions.
#' @param alpha vector of (quantile) prediction levels. `NA` predicts the mean
#' based on an average of the `1:10 / 11`-quantiles.
#' @param ... unused.
#'
#' @return A data frame of predictions, where a column corresponds to a given
#' quantile level of `alpha`.
#'
#' @examples
#' # Simulate data with 5 relevant and 5 irrelevant variables
#' set.seed(11)
#' x <- matrix(rnorm(5000), 500, 10)
#' y <- x[,1] -2*x[,2] + 3*x[,3] + 5*x[,4] - 4*x[,5]
#'
#'  # response needs to be in the first column of the data
#'   data <- data.frame(y = y, x = x)
#'
#'  # fit a sparse vine copula based regression model
#'  fit <- sparsevinereg(data)
#'
#'  # print the final model
#'  print(fit)
#'
#'  # make predictions with the model
#'  median_pred <- predict(fit, data = data, alpha = 0.5) # median
#'  mean_pred <- predict(fit, data = data, alpha = NA) # mean
#'
#' @export
#'
#' @import vinereg

predict.sparsevinereg <- function(object, data, alpha, ...){
  vreg_object <- object$vinereg_fit
  if(is.na(alpha)){
    preds <- vinereg:::predict.vinereg(vreg_object, newdata=data, alpha=alpha)
    preds_final <- data.frame(mean = rowMeans(preds))
  }
  else{
    preds_final <- vinereg:::predict.vinereg(vreg_object, newdata=data, alpha=alpha)
  }
  preds_final
}
