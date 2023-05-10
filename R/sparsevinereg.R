#' High-dimensional sparse vine copula regression
#'
#' It selects variables from high-dimensional continuous data
#' to make vine copula based (quantile) univariate predictions.
#'
#' @param df {matrix, data frame.} data on x-scale whose first column contains the variable
#' of interest to predict (response).
#' If a matrix or data frame, rows correspond to observations (i) and columns correspond to
#' variables (p).
#' @param alpha {numeric, 0-1.} Quantile level for which the residuals are estimated to perform
#' variable selection.
#' @param varsel {list, character.} The list of variable selection methods.
#' * ParCor = Partial correlation based selection (default).
#' * resid = Residual estimation based selection (default).
#' @param vinegraph {list, character.} The list of vine graphs to fit.
#' * Dvine = D-vine (default).
#' @param cores_vine {positive integer.} Number of cores to use for parallel computations
#' to fit vine copula models.
#' @param cores_varsel {positive integer.} Number of cores to use for parallel computations
#' to select variables with the residual estimation based method.
#' @param crit_vine {character.} The criterion to choose among vine copula models
#' using different variable selection methods.
#' * caic = Conditional AIC (default).
#' * cbic = Conditional BIC.
#' * cll = Conditional log-likelihood.
#'
#' @return An object of class sparsevinereg result. It contains the elements
#' \describe{
#' \item{method}{method used for variable selection in the final model.}
#' \item{vinegraph}{vine graph of the final model.}
#' \item{vars_indx}{indices of selected variables.}
#' \item{vinereg_fit}{the object class of  \link[vinereg]{vinereg}.}
#' } Use `print.sparsevinereg()` to print the variable selection method, vine graph, and
#' selected variable indices of the final model. `summary.sparsevinereg()` shows the
#'final model's vine and margins.
#'
#' @references
#' Sahin, O.,and Czado, C. (2022). High-dimensional sparse vine copula regression with application
#' to genomic prediction. arXiv preprint arXiv:2208.12383.
#'
#' @seealso [predict.sparsevinereg()]
#'
#' @examples
#' # Simulate data with 5 relevant and 5 irrelevant variables
#' set.seed(11)
#' x <- matrix(rnorm(5000), 500, 10)
#' y <- x[,1] -2*x[,2] + 3*x[,3] + 5*x[,4] - 4*x[,5]
#'
#' # response is in the first column of the data
#' data <- data.frame(y = y, x = x)
#'
#' # fit a sparse vine copula based regression model
#' fit <- sparsevinereg(data)
#'
#' # print the final model
#' print(fit)
#'
#' # make predictions with the model
#' median_pred <- predict(fit, data = data, alpha = 0.5) # median
#' mean_pred <- predict(fit, data = data, alpha = NA) # mean
#'
#' @export
#'
#' @import vinereg
#' @import kde1d
#' @importFrom parallel mclapply
#' @importFrom stats cor qnorm


sparsevinereg <- function(df, alpha=0.50, varsel=c("ParCor", "resid"),
                          vinegraph=c("Dvine"), cores_vine=1, cores_varsel=1, crit_vine='caic'){
  initial_df_check(df)
  initial_args_check(df, alpha, varsel, vinegraph, cores_vine, cores_varsel, crit_vine)
  thr_caic <- 1000000
  for(i in varsel){
    for(j in vinegraph){
      if(i=="ParCor" & j=="Dvine"){
        pred_mdl <- vineregParCor(df, cores_vine)
      }
      else if(i=="resid" & j=="Dvine"){
        pred_mdl <- vineregRes(df, alpha, cores_vine, cores_varsel)
      }
      else{
        print('The requested method has not been implemented.')
      }
    }
    cnd_caic <- pred_mdl$vinereg_fit$stats[[crit_vine]]
    if(cnd_caic < thr_caic){
      thr_caic <- cnd_caic
      winner <- pred_mdl
    }
  }
  class(winner) <- "sparsevinereg"
  winner
}


#' @export
print.sparsevinereg <- function(x,...) {
  fit_info(x)
  invisible(x)
}

#' @export
summary.sparsevinereg <- function(object, ...) {
  list(
    vine = object$vinereg_fit$vine,
    margins = object$vinereg_fit$margins
  )
}
