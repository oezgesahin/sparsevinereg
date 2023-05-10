#' internal function
#' @noRd
fit_info <- function(object){
  cat("method =", object$method, "  ")
  cat("vine =", object$vinegraph, "  ")
  cat("vars_indx =", object$vars_indx)
  cat("\n")
}


#' internal function
#' @noRd
initial_df_check <- function(data){
  data <- data.frame(data)
  if(is.null(dim(data)[1])){
    stop("data must contain at least 2 variables")
  }
  else{
    nrow_df <- dim(data)[1]
    ncol_df <- dim(data)[2]
  }
  if(nrow_df == 0)
    stop("data does not contain any observations")

  if(any(sapply(data, is.na)))
    stop("data must not contain any missing values")

  if(!all(sapply(data, is.numeric)))
    stop("data must be numeric")
}


#' internal function
#' @noRd
initial_args_check <- function(df, alpha, varsel, vinegraph, cores_vine, cores_varsel, crit_vine){

  if(!all(alpha > 0 & alpha < 1))
    stop("quantile level alpha must be larger than 0 and smaller than 1")

  if(!all(is.element(varsel,c('ParCor', 'resid'))))
    stop("Variable selection method must be ParCor or resid")

  if(!all(is.element(vinegraph,c('Dvine'))))
    stop("Allowed vine graphs are Dvine")

  if(!is.numeric(cores_vine))
    stop("Number of cores must be numeric")

  if(cores_vine <= 0)
    stop("Number of cores must be larger than 0")

  if(!is.numeric(cores_varsel))
    stop("Number of cores must be numeric")

  if(cores_varsel <= 0)
    stop("Number of cores must be larger than 0")

  if(!all(is.element(crit_vine,c('caic', 'cll', 'cbic'))))
    stop("The criterion to choose among vine copula models must be cll, caic, or cbic")
}
