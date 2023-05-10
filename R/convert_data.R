#' internal function
#' @noRd

norm_score <- function(df){
  n_col <- ncol(df)
  n_row <- nrow(df)
  emp_scrs <- matrix(0,n_row,n_col)
  qn <- stats::qnorm(((1:n_row) - 0.5) /n_row)
  for (j in 1:n_col){
    temp <- rank(df[,j])
    z_scr <- qn[temp]
    emp_scrs[,j] <- z_scr
  }
  emp_scrs
}


#' internal function
#' @noRd
u_scale <- function(var){
  fit_kde1d <- kde1d::kde1d(var)
  u_var <-  kde1d::pkde1d(var, fit_kde1d)
  u_var
}
