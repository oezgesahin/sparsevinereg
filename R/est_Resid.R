#' internal function
#' @noRd

est_resid <- function(resp, cov, name_cnd=NA, alpha=NA, order=NA, cores_vine){
  data_upd <- data.frame(y = resp, cov)
  colnames(data_upd)[2:ncol(data_upd)] <- name_cnd
  if(is.na(order)){
    fit_upd <- vinereg::vinereg(y ~ ., data=data_upd)
    res <- list("fit"=fit_upd)
  }
  else{
    fit_upd <- vinereg::vinereg(y ~ ., data=data_upd, order=names(data_upd)[-1], cores=cores_vine)
    pred_upd <- vinereg:::predict.vinereg(fit_upd, newdata = data_upd, alpha=alpha)
    resid <- resp - pred_upd[,1]
    res <- list("resid"=resid, "fit"=fit_upd)
  }
  res
}
