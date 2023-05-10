#' internal function
#' @noRd

update_mdl <- function(resp, cov, name_cnd=NA, cores_vine){
  data_upd <- data.frame(y = resp, cov)
  colnames(data_upd)[2:ncol(data_upd)] <- name_cnd
  fit_upd <- vinereg::vinereg(y ~ ., data=data_upd, order=names(data_upd)[-1], cores=cores_vine)
  res <- list("fit"=fit_upd)
  res
}
