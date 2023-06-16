#' internal function
#' @noRd

vineregParCor <- function(df, cores_vine){
  var_indx <- 1:(ncol(df)-1)
  cond <- TRUE
  single_vars <- vector()
  count <- 1
  prev_mdl <- list()
  total_vars <- length(var_indx)
  thr_caic <- 1000000
  u_vars <- matrix(0, nrow(df), ncol(df))
  for(j in 1:ncol(df)){
    u_vars[,j] <- u_scale(df[,j])
  }
  z_vars <- norm_score(u_vars)
  cor_mat <- stats::cor(z_vars)
  while(cond==TRUE){
    thr_single <- -2
    df_cor <- vector()
    df_cor_test <- vector()
    for(i in 1:length(var_indx)){
      mdl_single <- abs(ParCor(cor_mat, single_vars+1, 1,  (1+var_indx[i])))
      if(mdl_single > thr_single){
        thr_single <- mdl_single
        var_single <- var_indx[i]
      }
    }
    single_vars <- union(single_vars, var_single)
    var_indx <- setdiff(var_indx, var_single)
    cand_vars <- df[,(1+single_vars)]
    name_cnd <- colnames(df)[(1+single_vars)]
    mdl <- update_mdl(df[,1], cand_vars, name_cnd, cores_vine)
    prev_mdl[[count]] <- mdl
    mdl_caic <- mdl$fit$stats$caic
    if(mdl_caic >= thr_caic){
      single_vars <- single_vars[-length(single_vars)]
      final_mdl <- prev_mdl[[count-1]]$fit
      cond <- FALSE
      break
    }
    if(length(single_vars) == total_vars){
      final_mdl <- prev_mdl[[count]]$fit
      cond <- FALSE
      break
    }
    thr_caic <- mdl_caic
    count <- count + 1
  }
  res_list <- list("method"="ParCor", "vinegraph"="Dvine","vars_indx"=single_vars,
                   "vinereg_fit"=final_mdl)
}






