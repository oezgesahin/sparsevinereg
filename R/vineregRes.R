#' internal function
#' @noRd

vineregRes <- function(df, alpha, cores_vine, cores_varsel){
  response <- df[,1]
  var_indx <- 1:(ncol(df)-1)
  cond <- TRUE
  single_vars <- vector()
  count <- 1
  prev_mdl <- list()
  total_vars <- length(var_indx)
  thr_caic <- 1000000
  while(cond==TRUE){
    thr_single <- -1000000
    mdl_single <- parallel::mclapply(1:length(var_indx), function(k)  est_resid(response, df[,(1+var_indx[k])], colnames(df)[(1+var_indx[k])]),
                                     mc.cores=cores_varsel)
    for(i in 1:length(mdl_single)){
      cand_cll <- mdl_single[[i]]$fit$stats$cll
      if(cand_cll > thr_single){
        thr_single <- cand_cll
        var_single <- var_indx[i]
      }
    }
    single_vars <- union(single_vars, var_single)
    var_indx <- setdiff(var_indx, var_single)
    cand_vars <- df[,(1+single_vars)]
    name_cnd <- colnames(df)[(1+single_vars)]
    mdl <- est_resid(df[,1], cand_vars, name_cnd, alpha, 1, cores_vine)
    prev_mdl[[count]] <- mdl
    mdl_caic <- mdl$fit$stats$caic
    response <- mdl$resid
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
  res_list <- list("method"="resid", "vinegraph"="Dvine","vars_indx"=single_vars,
                   "vinereg_fit"=final_mdl)
}
