#' internal function
#' @noRd

ParCor <- function(M, given, j, k) {
  M_11 <- M[given, given]
  jk <- c(j, k)
  M_12 <- M[given, jk]
  M_21 <- M[jk, given]
  M_22 <- M[jk, jk]
  if (length(given) > 1) {
    tem <- solve(M_11, M_12)
    Om_212 <- M_21 %*% tem
    om_11 <- 1 - Om_212[1, 1]
    om_22 <- 1 - Om_212[2, 2]
    om_12 <- M[j, k] - Om_212[1, 2]
    out <- om_12/sqrt(om_11 * om_22)
  }
  if (length(given) == 1) {
    tem <- M_12/M_11
    Om_212 <- outer(M_21, tem)
    om_11 <- 1 - Om_212[1, 1]
    om_22 <- 1 - Om_212[2, 2]
    om_12 <- M[j, k] - Om_212[1, 2]
    out <- om_12/sqrt(om_11 * om_22)
  }
  if (length(given) == 0){
    out <- M[j, k]
  }
  out
}
