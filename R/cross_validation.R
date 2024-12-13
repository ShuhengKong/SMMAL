#' Title
#'
#' @param K Integer. The number of folds for cross-validation.
#' @param num_labelled Integer.
#' @param data Integer.
#' @param num_x Integer.
#' @param num_S Integer.
#'
#' @return A list containing:
#' \describe{
#'   \item{Y}{}
#'   \item{A}{}
#'   \item{X}{}
#'   \item{S}{}
#'   \item{W}{}
#'   \item{foldid}{}
#' }
#' @export
#'
cross_validation <- function(N,num_labelled,K,data,num_x,num_S){
  Y=data[, 1, drop = FALSE]
  A=data[, 2, drop = FALSE]
  X=data[, 3:2+num_x, drop = FALSE]
  S=data[, (2+num_x):(1+num_x+num_s), drop = FALSE]
  W = rbind(X, S)
  foldid = c(rep_len(1:K, length.out = num_labelled)[sample(num_labelled)],
             rep_len(1:K, length.out = N-num_labelled)[sample(N-num_labelled)])

return(list(Y=Y,A=A, X=X,S=S,W=W,foldid=foldid))
}
