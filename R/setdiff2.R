#' Look up student ID's using IP's (via SalesForce)
#'
#' @param x data.frame or similiar
#' @param y data.frame or similiar
#'
#' @return None
#' @export
#' 
#' 
setdiff2 <- function(x,y){
  d0 <- setdiff(colnames(x), colnames(y))
  d1 <- setdiff(colnames(y), colnames(x))
  cat("Columns in X but not in Y: \n\n")
  cat(d0, "\n\n")
  cat("Columns in Y but not in X: \n\n")
  cat(d1)
}