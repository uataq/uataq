#' Running mean smoothing
#'
#' \code{run_smooth} generates smoothed representation of x
#'
#' @param x numeric values
#' @param n number of points to smooth by
#'
#' @export

run_smooth <- function(x, n=10) {
  n <- trunc(n)
  y <- as.numeric(stats::filter(x, rep(1 / n, n), sides=2))
  return(y)
}
