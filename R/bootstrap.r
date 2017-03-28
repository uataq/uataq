#' Bootstrap resampling
#'
#' \code{bootstrap} resamples the dataset with replacement and applies a
#'   function to each sample
#'
#' @param x data frame to be resampled
#' @param fun function that takes a single argument (a data frame of each
#'   resampled dataset) and returns a single value
#' @param size size of each sample
#' @param iter number of bootstrap iterations
#'
#' @import dplyr
#' @export
bootstrap <- function(x, fun, size = nrow(x), iter = 10) {
  sapply(1:iter, function(i) {
    y <- dplyr::sample_n(x, size, replace = T)
    fun(y)
  })
}
