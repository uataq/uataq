#' Clear R console
#'
#' \code{clc} sends ctrl+L to clear the R console.
#' @export
clc <- function() {cat("\014")}
