#' Send newline characters to the R console
#'
#' @param n number of newline characters to print; negative n will clear the
#'   console with uataq::clc()
#' @export
br <- function(n = 1) {
  if (n < 0)
    uataq::clc()
  else 
    cat(rep('\n', n))
}
