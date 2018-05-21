#' Update uataq package
#'
#' \code{update_uataq} fetches the most recent version of the uataq R package
#'   from github
#'   
#' @importFrom devtools install_github
#' @export
update_uataq <- function() {
  devtools::install_github('benfasoli/uataq')
}
