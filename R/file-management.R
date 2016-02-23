#' RDS file conversion
#' 
#' \code{rds2csv} converts .rds files to ASCII tabular representation
#'
#' @param rds path to .rds file.
#' @param file either a character string naming a file or a connection open
#'   for writing. '' indicates output to the console.
#' @param sep the field separator string.
#' @param ... arguments passed to readr::write_delim
#'                  
#' @export
rds2csv <- function(rds, file, sep=',', ...) {
  if(!require(readr))
    stop("Package 'readr' required. Run:  install.packages('readr')")
  rd <- readRDS(rds)
  readr::write_delim(rd, file, delim=sep, ...)
}