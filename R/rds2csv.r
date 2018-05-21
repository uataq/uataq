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
#' @importFrom readr write_delim
#' @export
rds2csv <- function(rds, file, sep=',', ...) {
  rd <- readRDS(rds)
  if (!is.data.frame(rd)) 
    stop(paste(rds, 'is not a data.frame.'))
  readr::write_delim(rd, file, delim=sep, ...)
}
