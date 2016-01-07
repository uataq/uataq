#' RDS file conversion
#'
#' \code{rds2csv} converts .rds files to .csv representations.
#'
#' @param rds path to .rds file.
#' @param file either a character string naming a file or a connection open for writing. '' indicates output to the console.
#' @param sep the field separator string.
#' @param quote a logical value (TRUE or FALSE) or a numeric vector. If TRUE, any character or factor columns will be surrounded by double quotes. If a numeric vector, its elements are taken as the indices of columns to quote. In both cases, row and column names are quoted if they are written. If FALSE, nothing is quoted.
#' @param ... arguments passed to write.table
rds2csv <- function(rds, file, sep=',', quote=F, ...) {
  rd <- readRDS(rds)
  write.table(rd, file=file, sep=sep, ...)
}