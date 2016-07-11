#' Convert dddmm.mmm to dd.dddd coordinates
#'
#' \code{gps_dm2dd} converts the raw GPS $GPGGA output in the form of
#'   dddmm.mmm to map compatible degree form dd.dddd.
#'
#' @param x numeric coordinate, either latitude or longitude, to convert
#'
#' @export
gps_dm2dd <- function(x) {
  x <- as.numeric(x)
  dd <- floor(x/100)
  return(dd + (x - dd*100)/60)
}
