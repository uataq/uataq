#' Generates hex colors for plotting
#'
#' \code{colorize} uses colorRampPalette() to generate custom color scales for
#' plotting.
#'
#' @param data data to be converted to color scale
#' @param clim values in data to limit color scale, as c(min, max)
#' @param ncol number of colors to use between cmin and cmax
#' @param cols colors to interpolate between
#' @param ...  arguments to pass to colorRampPalette
#' 
#' @export
colorize <- function(data, clim = NULL, ncol = 64,
                     cols = c('blue', 'cyan', 'green', 'yellow', 'orange',
                              'red'), ...) {
  if (is.null(clim)) {
    clim <- c(min(data, na.rm=T), max(data, na.rm=T))
  }
  cmin <- clim[1]
  cmax <- clim[2]

  cpalet <- grDevices::colorRampPalette(cols, ...)

  data[data < cmin] <- cmin
  data[data > cmax] <- cmax
  idx <- ceiling((ncol - 1) * (data - cmin) / (cmax - cmin)) + 1

  chex <- cpalet(ncol) # in RRGGBB
  return(chex[idx])
}
