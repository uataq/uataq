#' Linearly interpolate NA values
#'
#' \code{na_interp} linearly interpolates NA values found in vector y with
#'   respect to dimension x (e.g. timestamp).
#'
#' @param y numeric vector in which to fill bracketed NA values
#' @param x vector giving dimension of y. NULL if y is equally spaced
#'
#' @export
na_interp <- function(y, x=NULL) {
  if(is.null(x)) x <- 1:length(y)
  nona  <- which(!is.na(y))
  start <- head(nona, 1)
  end   <- tail(nona, 1)
  if(length(end - start) < 1) return(y)
  ysub  <- y[start:end]
  y[start:end] <- approx(x[start:end], ysub, n=length(ysub), method='linear')$y
  return(y)
}
