#' Find nearest neighbors
#'
#' \code{findNeighbor} finds the closest value index in y for each x, using findInterval
#'
#' @param x desired values
#' @param y table to search values
findNeighbor <- function(x,y){
  x <- as.numeric(x)
  y <- as.numeric(y)
  small<-findInterval(x,y,all.inside=TRUE)
  large<-small+1
  is.larger<- 2*x > y[small] + y[large]
  return(small+is.larger)
}