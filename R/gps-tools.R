#' Calculate GPS direction
#'
#' \code{gps_dir} calculates the direction between GPS coordinates
#'
#' @param lat latitude, in dd.dddd
#' @param lon longitude, in dd.dddd
gps_dir <- function(lat, lon) {
  # 0 degrees = north, increasing to the east.
  if (length(lon)!=length(lat)) stop('Error: GPS input of different lengths.')
  len <- length(lat)
  
  earth.radius <- 6.371 * 10^6 # m
  
  # Convert degrees to radians
  latrad <- lat * pi/180 # rad
  lonrad <- lon * pi/180 # rad
  
  dlat <- latrad[2:len] - latrad[1:(len-1)]
  dlon <- lonrad[2:len] - lonrad[1:(len-1)]
  
  y <- sin(dlon) * cos(latrad[2:len])
  x <- cos(latrad[1:(len-1)]) * sin(latrad[2:len]) - 
    sin(latrad[1:(len-1)]) * cos(latrad[2:len]) * cos(dlon)
  
  dir <- atan2(y, x) * 180/ pi
  dir <- (dir + 360) %% 360
  dir <- c(NA, dir)
  
  return(dir)
}

#' Calculate GPS distances
#'
#' \code{gps_distance} calculates the distances between GPS coordinates
#'
#' @param lat latitude, in dd.dddd
#' @param lon longitude, in dd.dddd
#' @param units output units, either 'm' or 'rad'
gps_distance  <- function(lat, lon, units='m') {
  if (length(lon)!=length(lat)) stop('Error: GPS input of different lengths.')
  len <- length(lat)
  
  earth.radius <- 6.371 * 10^6 # m
  
  # Convert degrees to radians
  latrad <- lat * pi/180 # rad
  lonrad <- lon * pi/180 # rad
  
  dlat <- latrad[2:len] - latrad[1:(len-1)]
  dlon <- lonrad[2:len] - lonrad[1:(len-1)]
  
  a <- sin(dlat / 2)^2 + sin(dlon / 2)^2 * cos(latrad[1:(len-1)]) * cos(latrad[2:len])
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- c(NA, c)
  
  if (units=='m') return(d * earth.radius)
  else if (units=='rad') return (d)
}

#' Calculate GPS speed
#'
#' \code{gps_speed} calculates the speed in m/s between GPS coordinates
#'
#' @param Time POSIXct times for observations
#' @param lat latitude, in dd.dddd
#' @param lon longitude, in dd.dddd
gps_speed <- function(Time, lat, lon) {
  if (length(Time)!=length(lat) | length(Time)!=length(lon)) stop('Error: GPS input of different lengths.')
  len <- length(Time)
  
  earth.radius <- 6.371 * 10^6 # m
  
  # Convert degrees to radians
  latrad <- lat * pi/180 # rad
  lonrad <- lon * pi/180 # rad
  
  dlat <- latrad[2:len] - latrad[1:(len-1)]
  dlon <- lonrad[2:len] - lonrad[1:(len-1)]
  dt   <- as.numeric(Time[2:len] - Time[1:(len-1)])
  
  a <- sin(dlat / 2)^2 + sin(dlon / 2)^2 * cos(latrad[1:(len-1)]) * cos(latrad[2:len])
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- c(NA, earth.radius * c)
  
  v <- d / c(NA, dt) # m/s
  return(v)
}