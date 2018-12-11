#' Calculate GPS speed, distance, and direction
#'
#' \code{gps_move} calculates the speeds, distances, and directions given
#'   vectors of GPS coordinates
#'
#' @param Time POSIXct times for observations
#' @param lat latitude, in dd.dddd
#' @param lon longitude, in dd.dddd
#'
#' @return list containing (1) \code{$speed} at each point, (2)
#'   \code{$distance} from the previous point, and (3) \code{$direction}
#'   the direction of movement in degrees.
#'
#' @export
#' 
gps_move <- function(Time, lat, lon) {
  if (length(Time)!=length(lat) | length(Time)!=length(lon))
    stop('Error: GPS input of different lengths.')
  len <- length(Time)

  earth_radius <- 6.371 * 10^6 # m
  latrad <- lat * pi/180 # rad
  lonrad <- lon * pi/180 # rad

  dlat <- latrad[2:len] - latrad[1:(len-1)]
  dlon <- lonrad[2:len] - lonrad[1:(len-1)]
  dt   <- as.numeric(Time[2:len] - Time[1:(len-1)])

  # Direction calculations ----------------------------------------------------
  y <- sin(dlon) * cos(latrad[2:len])
  x <- cos(latrad[1:(len-1)]) * sin(latrad[2:len]) -
    sin(latrad[1:(len-1)]) * cos(latrad[2:len]) * cos(dlon)
  dir <- atan2(y, x) * 180/ pi
  dir <- (dir + 360) %% 360
  dir <- c(NA, dir)

  # Speed and distance calculations -------------------------------------------
  a <- sin(dlat / 2)^2 + sin(dlon / 2)^2 *
    cos(latrad[1:(len-1)]) * cos(latrad[2:len])
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d_rad <- c(NA, c)
  d_m   <- d_rad * earth_radius

  v <- d / c(NA, dt) # m/s
  return(list(speed     = v,
              distance  = list(rad = d_rad,
                               m   = d_m),
              direction = dir))
}
