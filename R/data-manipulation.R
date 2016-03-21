#' Split delimited strings
#' 
#' \code{breakstr} splits strings by the given delimiter and returns a
#'   data_frame. Lines that do not fit the correct number of columns
#'   are omitted.
#'   
#' @param strings character array of strings to be split
#' @param pattern delimiter at which to split strings
#' @param ncol    number of columns. NULL will determine by the median number
#'                  of columns found in strings
#' 
#' @export
breakstr <- function(strings, pattern=',', ncol=NULL) {
  require(dplyr)
  require(stringr)
  ndelim <- stringr::str_count(strings, pattern)
  if(is.null(ncol)) ncol <- median(ndelim, na.rm=T) + 1
  split <- subset(strings, ndelim == (ncol-1)) %>%
    stringr::str_split_fixed(pattern, n=ncol) %>%
    as.data.frame(stringsAsFactors=F) %>%
    as_data_frame()
  return(split)
}

#' Calibrate trace gas data
#' 
#' \code{calibrate} atmospheric trace gas data against several reference gas 
#'   tanks.
#'   
#' @param time POSIXct class timestamp, length n
#' @param gasm numeric measured values, length n
#' @param gask numeric gas concentration flag, length n. -10 for atmospheric
#'               observations, -99 for flush periods, or a positive value if
#'               sampling a known concentration
#' @param auto if TRUE, stops calibration if the dataset ends with reference gas
#'               flow still active
#' @param er_tol tolerated deviation from known value. e.g. 0.1 for 10 percent
#'               deviation from known
#' @param dt_tol tolerated amount of time between observations, in seconds. If 
#'               exceeded, will remove all atmospheric data until the next 
#'               calibration
#'                  
#' @export
calibrate <- function(time, gasm, gask, auto=F, er_tol=0.1, dt_tol=18000)
{
  require(dplyr)
  
  # Input error checking ------------------------------------------------------
  data <- data_frame(time, gasm, gask) %>%
    arrange(time) %>%
    filter(!(gask %in% c(-1, -2, -3, -99, 0))) %>%
    mutate(dt = c(NA, time[2:n()] - time[1:(n()-1)]))
  
  if (auto == T && tail(gask, 1) != -10) {
    stop('Instrument currently flowing reference. Not calibrating this run.')
  }
  
  # Populate known and measured data matrices ---------------------------------
  std_uniq <- unique(subset(data, gask != -10)$gask)
  if (length(std_uniq) < 1) {
    out <- data_frame(time = data$time,
                      cal  = NA,
                      raw  = data$gasm,
                      m    = NA,
                      b    = NA,
                      n    = 0,
                      r_sq = NA,
                      flag = data$gask) %>%
      filter(flag == -10) %>%
      select(-flag)
    return(out)
  }
  
  n_std <- length(std_uniq)
  n_obs <- nrow(data)
  
  stdk     <- matrix(std_uniq, nrow=n_obs, ncol=n_std, byrow=T)
  std_flag <- matrix(data$gask, nrow=n_obs, ncol=n_std)
  
  # Mask for times when sampling any of the unique reference gases. Then, set
  # atmospheric sampling periods to NA. stdm then becomes a matrix with columns
  # of known concentrations, NA for atmosphere, and numeric values representing
  # the measured concentrations of the reference gas.
  stdm <- matrix(data$gasm, nrow=n_obs, ncol=n_std)
  stdm[stdk != std_flag | is.na(std_flag)] <- NA
  stdm[abs(stdm - stdk) / stdk > er_tol] <- NA
  
  # Run length encoding -------------------------------------------------------
  # Apply run length encoding to slice data by each unique valve change and add
  # an identifier for each period. Reconstruct the original values with the
  # additional index column and perform averaging on each period (per_mean).
  run <- as_data_frame(rle(data$gask))
  
  data$idx <- run %>%
    mutate(values = 1:n()) %>%
    inverse.rle()
  
  data_grp <- data %>%
    group_by(idx)
  
  data$per_mean <- data_grp %>%
    summarize(values = mean(gasm, na.rm=T)) %>%
    bind_cols(data_frame(lengths = run$lengths)) %>%
    inverse.rle()
  
  data$per_dtmax <- data_grp %>%
    summarize(values = max(dt, na.rm=T)) %>%
    bind_cols(data_frame(lengths = run$lengths)) %>%
    inverse.rle()
  
  stdm[!is.na(stdm)] <- rep.int(data$per_mean, n_std)[!is.na(stdm)]
  
  # Remove periods with longer than dt_tol seconds between calibrations.
  data$gasm[data$per_dtmax > dt_tol] <- NA
  
  # Linear interpolation of measured reference --------------------------------
  # Linearly interpolate each gas in stdm over atmospheric sampling periods
  # when the standard is not being measured.
  stdm <- apply(stdm, 2, uataq::na_interp, x=data$time)
  
  # Generate calibration coefficients -----------------------------------------
  # Identify the number of references used to calibrate each observation and
  # perform ordinary least squares regression to generate a linear
  # representation of the instrument drift. Then, apply the generated slope and
  # intercept to the atmospheric observations to correct.
  n_cal <- rowSums(!is.na(stdm))
  x     <- stdk
  y     <- stdm
  
  xsum  <- rowSums(x, na.rm=T)
  ysum  <- rowSums(y, na.rm=T)
  xysum <- rowSums(x * y)
  x2sum <- rowSums(x^2)
  y2sum <- rowSums(y^2)
  
  m <- (n_cal * xysum - xsum * ysum) / (n_cal * x2sum - xsum * xsum)
  b <- (x2sum * ysum - xsum * xysum) / (n_cal * x2sum - xsum * xsum)
  r_squared <- (n_cal * xysum - xsum * ysum)^2 /
    ((n_cal * x2sum - xsum * xsum) * (n_cal * y2sum - ysum * ysum))
  
  # For periods with only a single calibration gas, assume that the instrument
  # is perfectly linear through zero and make only a slope correction.
  n1 <- n_cal == 1
  m[n1] <- rowSums(x[n1, ], na.rm=T) / rowSums(y[n1,], na.rm=T)
  b[n1] <- 0
  
  out <- data_frame(time = data$time,
                    cal  = (data$gasm - b) / m,
                    raw  = data$gasm,
                    m    = m,
                    b    = b,
                    n    = n_cal,
                    r_sq = r_squared,
                    flag = data$gask) %>%
    filter(flag == -10) %>%
    select(-flag)
  
  # Trim NA values which are not bracketed by two calibrations.
  # idx1 <- head(which(out$n > 0), 1)
  # idx2 <- tail(which(out$n > 0), 1)
  # out <- out[idx1:idx2, ]
  return(out)
}

#' Keeling isotope analysis
#' 
#' \code{keeling} performs keeling isotope analysis, generating a linear fit
#'   and d13C keeling plot.
#'
#' @param co2 numeric co2 concentrations, in ppm
#' @param d13c numeric d13C, in per mille
#' @param bg_co2 numeric background co2 concentration, in ppm
#' @param bg_d13c numeric background d13C, in per mille
#' 
#' @return list containing (1) \code{$fit} the linear fit object and (2)
#'   \code{$fig} the ggplot figure.
#' 
#' @export
keeling <- function(co2, d13c, bg_co2=400, bg_d13c=-8)
{
  require(dplyr)
  require(ggplot2)
  
  bg_co2_inv  <- 1 / bg_co2
  bg_d13c_inv <- 1 / bg_d13c
  
  df <- data_frame(co2, 
                   d13c, 
                   co2_inv = 1 / co2, 
                   d13c_inv = 1 / d13c)
  
  fit <- with(df, lm(d13c ~ co2_inv))
  
  lab <- list(x=min(df$co2_inv) + 0.25 * (max(df$co2_inv) - min(df$co2_inv)),
              y=max(df$d13c) - 0.2 * (max(df$d13c) - min(df$d13c)),
              lab=paste(collapse='',
                        'atop(delta^13 ~ "C = " ~',
                        signif(coef(fit)[2], 5),
                        '~ frac(1, CO[2]) ~',
                        signif(coef(fit)[1], 5),
                        ', R^2 ~ "=" ~ ',
                        signif(summary(fit)$r.squared, 4), ')'))
  fig <- ggplot(df, aes(x = co2_inv, y = d13c)) + 
    geom_point() +
    geom_point(x=bg_co2_inv, y=bg_d13c, color='orange', size=3, shape=3) +
    geom_abline(slope=coef(fit)[2], 
                intercept=coef(fit)[1], 
                color='darkorange') +
    xlab(expression(frac(1, CO[2]) ~ bgroup('[', frac(1, ppm), ']'))) +
    ylab(expression(delta^13 ~ 'C [\u2030]')) +
    annotate('text', x=lab$x, y=lab$y, label=lab$lab, parse=T) +
    theme_classic()
  print(fig)
  return(list(fig=fig, fit=fit))
}

#' Find nearest neighbors
#' 
#' \code{find_neighbor} finds the closest value index in y for each x, 
#'   using findInterval. Returns the index to be applied to y that most
#'   closely matches x.
#'
#' @param x desired values
#' @param y table to search values
#'                  
#' @export
find_neighbor <- function(x, y){
  x <- as.numeric(x)
  y <- as.numeric(y)
  small <- findInterval(x, y, all.inside=TRUE)
  large <- small + 1
  is.larger <- 2*x > y[small] + y[large]
  return(small + is.larger)
}

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

#' Running mean smoothing
#' 
#' \code{run_smooth} generates smoothed representation of x
#'
#' @param x numeric values
#' @param n number of points to smooth by
#'                  
#' @export
run_smooth <- function(x, n=10) {
  return(stats::filter(x, rep(1 / n, n), sides=2))
}
