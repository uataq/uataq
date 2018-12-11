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
#' @import dplyr
#' 
#' @export

calibrate <- function(time, gasm, gask, auto=F, er_tol=0.1, dt_tol=18000)
{
  # Input error checking ------------------------------------------------------
  data <- data_frame(time, gasm=as.numeric(gasm), gask=as.numeric(gask)) %>%
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
                      rmse = NA,
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
  run <- data$gask %>%
    rle %>%
    unclass %>%
    as_data_frame

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
  stdk[is.na(stdm)] <- NA

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
  xysum <- rowSums(x * y, na.rm=T)
  x2sum <- rowSums(x^2, na.rm=T)
  y2sum <- rowSums(y^2, na.rm=T)

  m <- (n_cal * xysum - xsum * ysum) / (n_cal * x2sum - xsum * xsum)
  b <- (x2sum * ysum - xsum * xysum) / (n_cal * x2sum - xsum * xsum)
  r_squared <- (n_cal * xysum - xsum * ysum)^2 /
    ((n_cal * x2sum - xsum * xsum) * (n_cal * y2sum - ysum * ysum))
  fit_residuals <- y - (m * x + b)
  rmse <- sqrt(rowMeans(fit_residuals^2, na.rm = T))

  # Invalidate R-squared and RMSE without 3 references
  invalid <- n_cal < 3
  r_squared[invalid] <- NA
  rmse[invalid]      <- NA

  # For periods with no calibration gases, set slope and intercep to NA
  n0 <- n_cal == 0
  m[n0] <- NA
  b[n0] <- NA

  # For periods with only a single calibration gas, assume that the instrument
  # is perfectly linear through zero and make only a slope correction.
  n1 <- n_cal == 1
  if (ncol(x) < 2) {
    m[n1] <- y[n1,] / x[n1, ]
  } else {
    m[n1] <- rowSums(y[n1,], na.rm=T) / rowSums(x[n1, ], na.rm=T)
  }
  b[n1] <- 0


  out <- data_frame(time = data$time,
                    cal  = (data$gasm - b) / m,
                    raw  = data$gasm,
                    m    = m,
                    b    = b,
                    n    = n_cal,
                    r_sq = r_squared,
                    rmse = rmse,
                    flag = data$gask) %>%
    filter(flag == -10) %>%
    select(-flag)

  # Trim NA values which are not bracketed by two calibrations.
  # idx1 <- head(which(out$n > 0), 1)
  # idx2 <- tail(which(out$n > 0), 1)
  # out <- out[idx1:idx2, ]
  return(out)
}
