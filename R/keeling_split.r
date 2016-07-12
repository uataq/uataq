#' Keeling isotope analysis
#'
#' \code{keeling} performs keeling isotope analysis, generating a linear fit
#'   using QR regression and a d13C keeling plot.
#'
#' @param time POSIXct timestamps of measurements
#' @param co2 numeric co2 concentrations, in ppm
#' @param d13c numeric d13C, in per mille
#' @param format character format compatible with strptime to split the data
#'   by. Defaults to '\%Y-\%m-01' which indicates monthly regression statistics
#'   with floored timestamps (e.g. 2016-01-01 indicates the statistics for the
#'   month of January, 2016)
#'
#' @return data_frame containing the slope, yintercept, fit statistics including
#'   \code{r_squared}, \code{rmse}, \code{std_error}, and \code{n}, and
#'   \code{lm_obj} the fit object created using the lm function for each time
#'   split
#'
#' @export
keeling_split <- function(time, co2, d13c, format = '%Y-%m-01', tz = 'UTC') {
  require(dplyr)

  df <- data_frame(time, co2, d13c, co2_inv = 1 / co2, d13c_inv = 1 / d13c)

  df %>%
    group_by(Time = time %>%
               strftime(format, tz = tz) %>%
               as.POSIXct(tz = tz)) %>%
    do({
      mod <- lm(.$d13c ~ .$co2_inv, method = 'qr')
      mod_summary <- summary(mod)
      data_frame(slope      = coef(mod)[2],
                 yint       = coef(mod)[1],
                 r_squared  = mod_summary$r.squared,
                 rmse       = resid(mod)^2 %>% mean(na.rm=T) %>% sqrt,
                 std_error  = coef(mod_summary)[3],
                 n          = nrow(.),
                 lm_obj     = list(mod))
    })
}
