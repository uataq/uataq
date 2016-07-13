
Calibration Documentation
=========================

This document outlines the workflow used in the UATAQ trace gas calibration algorithm written in R and is included with other data manipulation and trace gas analysis tools found in the [uataq R package](https://github.com/benfasoli/uataq). The raw source code can be found [here](https://github.com/benfasoli/uataq/blob/master/R/calibrate.r).

Typically, UATAQ sites are calibrated every two hours using three reference gases. The measurement of the reference gas is linearly interpolated during bracketed periods when sampling from other sources. For managing tolerances, see the Optional Inputs section.

Example
=======

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

Data from the Logan, UT CO2 site was calibrated during January, 2015. The sample below shows the raw measurements for the atmosphere (red), the three standard gases interpolated over time (yellow, green, blue), and the corrected concentrations (purple). The dotted lines represent the known values of each standard. It is evident that the instrument was reading ~13ppm higher than the known values. The corrected concentrations are produced by generating a linear slope and intercept from the positions of the interpolated standards (yellow, green, blue lines) for each atmospheric data point.

Required inputs
---------------

To calibrate atmospheric observations, three vectors of matching length are required:

1.  gasm - measured concentrations (numeric). These are the concentrations that the instrument reports over time.

2.  gask - known concentration flag (numeric). This informs the algorithm *what* is being measured at any point of time. We define atmospheric observations with a numeric value of -10, periods during which the optical cavity is being flushed with -99, and periods when sampling reference values with the known concentration.

3.  time - timestamps (POSIXct). To be preserved in the calibrated dataset.

### Example input

                       time  gasm  gask
                     (time) (dbl) (dbl)
     1  2016-01-12 23:00:00   400   -10
     2  2016-01-12 23:00:01   402   -10
     3  2016-01-12 23:00:02   435   -99
     4  2016-01-12 23:00:03   473   -99
     5  2016-01-12 23:00:04   499   500
     6  2016-01-12 23:00:05   501   500
     7  2016-01-12 23:00:06   468   -99
     8  2016-01-12 23:00:07   422   -99
     9  2016-01-12 23:00:08   405   -10
     10 2016-01-12 23:00:09   404   -10

Optional inputs
---------------

In addition to these inputs, the tolerance for percent deviation from the known reference value *er\_tol* and for the time elapsed between calibrations *dt\_tol* can optionally be defined. Measured values that do not meet these criteria will be removed from the calibrated dataset.

Methods
=======

Run length encoding is used to group the data by each valve change identified by `gask`. The measured concentrations `gasm` are then averaged by these periods to produce a single value for each gas for each calibration sequence.

``` r
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
```

Matrix differencing
-------------------

Matrices are then propagated for the measured values `stdm` and known references `stdk`, each of which has dimensions equivalent to the number of observations (rows) and the number of unique reference gases used (columns). `stdm` is filled with measured reference values in the columns associated with the standard tank being measured, while `stdk` contains constant known values. These two matrices are then filled with measured and known concentrations; differencing the two allows the

``` r
# Establish the values and number of unique reference gases.
std_uniq <- unique(subset(data, gask != -10)$gask)
n_std <- length(std_uniq)
n_obs <- nrow(data)
std_flag <- matrix(data$gask, nrow=n_obs, ncol=n_std)
# Mask for times when sampling any of the unique reference gases. Then, set
# atmospheric sampling periods to NA. stdm then becomes a matrix with columns
# of known concentrations, NA for atmosphere, and numeric values representing
# the measured concentrations of the reference gas.
stdm <- matrix(data$gasm, nrow=n_obs, ncol=n_std)
stdm[stdk != std_flag | is.na(std_flag)] <- NA
stdm[!is.na(stdm)] <- rep.int(data$per_mean, n_std)[!is.na(stdm)]
# Linearly interpolate each gas in stdm over atmospheric sampling periods
# when the standard is not being measured.
stdm <- apply(stdm, 2, uataq::na_interp, x=data$time)
stdk[is.na(stdm)] <- NA
```

Interpolation of references
---------------------------

Each standard is linearly interpolated to represent the drift in the averaged measuremed standards over time. Source code for the implementation of the linear interpolation can be found [here](https://github.com/benfasoli/uataq/blob/master/R/data-manipulation.R#L246-L264).

``` r
# Linearly interpolate each gas in stdm over atmospheric sampling periods
# when the standard is not being measured.
stdm <- apply(stdm, 2, uataq::na_interp, x=data$time)
stdk[is.na(stdm)] <- NA
```

OLS Regression
--------------

Ordinary least squrared regression is then applied to the interpolated reference values during the atmospheric sampling periods to generate a linear slope and intercept. These are then used to correct the uncalibrated value and are returned along with the calibrated data, raw data, timestamp, and fit statistics.

``` r
# Identify the number of references used to calibrate each observation and
# perform ordinary least squares regression to generate a linear
# representation of the instrument drift. Then, apply the generated slope and
# intercept to the atmospheric observations to correct.
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

# For periods with only a single calibration gas, assume that the instrument
# is perfectly linear through zero and make only a slope correction.
n1 <- n_cal == 1
m[n1] <- x[n1, ] / y[n1,]
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
```
