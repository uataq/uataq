#' Keeling isotope analysis
#'
#' \code{keeling} performs keeling isotope analysis, generating a linear fit
#'   using QR regression and a d13C keeling plot.
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
keeling_plot <- function(co2, d13c, bg_co2=400, bg_d13c=-8) {
  require(dplyr)
  require(ggplot2)

  bg_co2_inv  <- 1 / bg_co2
  bg_d13c_inv <- 1 / bg_d13c

  df <- data_frame(co2,
                   d13c,
                   co2_inv = 1 / co2,
                   d13c_inv = 1 / d13c)

  fit <- with(df, lm(d13c ~ co2_inv, method = 'qr'))

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
