#' Generate bad data strings
#' 
#' \code{bad_data} takes the given information and formats a string compatible
#'   with UATAQ processing routines. Times should be in MST/MDT (as displayed
#'   on the UATAQ data viewer) and formatted as '2016-02-12 00:00:00.000'.
#'   
#' @param t_start character timestamp to begin changing data
#' @param t_end character timestamp to end changing data
#' @param comment comment about why the data is being removed, typically 
#'   formatted as 'Name: here is my comment'
#' @param miu_old character of the ID string to remove or 'all' to specify all 
#'   valve positions
#' @param miu_new character string to replace the old miu values with, or NA to
#'   remove the data
#' 
#' @export
bad_data <- function(t_start, t_end, comment, miu_old = 'all', miu_new = NA) {
  require(dplyr)
  
  t_start <- t_start %>%
    as.POSIXct(tz = 'America/Denver') %>%
    format(tz = 'UTC', format = '%Y-%m-%d %H:%M:%OS3')
  t_end   <- t_end %>%
    as.POSIXct(tz = 'America/Denver') %>%
    format(tz = 'UTC', format = '%Y-%m-%d %H:%M:%OS3')
  comment <- gsub(',', ';', comment, fixed=T)
  
  return(paste(sep=', ', t_start, t_end, miu_old, miu_new, comment))
}