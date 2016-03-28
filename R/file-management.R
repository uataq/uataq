#' Slice data_frame into files
#' 
#' \code{archive} splits data_frame by time specified in the \code{path}
#'   argument and saves to files.
#'   
#' @param df data_frame to save. Must contain a column named 'time'
#' @param tz timezone of POSIXct timestamp in df
#' @param path file naming scheme used to split and save data. Format must be
#'   compatible with \code{strptime}
#' 
#' @export
archive <- function(df, tz='UTC', path='%Y_%m.dat')
{
  if (nrow(df) < 1) stop('No data to append.')
  if (!dir.exists(dirname(path)))
    dir.create(dirname(path), recursive=T, mode='0755')
  require(dplyr)
  require(readr)
  
  time_col <- grep('time', names(df), ignore.case=T, value=T)[1]
  
  grp <- df %>%
    rename_(.dots=setNames(time_col, 'Time_temp')) %>%
    arrange(Time_temp) %>%
    filter(!is.na(Time_temp)) %>%
    group_by(fnm = format(Time_temp, tz=tz, format=path)) %>%
    do(df_list = data.frame(.) %>% 
         select(-fnm))
  
  fnm <- grp$fnm
  df_list <- grp$df_list
  
  for (i in 1:length(fnm)) {
    if(file.exists(fnm[[i]])){
      t_start <- system(paste0('tail -n 1 ', fnm[[i]]), intern=T) %>%
        uataq::breakstr() %>%
        select(1) %>%
        as.character() %>%
        as.POSIXct(tz='UTC', format='%Y-%m-%d %H:%M:%S')
      
      df_list[[i]] <- df_list[[i]] %>%
        filter(Time_temp > t_start) %>%
        mutate(Time_temp = format(Time_temp, tz=tz, 
                                  format='%Y-%m-%d %H:%M:%OS2')) %>%
        rename_(.dots=setNames('Time_temp', paste0('Time_', tz)))
      
      readr::write_csv(df_list[[i]], fnm[[i]], append=T)
    } else {
      df_list[[i]] <- df_list[[i]] %>%
        mutate(Time_temp = format(Time_temp, tz=tz, 
                                  format='%Y-%m-%d %H:%M:%OS2')) %>%
        rename_(.dots=setNames('Time_temp', paste0('Time_', tz)))
      
      readr::write_csv(df_list[[i]], fnm[[i]], append=F)
    }
  }
}


#' RDS file conversion
#' 
#' \code{rds2csv} converts .rds files to ASCII tabular representation
#'
#' @param rds path to .rds file.
#' @param file either a character string naming a file or a connection open
#'   for writing. '' indicates output to the console.
#' @param sep the field separator string.
#' @param ... arguments passed to readr::write_delim
#'                  
#' @export
rds2csv <- function(rds, file, sep=',', ...) {
  require(readr)
  rd <- readRDS(rds)
  readr::write_delim(rd, file, delim=sep, ...)
}