#' Slice data_frame into files
#'
#' \code{archive} splits data_frame by time specified in the \code{path}
#'   argument and saves to files.
#'
#' @param df data_frame to save. Must contain a column named 'time'
#' @param tz timezone of POSIXct timestamp in df
#' @param path file naming scheme used to split and save data. Format must be
#'   compatible with \code{strptime}
#' @param col_names whether to write header line with column names, NULL applies
#'   column names to new files only, FALSE doesn't write a header, and TRUE
#'   will write a line with the column names even if appending. Be careful with
#'   TRUE, as you may end up with column flags mid-file.
#'
#' @import dplyr readr
#' @importFrom stats setNames
#' @export
archive <- function(df, tz='UTC', path='%Y_%m.dat', col_names=NULL)
{
  if (nrow(df) < 1) stop('No data to append.')
  if (!dir.exists(dirname(path)))
    dir.create(dirname(path), recursive=T, mode='0755')

  time_col <- grep('time', names(df), ignore.case=T, value=T)[1]
  write_col_names <- col_names

  grp <- df %>%
    rename_(.dots=setNames(time_col, 'Time_temp')) %>%
    # arrange(Time_temp) %>%
    filter(!is.na(Time_temp)) %>%
    group_by(fnm = format(Time_temp, tz=tz, format=path)) %>%
    do(df_list = data.frame(.) %>%
         select(-fnm))

  fnm <- grp$fnm
  df_list <- grp$df_list

  for (i in 1:length(fnm)) {
    if (file.exists(fnm[[i]])) {
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
      append <- T
    } else {
      df_list[[i]] <- df_list[[i]] %>%
        mutate(Time_temp = format(Time_temp, tz=tz,
                                  format='%Y-%m-%d %H:%M:%OS2')) %>%
        rename_(.dots=setNames('Time_temp', paste0('Time_', tz)))
      append <- F
    }
    if (nrow(df_list[[i]]) < 1) next
    if (is.null(col_names)) write_col_names <- !append
    readr::write_csv(df_list[[i]], fnm[[i]],
                     append=append, col_names=write_col_names)
  }
}
