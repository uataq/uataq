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

breakstr <- function(strings, pattern = ',', ncol = NULL) {
  ndelim <- stringr::str_count(strings, pattern)
  if (is.null(ncol))
    ncol <- stats::median(ndelim, na.rm = T) + 1
  subset(strings, ndelim == (ncol-1)) %>%
    stringr::str_split_fixed(pattern, n = ncol) %>%
    as.data.frame(stringsAsFactors = F) %>%
    dplyr::mutate_all(funs(utils::type.convert(., as.is = T)))
}
