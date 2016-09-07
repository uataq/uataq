#' Initialize new STILT project
#' @author Ben Fasoli
#'
#' \code{stilt_init} generates the framework for a new STILT modeling project
#'
#' @param name name or location to initialize new project
#' @param repo 
#' @export

stilt_init <- function(name, repo = 'https://github.com/benfasoli/stilt') {
  repo_options <- c('https://github.com/benfasoli/stilt')
  
  if (!repo %in% repo_options) {
    stop('Invalid repo argument. Try https://github.com/benfasoli/stilt')
  }
  
  system(paste('git clone', repo))
  system(paste('mv stilt', name))
  setwd(name)
  system('chmod +x setup')
  system('./setup')
}