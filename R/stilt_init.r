#' Initialize new STILT project
#' @author Ben Fasoli
#'
#' \code{stilt_init} generates the framework for a new STILT modeling project
#'
#' @param project name or location to initialize new project
#' @param repo repo to fetch stilt from using \code{git clone repo}
#'
#' @import whisker
#' @export

stilt_init <- function(project, repo = 'https://github.com/benfasoli/stilt') {

  project <- basename(project)
  wd <- dirname(project)
  if (wd == '.') wd <- getwd()

  system(paste('git clone', repo))
  system(paste('mv stilt', project))
  setwd(project)
  system('chmod +x setup')
  system('./setup')

  run_stilt <- readLines('r/run_stilt.r')
  # name_idx <- grepl('project <-', run_stilt, fixed = T)
  # run_stilt[name_idx] <- paste0('project <- \'', project, '\'')
  run_stilt <- whisker::whisker.render(run_stilt)
  writeLines(run_stilt, 'r/run_stilt.r')
}
