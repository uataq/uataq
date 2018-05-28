#' Initialize new STILT project
#' @author Ben Fasoli
#'
#' \code{stilt_init} generates the framework for a new STILT modeling project
#'
#' @param project name or location to initialize new project
#' @param branch name of repo branch to checkout
#' @param repo repo to fetch stilt from using \code{git clone repo}
#'
#' @import whisker
#' @export

stilt_init <- function(project, branch = 'master',
                       repo = 'https://github.com/uataq/stilt') {

  # Extract project name and working directory
  project <- basename(project)
  wd <- dirname(project)
  if (wd == '.')
    wd <- getwd()

  # Clone git repository
  system(paste('git clone -b', branch, '--single-branch', repo, project))

  # Run setup executable
  setwd(file.path(wd, project))
  system('chmod +x setup')
  system('./setup')

  # Render run_stilt.r template with project name
  run_stilt <- readLines('r/run_stilt.r')
  run_stilt <- whisker::whisker.render(run_stilt)
  writeLines(run_stilt, 'r/run_stilt.r')
}
