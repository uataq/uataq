#' Generate bad data strings
#'
#' \code{bad_data_addin} takes the given information and formats a string compatible
#'   with UATAQ processing routines. Times should be in MST/MDT (as displayed
#'   on the UATAQ data viewer) and formatted as '2016-02-12 00:00:00.000'.
#'   Deployed as an RStudio Addin.
#'
#' @param t_start character timestamp to begin changing data
#' @param t_end character timestamp to end changing data
#' @param comment comment about why the data is being removed, typically
#'   formatted as 'Name: here is my comment'
#' @param miu_old character of the ID string to remove or 'all' to specify all
#'   valve positions
#' @param miu_new character string to replace the old miu values with, or NA to
#'   remove the data
bad_data_addin <- function() {
  require(dplyr)
  require(miniUI)
  require(shiny)

  # Generate Addin UI ----------------------------------------------------------
  ui <- miniPage(
    miniContentPanel(
      scrollable=F, padding=10,
      HTML('This tool takes the given information and formats a string',
           'compatible with UATAQ processing routines. Times should be in',
           'MST or MDT as displayed on the <a href="http://air.utah.edu/s/gasview/"',
           'target="_blank">UATAQ gasview data display</a>.'),
      fillRow(height=80,
              textInput('t_start', 'Start time', '2016-02-12 00:00:00.000',
                        placeholder='2016-02-12 00:00:00.000'),
              textInput('t_end', 'End time', '2016-02-12 00:00:00.000',
                        placeholder='2016-02-12 00:00:00.000')
      ),
      fillRow(height=80,
              textInput('miu_old', 'Old ID', 'all', placeholder='all'),
              textInput('miu_new', 'New ID', 'NA', placeholder='NA')
      ),
      textInput('comment', 'Comment', placeholder='Name: here\'s my comment',
                width='100%'),
      hr(),
      h5('Bad data string'),
      verbatimTextOutput('string')
    )
  )

  # Produce bad data string ----------------------------------------------------
  server <- function(input, output) {
    output$string <- renderText({
      if (nchar(input$comment) < 1) return(' ')
      t_start <- input$t_start %>%
        as.POSIXct(tz = 'America/Denver') %>%
        format(tz = 'UTC', format = '%Y-%m-%d %H:%M:%OS3')
      t_end   <- input$t_end %>%
        as.POSIXct(tz = 'America/Denver') %>%
        format(tz = 'UTC', format = '%Y-%m-%d %H:%M:%OS3')
      comment <- gsub(',', ';', input$comment, fixed=T)
      paste(sep=', ', t_start, t_end, input$miu_old, input$miu_new, comment)
    })
  }

  viewer <- dialogViewer('Generate bad data string', width=700)
  runGadget(ui, server, viewer = viewer)
}
