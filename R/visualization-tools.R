#' Generates interactive ggplot
#'
#' \code{iggplot} starts new interactive gadget session to create and view
#'   the given ggplot object. Returns the zoomed ggplot object when the 
#'   'Done' button is pressed.
#'
#' @param fig ggplot object to be made interactive
#' @examples
#' x <- seq(0, 2*pi, length.out=100)
#' f <- qplot(x, sin(x))
#' iggplot(f)
#'                  
#' @export
iggplot <- function(fig) {
  require(ggplot2)
  require(miniUI)
  require(shiny)
  if(!all(class(fig) %in% c('gg','ggplot')))
    stop('Incorrect fig input. Needs to be a ggplot object.')
  
  x.class <- ggplot2::ggplot_build(fig)$panel$x_scales[[1]]$scale_name
  y.class <- ggplot2::ggplot_build(fig)$panel$y_scales[[1]]$scale_name
  
  # Generate Plot UI ----------------------------------------------------------
  ui <- miniPage(
    gadgetTitleBar('Drag and double click to zoom', left=NULL),
    plotOutput('plot', height='95%',
               dblclick='plot_dblclick',
               hover=hoverOpts('plot_hover', 
                               delay=100, delayType='debounce'),
               brush=brushOpts('plot_brush', resetOnNew=T,
                               delay=5000, delayType='debounce')),
    textOutput('hover_vals')
  )
  
  # Plot reactivity ----------------------------------------------------------=
  server <- function(input, output) {
    lim <- reactiveValues(x=NULL, y=NULL)
    
    plt <- reactive({
      newfig <- fig
      if(!is.null(lim$x))
        newfig <- newfig + xlim(lim$x)
      if(!is.null(lim$y))
        newfig <- newfig + ylim(lim$y)
      newfig
    })
    
    output$plot <- renderPlot(plt())
    
    output$hover_vals <- renderText({
      if(is.null(input$plot_hover)) return(' ')
      else {
        if(x.class=='datetime')
          x <- as.POSIXct(input$plot_hover$x, origin='1970-01-01')
        else x <- input$plot_hover$x
        if(y.class=='datetime')
          y <- as.POSIXct(input$plot_hover$y, origin='1970-01-01')
        else y <- input$plot_hover$y
        
        return(paste0('x = ', x, '    ', 'y = ', y))}
    })
    
    observeEvent(input$plot_brush, {
      brush <- input$plot_brush
      if(!is.null(brush)) {
        if(x.class=='datetime')
          lim$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin='1970-01-01')
        else lim$x <- c(brush$xmin, brush$xmax)
        lim$y <- c(brush$ymin, brush$ymax)
      }
    })
    
    observeEvent(input$plot_dblclick, {
      if(!any(is.null(lim))) {
        lim$x <- NULL
        lim$y <- NULL
      }
    })
    
    observeEvent(input$done, stopApp(plt()))
  }
  
  suppressWarnings(runGadget(ui, server))
}
