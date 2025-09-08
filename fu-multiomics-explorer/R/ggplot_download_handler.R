ggplotDownloadHandlerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2, downloadButton(ns('download_button'), label = "Plot", style = "margin-top: 25px;")),
      column(2, selectizeInput(ns('type'), "Format:", choices=c('pdf', 'png', 'svg', 'jpeg', 'tiff'), selected='pdf')),
      column(2, numericInput(ns('width'), "Width:", value = 1800)),
      column(2, numericInput(ns('height'), "Height:", value = 1200)),
      column(2, selectizeInput(ns('units'), "Units:", choices=c("px", "in", "cm", "mm"), selected='px')),
      column(2, shinyjs::hidden(numericInput(ns('dpi'), "dpi:", value = 300)))
    )
  )
}

ggplotDownloadHandlerServer <- function(id, plot, filename_base='plot') {
  
  moduleServer(id, function(input, output, session) {
  
    observeEvent(input$units, ignoreInit = TRUE, {
      
      if (input$units == 'px') {
        updateNumericInput(session, 'width', "Width:", value = 1800)
        updateNumericInput(session, 'height', "Height:", value = 1200)
      } else if (input$units == 'in') {
        updateNumericInput(session, 'width', "Width:", value = 6)
        updateNumericInput(session, 'height', "Height:", value = 4)
      } else if (input$units == 'cm') {
        updateNumericInput(session, 'width', "Width:", value = 15)
        updateNumericInput(session, 'height', "Height:", value = 10)
      } else if (input$units == 'mm') {
        updateNumericInput(session, 'width', "Width:", value = 150)
        updateNumericInput(session, 'height', "Height:", value = 100)
      }
    })
    
    observeEvent(input$type, ignoreInit = TRUE, {
      
      if (input$type == 'pdf') {
        shinyjs::hide('dpi')
      } else if (input$type == 'png') {
        shinyjs::show('dpi')
      } else if (input$type == 'svg') {
        shinyjs::hide('dpi')
      } else if (input$type == 'jpeg') {
        shinyjs::show('dpi')
      } else if (input$type == 'tiff') {
        shinyjs::show('dpi')
      }
    })
    
    output$download_button <- downloadHandler(
      filename = function() {paste0(filename_base, '.', input$type)},
      content = function(file) {
        ggsave(file, plot = plot(), device = input$type, units = input$units, width = input$width, height = input$height, dpi = input$dpi)
      }
    )

  })
}