dfDownloadHandlerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2, downloadButton(ns('download_button'), label = "Data", style = "margin-top: 25px;")),
      column(2, selectizeInput(ns('type'), "Format:", choices=c('tsv', 'csv'), selected='tsv'))
    )
  )
}

dfDownloadHandlerServer <- function(id, df, filename_base='data') {
  
  moduleServer(id, function(input, output, session) {
    
    sep = reactive({
      if (input$type == 'tsv') {
        return('\t')
      } else {
        return(',')
      }
    })
    
    output$download_button <- downloadHandler(
      filename = function() {paste0(filename_base, '.', input$type)},
      content = function(file) {
        write.table(df(), file, row.names = FALSE, sep=sep(), quote = FALSE)
      }
    )
    
  })
}