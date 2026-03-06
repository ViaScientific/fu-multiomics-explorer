dfDownloadPopoverUI = function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns('type'), "Format:", choices=c('tsv', 'csv'), selected='tsv'),
    downloadButton(ns('download_button'), label = "Data")
  )
}

dfDownloadPopoverServer = function(id, df, filename_base='data') {
  
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