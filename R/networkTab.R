networkTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width=12, title="Network", status='primary', solidHeader = TRUE,
    	cyjShinyOutput(ns('network'), width="100%", height=650)
    )
  )
}

networkTabServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
  	network_input = reactive({
  		paste(readLines('data/clean/full_network.cyjs'), collapse = '')
  	})
  	
    output$network <- renderCyjShiny({
      cyjShiny(network_input(), layoutName="cola", styleFile = 'www/style.js')
    })
    
  })
}