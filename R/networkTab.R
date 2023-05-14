networkTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      cyjShinyOutput(ns('cyjShiny'), width="97%", height=800)
    )
  )
}

networkTabServer <- function(id, network) {
  
  moduleServer(id, function(input, output, session) {
    
    output$cyjShiny <- renderCyjShiny({
      cyjShiny(network(), layoutName="cola", styleFile = 'style.js')
    })
    
  }
  )
}