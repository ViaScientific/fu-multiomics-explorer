searchTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, selectizeInput(ns("type"), "Data Type:",choices=c('', "Gene" = "gene", "Protein" = "protein", "Metabolite" = "metabolite"))),
      column(9, selectizeInput(ns("value"), "Selection:", choices=NULL)),
    ),
    conditionalPanel("input.type != '' & input.value != ''",
                     barplotUI(ns('barplot')),
                     ns=ns
    )
  )
}

searchTabServer <- function(id, gene_options, protein_options, metabolic_options, protein_data_processed, metabolic_data_processed) {
  
  moduleServer(id, function(input, output, session) {
    
    choices = reactive({
      if (input$type == 'protein') {
        protein_options()
      } else if (input$type == 'metabolite') {
        metabolic_options()
      } else {
       return(NULL)
      }
    }) 
    
    observeEvent(choices(), {
      updateSelectizeInput(session, 'value', "Selection:", choices=choices(), server=TRUE)
    })
    
    data_filtered = reactive({
      
      if (input$type == 'protein') {
        protein_data_processed() %>% 
          filter(Protein==input$value) %>% 
          select(-Protein)
      } else if (input$type == 'metabolite') {
        metabolic_data_processed() %>% 
          filter(ionTopName==input$value) %>% 
          select(-ionTopName)
      } else {
        return(NULL)
      }
      
    })
    
    barplotServer("barplot", data_filtered)
  })
}