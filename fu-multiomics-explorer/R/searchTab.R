searchTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, selectizeInput(ns("type"), "Data Type:",choices=c('', "Gene" = "gene", "Protein" = "protein", "Metabolite" = "metabolite"), selected='')),
      column(9, selectizeInput(ns("value"), "Selection:", choices=NULL)),
    ),
    conditionalPanel("input.type != '' & input.value != ''",
                     barplotUI(ns('barplot')),
                     ns=ns
    )
  )
}

searchTabServer <- function(id, gene_options, protein_options, metabolic_options, gene_data, protein_data_processed, metabolic_data_processed) {
  
  moduleServer(id, function(input, output, session) {
    
    choices = reactive({
      if (input$type == 'protein') {
        protein_options()
      } else if (input$type == 'metabolite') {
        metabolic_options()
      } else {
        gene_options()
      }
    }) 
    
    observeEvent(choices(), {
      updateSelectizeInput(session, 'value', "Selection:", choices=choices(), selected='', server=TRUE)
    })
  
    fetched_data = reactive({
      if (input$value %in% gene_options()) {
        metadata = gene_data()@meta.data %>% tibble::rownames_to_column('cell_barcode') %>% dplyr::select(cell_barcode, Donor, Type, celltype)
        FetchData(gene_data(), vars = input$value) %>% tibble::rownames_to_column("cell_barcode") %>% left_join(metadata, by='cell_barcode') %>%
          group_by(Donor, Type) %>% summarise(Count = sum(!!sym(input$value)), .groups='drop')
      } else {
        return(NULL)
      }
    })
    
    data_filtered = reactive({
      req(input$value)
      if (input$type == 'protein') {
        protein_data_processed() %>% 
          filter(Protein==input$value) %>% 
          dplyr::select(-Protein)
      } else if (input$type == 'metabolite') {
        metabolic_data_processed() %>% 
          filter(ionTopName==input$value) %>% 
          dplyr::select(-ionTopName)
      } else { 
          fetched_data()
      }
    })
    
    barplotServer("barplot", data_filtered)
    
  })
}