#' User Interface for Search Tab
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the search tab, allowing users to search and visualize data.
#' @importFrom shiny NS tagList fluidRow column selectizeInput conditionalPanel
#' @export
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

#' Server Logic for Search Tab
#'
#' @param id A namespace ID for the module.
#' @param gene_options A reactive list of gene options.
#' @param protein_options A reactive list of protein options.
#' @param metabolic_options A reactive list of metabolite options.
#' @param gene_data A Seurat object containing gene expression data.
#' @param protein_data_processed A reactive data frame of processed protein data.
#' @param metabolic_data_processed A reactive data frame of processed metabolite data.
#' @return None; used to manage the search tab's server logic.
#' @importFrom shiny moduleServer reactive observeEvent req
#' @importFrom dplyr filter select group_by summarise left_join mutate
#' @importFrom tibble rownames_to_column
#' @importFrom Seurat FetchData
#' @export
searchTabServer <- function(id, gene_options, protein_options, metabolic_options, gene_data, protein_data_processed, metabolic_data_processed) {
  
  moduleServer(id, function(input, output, session) {
    
    # Reactive choices for selection input
    choices <- reactive({
      tryCatch({
        if (input$type == 'protein') {
          protein_options()
        } else if (input$type == 'metabolite') {
          metabolic_options()
        } else {
          gene_options()
        }
      }, error = function(e) {
        message("Error in determining choices: ", e$message)
        NULL
      })
    })
    
    # Update the selection input based on the data type
    observeEvent(choices(), {
      updateSelectizeInput(session, 'value', "Selection:", choices = choices(), selected = '', server = TRUE)
    }, ignoreNULL = TRUE)
    
    # Fetch gene data
    fetched_data <- reactive({
      tryCatch({
        req(input$value)
        if (input$value %in% gene_options()) {
          metadata <- gene_data()@meta.data %>%
            tibble::rownames_to_column('cell_barcode') %>%
            dplyr::select(cell_barcode, Donor, Type, celltype)
          
          FetchData(gene_data(), vars = input$value) %>%
            tibble::rownames_to_column("cell_barcode") %>%
            left_join(metadata, by = 'cell_barcode') %>%
            group_by(Donor, Type) %>%
            summarise(Count = sum(!!sym(input$value)), .groups = 'drop')
        } else {
          NULL
        }
      }, error = function(e) {
        message("Error in fetching gene data: ", e$message)
        NULL
      })
    })
    
    # Filter data based on selection type
    data_filtered <- reactive({
      tryCatch({
        req(input$value)
        if (input$type == 'protein') {
          protein_data_processed() %>%
            filter(Protein == input$value) %>%
            dplyr::select(-Protein)
        } else if (input$type == 'metabolite') {
          metabolic_data_processed() %>%
            filter(ionTopName == input$value) %>%
            dplyr::select(-ionTopName)
        } else {
          fetched_data()
        }
      }, error = function(e) {
        message("Error in filtering data: ", e$message)
        NULL
      })
    })
    
    # Invoke the barplot module
    barplotServer("barplot", data_filtered)
    
  })
}