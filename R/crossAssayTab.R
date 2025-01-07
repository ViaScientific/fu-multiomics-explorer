#' User Interface for Cross-Assay Tab
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the cross-assay correlation tab.
#' @importFrom shiny NS tagList fluidRow column conditionalPanel
#' @importFrom shinydashboard box
#' @export
crossAssayTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width=6, title="Correlation", status='primary', solidHeader = TRUE,
    	fluidRow(
      	column(1, "X-axis:"),
      	column(3, selectizeInput(ns("x_type"), "Assay Type:", c("Gene" = "gene", "Protein" = "protein", "Metabolite" = "metabolite"), selected="protein")),
      	column(8, selectizeInput(ns("x_value"), "Selection:", choices=NULL)),
    	),
    	fluidRow(
      	column(1, "Y-axis:"),
      	column(3, selectizeInput(ns("y_type"), "Assay Type:", c("Gene" = "gene", "Protein" = "protein", "Metabolite" = "metabolite"), selected='protein')),
      	column(8, selectizeInput(ns("y_value"), "Selection:", choices=NULL)),
    	),
    	conditionalPanel("input.x_value != '' & input.y_value != ''",
                     scatterplotUI(ns('correlate')),
                     ns=ns
    	)
    )
  )
}

#' Server Logic for Cross-Assay Tab
#'
#' @param id A namespace ID for the module.
#' @param transcript_data Reactive object containing transcriptomic data.
#' @param protein_data Reactive object containing proteomic data.
#' @param metabolite_data Reactive object containing metabolomic data.
#' @return None; used for rendering UI components and server logic.
#' @importFrom shiny moduleServer reactive observeEvent req
#' @importFrom dplyr %>% distinct arrange filter select rename inner_join
#' @export
crossAssayTabServer <- function(id, transcript_data, protein_data, metabolite_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive for gene options
    gene_options <- reactive({
      tryCatch({
        (transcript_data() %>% distinct(Gene) %>% arrange(Gene))$Gene
      }, error = function(e) {
        message("Error in generating gene options: ", e$message)
        NULL
      })
    })
    
    # Reactive for protein options
    protein_options <- reactive({
      tryCatch({
        (protein_data() %>% distinct(Protein) %>% arrange(Protein))$Protein
      }, error = function(e) {
        message("Error in generating protein options: ", e$message)
        NULL
      })
    })
    
    # Reactive for metabolite options
    metabolite_options <- reactive({
      tryCatch({
        (metabolite_data() %>% distinct(ionTopName) %>% arrange(ionTopName))$ionTopName
      }, error = function(e) {
        message("Error in generating metabolite options: ", e$message)
        NULL
      })
    })
    
    # Reactive for x_name
    x_name <- reactive({
      req(input$x_value)
      paste0(input$x_value, '.', input$x_type)
    })
    
    # Reactive for x_data
    x_data <- reactive({
      req(input$x_value)
      tryCatch({
        if (input$x_type == 'gene') {
          transcript_data() %>%
            filter(Gene == input$x_value) %>%
            select(-Gene) %>%
            rename(!!x_name() := Value)
        } else if (input$x_type == 'protein') {
          protein_data() %>%
            filter(Protein == input$x_value) %>%
            select(-Protein) %>%
            rename(!!x_name() := Value)
        } else if (input$x_type == 'metabolite') {
          metabolite_data() %>%
            filter(ionTopName == input$x_value) %>%
            select(-ionTopName) %>%
            rename(!!x_name() := Value)
        }
      }, error = function(e) {
        message("Error in generating x_data: ", e$message)
        NULL
      })
    })
    
    # Reactive for y_name
    y_name <- reactive({
      req(input$y_value)
      paste0(input$y_value, '.', input$y_type)
    })
    
    # Reactive for y_data
    y_data <- reactive({
      req(input$y_value)
      tryCatch({
        if (input$y_type == 'gene') {
          transcript_data() %>%
            filter(Gene == input$y_value) %>%
            select(-Gene) %>%
            rename(!!y_name() := Value)
        } else if (input$y_type == 'protein') {
          protein_data() %>%
            filter(Protein == input$y_value) %>%
            select(-Protein) %>%
            rename(!!y_name() := Value)
        } else if (input$y_type == 'metabolite') {
          metabolite_data() %>%
            filter(ionTopName == input$y_value) %>%
            select(-ionTopName) %>%
            rename(!!y_name() := Value)
        }
      }, error = function(e) {
        message("Error in generating y_data: ", e$message)
        NULL
      })
    })
    
    # Reactive for merged_data
    merged_data <- reactive({
      req(input$x_type)
      req(input$y_type)
      tryCatch({
        if ((input$x_type == 'gene' & input$y_type == 'metabolite') || 
            (input$x_type == 'metabolite' & input$y_type == 'gene')) {
          x_data() %>% inner_join(y_data(), by = c("ComparisonID2", "Type"))
        } else {
          x_data() %>% inner_join(y_data(), by = c("ComparisonID", "Type"))
        }
      }, error = function(e) {
        message("Error in merging data: ", e$message)
        NULL
      })
    })
    
    # Reactive for x_options
    x_options <- reactive({
      tryCatch({
        if (input$x_type == 'gene') {
          gene_options()
        } else if (input$x_type == 'protein') {
          protein_options()
        } else if (input$x_type == 'metabolite') {
          metabolite_options()
        }
      }, error = function(e) {
        message("Error in generating x_options: ", e$message)
        NULL
      })
    })
    
    # Update x_value selectize input
    observeEvent(x_options(), {
      updateSelectizeInput(session, 'x_value', "Selection:", choices = x_options(), server = TRUE)
    })
    
    # Reactive for y_options
    y_options <- reactive({
      tryCatch({
        if (input$y_type == 'gene') {
          gene_options()
        } else if (input$y_type == 'protein') {
          protein_options()
        } else if (input$y_type == 'metabolite') {
          metabolite_options()
        }
      }, error = function(e) {
        message("Error in generating y_options: ", e$message)
        NULL
      })
    })
    
    # Update y_value selectize input
    observeEvent(y_options(), {
      updateSelectizeInput(session, 'y_value', "Selection:", choices = y_options(), server = TRUE)
    })
    
    # Call scatterplot module
    scatterplotServer('correlate', merged_data, x_name, y_name)
    
  })
}