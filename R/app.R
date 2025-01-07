#' Launch the Multiomics Explorer Shiny App
#'
#' @description This function launches the Multiomics Explorer Shiny application, which provides tools for analyzing and visualizing multi-omics data.
#' @importFrom shiny fluidPage addResourcePath tags tabPanel plotOutput navbarPage shinyApp h3 
#' @importFrom shinyBS bsCollapse 
#' @importFrom shinycssloaders withSpinner
#' @export

# Define the UI
ui <- function() {
    # Add resource path for static files
    addResourcePath(prefix = "www", directoryPath = system.file("extdata", "www", package = "fuMultiomicsExplorer"))
    
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
      ),
      navbarPage("Multiomics Explorer",
                 tabPanel("Transcriptomic Data", transcriptomicTabUI('transcriptomic_tab')),
                 tabPanel("Proteomic Data", proteomicTabUI('proteomic_tab')),
                 tabPanel("Metabolomic Data", metabolomicTabUI('metabolomic_tab')),
                 tabPanel("Linkage Assay", crossAssayTabUI('crossAssay_tab')),
                 tabPanel("Network", networkTabUI('network_tab'))
      )
    )
}

#' Launch the Multiomics Explorer Shiny App
#'
#' @description This function launches the Multiomics Explorer Shiny application, which provides tools for analyzing and visualizing multi-omics data.
#' @param input, input params from UI
#' @param output, output params to UI
#' @param session, session variable
#' @return the panel for main plots;
#' @export
  # Define the server
server <- function(input, output, session) {
    tryCatch({
      # Initialize the modules
      transcript_data <- transcriptomicTabServer('transcriptomic_tab')
      protein_data <- proteomicTabServer('proteomic_tab')
      metabolite_data <- metabolomicTabServer('metabolomic_tab')
      
      # Linkage assay module
      crossAssayTabServer('crossAssay_tab', transcript_data, protein_data, metabolite_data)
      
      # Network module
      networkTabServer('network_tab')
    }, error = function(e) {
      # Log the error and show a message in the console
      message("An error occurred: ", e$message)
      message("Check the file paths and data inputs.")
    })
}

options(
  warn = -1,
  shiny.sanitize.errors = TRUE,
  shiny.host = "0.0.0.0",
  shiny.port = 8789,
  shiny.reactlog = TRUE
)


# Run the app
shinyApp(ui = ui, server = server)
