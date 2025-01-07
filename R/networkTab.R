#' User Interface for Network Tab
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the network tab, including options for node selection, visibility, and zoom.
#' @importFrom shiny NS tagList h4
#' @importFrom shinydashboard box
#' @importFrom cyjShiny cyjShinyOutput
#' @export
networkTabUI <- function(id) {
  ns <- NS(id)
  addResourcePath(prefix = "www", directoryPath = system.file("extdata", "www", package = "fuMultiomicsExplorer"))
  
  tagList(
    box(
      width = 3, title = "Options", status = 'primary', solidHeader = TRUE,
      h4("Node Selection:"),
      nodeSelectionUI(ns('node_selection')),
      h4("Node Visibility:"),
      visibilityUI(ns('visibility')),
      h4("Zoom:"),
      zoomUI(ns('zoom'))
    ),
    box(
      width = 9, title = "Network", status = 'primary', solidHeader = TRUE,
      cyjShinyOutput(ns('network'), width = "100%", height = 650)
    )
  )
}

#' Server Logic for Network Tab
#'
#' @param id A namespace ID for the module.
#' @return None; used to manage the network tab's server logic, including node selection, visibility, and zoom.
#' @importFrom shiny moduleServer reactive
#' @importFrom cyjShiny cyjShiny renderCyjShiny
#' @importFrom jsonlite fromJSON
#' @export
networkTabServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Reactive: Network input
    network_input <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "clean", "full_network.cyjs", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        paste(readLines(file_path), collapse = "")
      }, error = function(e) {
        message("Error in loading network input: ", e$message)
        NULL
      })
    })
    
    # Reactive: Nodes data
    nodes <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "clean", "full_network.cyjs", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        df <- fromJSON(file_path)
        data.frame(id = df$elements$nodes$data$id, Label = df$elements$nodes$data$label)
      }, error = function(e) {
        message("Error in loading nodes: ", e$message)
        NULL
      })
    })
    
    # Node selection server
    nodeSelectionServer('node_selection', nodes)
    
    # Visibility server
    visibilityServer('visibility')
    
    # Zoom server
    zoomServer('zoom')
    
    # Render network
    output$network <- renderCyjShiny({
      req(network_input())
      tryCatch({
        cyjShiny(network_input(), layoutName = "cola", styleFile = "www/style.js")
      }, error = function(e) {
        message("Error in rendering network: ", e$message)
        NULL
      })
    })
  })
}