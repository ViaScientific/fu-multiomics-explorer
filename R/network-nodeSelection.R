#' User Interface for Node Selection
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the node selection module.
#' @importFrom shiny NS tagList fluidRow column actionButton selectizeInput
#' @export
nodeSelectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(9, selectizeInput(ns("select_by_name_list"), "Select by ID:", choices = NULL, multiple = TRUE)),
      column(3, actionButton(ns("select_by_name"), "Select", style = 'margin-top:25px;'))
    ),
    actionButton(ns("clearSelection"), "Unselect All"),
    actionButton(ns("invert_selection"), "Invert Selection"),
    actionButton(ns("sfn"), "Highlight Neighbors")
  )
}

#' Server Logic for Node Selection
#'
#' @param id A namespace ID for the module.
#' @param nodes A reactive expression returning a data frame of nodes.
#' @return None; used for managing node selection in the UI.
#' @importFrom shiny moduleServer reactive observeEvent
#' @importFrom dplyr filter
#' @importFrom cyjShiny selectNodes getSelectedNodes clearSelection
#'    selectFirstNeighbors invertSelection loadStyleFile styleFile
#' @export
nodeSelectionServer <- function(id, nodes) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive: Node options
    node_options <- reactive({
      tryCatch({
        nodes()$Label
      }, error = function(e) {
        message("Error in loading node options: ", e$message)
        NULL
      })
    })
    
    # Update Selectize Input with Node Options
    observeEvent(node_options(), ignoreInit = FALSE, {
      tryCatch({
        updateSelectizeInput(session, 'select_by_name_list', "Select by ID:", choices = node_options(), selected = '', server = TRUE)
      }, error = function(e) {
        message("Error in updating node options: ", e$message)
      })
    })
    
    # Select Nodes by Name
    observeEvent(input$select_by_name, {
      tryCatch({
        if (!is.null(input$select_by_name_list)) {
          selected <- nodes() %>% filter(Label %in% input$select_by_name_list)
          selectNodes(session, selected$id)
          updateSelectizeInput(session, 'select_by_name_list', "Select by ID:", choices = node_options(), selected = '', server = TRUE)
        }
      }, error = function(e) {
        message("Error in selecting nodes: ", e$message)
      })
    })
    
    # Clear Node Selection
    observeEvent(input$clearSelection, ignoreInit = TRUE, {
      tryCatch({
        clearSelection(session)
        getSelectedNodes(session)
      }, error = function(e) {
        message("Error in clearing node selection: ", e$message)
      })
    })
    
    # Highlight Neighbors
    observeEvent(input$sfn, ignoreInit = TRUE, {
      tryCatch({
        selectFirstNeighbors(session)
      }, error = function(e) {
        message("Error in highlighting neighbors: ", e$message)
      })
    })
    
    # Invert Node Selection
    observeEvent(input$invert_selection, ignoreInit = TRUE, {
      tryCatch({
        invertSelection(session)
      }, error = function(e) {
        message("Error in inverting node selection: ", e$message)
      })
    })
  })
}