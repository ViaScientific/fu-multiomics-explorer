#' User Interface for Node Visibility Control
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for controlling the visibility of nodes.
#' @importFrom shiny NS tagList fluidRow column actionButton
#' @export
visibilityUI <- function(id) {
	ns <- NS(id)
	tagList(
		actionButton(ns("show_all"), "Show All"),
		fluidRow(
			column(6, actionButton(ns("hide_selected"), "Hide Selected")),
			column(6, actionButton(ns("hide_unselected"), "Hide Unselected"))
		)
	)
}

#' Server Logic for Node Visibility Control
#'
#' @param id A namespace ID for the module.
#' @return None; used to manage node visibility in the UI.
#' @importFrom shiny moduleServer observeEvent
#' @importFrom cyjShiny hideSelection getSelectedNodes invertSelection
#'    clearSelection selectNodes showAll
#' @export
visibilityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Show all nodes
    observeEvent(input$show_all, ignoreInit = FALSE, {
      tryCatch({
        showAll(session)
      }, error = function(e) {
        message("Error in showing all nodes: ", e$message)
      })
    })
    
    # Hide selected nodes
    observeEvent(input$hide_selected, ignoreInit = FALSE, {
      tryCatch({
        hideSelection(session)
      }, error = function(e) {
        message("Error in hiding selected nodes: ", e$message)
      })
    })
    
    # Hide unselected nodes
    observeEvent(input$hide_unselected, ignoreInit = FALSE, {
      tryCatch({
        selected <- getSelectedNodes(session)
        invertSelection(session)
        hideSelection(session)
        clearSelection(session)
        selectNodes(session, selected)
      }, error = function(e) {
        message("Error in hiding unselected nodes: ", e$message)
      })
    })
    
  })
}