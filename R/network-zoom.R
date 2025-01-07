#' User Interface for Zoom Controls
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for zoom controls.
#' @importFrom shiny NS tagList actionButton
#' @export
zoomUI <- function(id) {
	ns <- NS(id)
	tagList(
		actionButton(ns("fit"), "All Nodes"),
		actionButton(ns("fitSelected"), "Selected Nodes")
	)
}

#' Server Logic for Zoom Controls
#'
#' @param id A namespace ID for the module.
#' @return None; used to control zooming of nodes in the UI.
#' @importFrom shiny moduleServer observeEvent
#' @importFrom cyjShiny fitSelected
#' @export
zoomServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Fit all nodes
    observeEvent(input$fit, ignoreInit = FALSE, {
      tryCatch({
        fit(session, 10)
      }, error = function(e) {
        message("Error in fitting all nodes: ", e$message)
      })
    })
    
    # Fit selected nodes
    observeEvent(input$fitSelected, ignoreInit = TRUE, {
      tryCatch({
        fitSelected(session, 10)
      }, error = function(e) {
        message("Error in fitting selected nodes: ", e$message)
      })
    })
    
  })
}