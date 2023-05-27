visibilityUI <- function(id) {
	ns <- NS(id)
	tagList(
		actionButton(ns("show_all"), "Show All"),
		actionButton(ns("hide_selected"), "Hide Selected"),
	)
}

visibilityServer <- function(id) {
	
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$show_all, ignoreInit=FALSE, {
			showAll(session)
		})
		
		observeEvent(input$hide_selected, ignoreInit=FALSE, {
			hideSelection(session)
		})
		
	})
}