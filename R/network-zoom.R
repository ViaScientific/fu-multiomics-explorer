zoomUI <- function(id) {
	ns <- NS(id)
	tagList(
		actionButton(ns("fit"), "All Nodes"),
		actionButton(ns("fitSelected"), "Selected Nodes")
	)
}

zoomServer <- function(id) {
	
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$fit, ignoreInit=FALSE, {
			fit(session, 10)
		})
		
		observeEvent(input$fitSelected, ignoreInit=TRUE,{
			fitSelected(session, 10)
		})
		
		
	})
}