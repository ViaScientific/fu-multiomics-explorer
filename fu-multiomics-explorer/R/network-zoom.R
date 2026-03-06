zoomUI <- function(id) {
	ns <- NS(id)
	tagList(
	  h5('Zoom:'),
	  fluidRow(
  		column(4, actionButton(ns("fit"), "All Nodes", class = "btn-secondary")),
		  column(6, actionButton(ns("fitSelected"), "Selected Nodes", class = "btn-secondary"))
	  )
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