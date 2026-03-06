visibilityUI <- function(id) {
	ns <- NS(id)
	tagList(
	  h5("Node Visibility:"),
	  column(3, actionButton(ns("show_all"), "Show All", class = "btn-secondary")),
	  fluidRow(
			column(5, actionButton(ns("hide_selected"), "Hide Selected", class = "btn-secondary")),
			column(5, actionButton(ns("hide_unselected"), "Hide Unselected", class = "btn-secondary"))
		)
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
		
		observeEvent(input$hide_unselected, ignoreInit=FALSE, {
			selected = getSelectedNodes(session)
			invertSelection(session)
			hideSelection(session)
			clearSelection(session)
			selectNodes(session, selected)
		})
		
	})
}