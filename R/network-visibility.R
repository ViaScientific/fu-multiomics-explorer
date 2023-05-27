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