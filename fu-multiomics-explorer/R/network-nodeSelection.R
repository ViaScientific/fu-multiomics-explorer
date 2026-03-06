nodeSelectionUI <- function(id) {
	ns <- NS(id)
	tagList(
	  h5("Node Selection:"),
		fluidRow(
			column(9, selectizeInput(ns("select_by_name_list"), "Select by ID:", choices = NULL, multiple=TRUE)),
			column(3, actionButton(ns("select_by_name"), "Select",  class = "btn-secondary", style = 'margin-top:35px;'))
		),
		column(4, actionButton(ns("clearSelection"), "Unselect All", class = "btn-secondary")),
		fluidRow(
		  column(5, actionButton(ns("invert_selection"), "Invert Selection", class = "btn-secondary")),
		  column(6, actionButton(ns("sfn"), "Highlight Neighbors", class = "btn-secondary"))
		)
	)
}

nodeSelectionServer <- function(id, nodes) {
	
	moduleServer(id, function(input, output, session) {
		
		node_options = reactive({
			nodes()$Label
		})
		
		observeEvent(node_options(), ignoreInit=FALSE, {
			updateSelectizeInput(session, 'select_by_name_list', "Select by ID:", choices=node_options(), selected='', server=TRUE)
		})
		
		observeEvent(input$select_by_name, {
			if (!is.null(input$select_by_name_list)) {
				selected = nodes() %>% filter(Label %in% input$select_by_name_list)
				selectNodes(session, selected$id)
				updateSelectizeInput(session, 'select_by_name_list', "Select by ID:", choices=node_options(), selected='', server=TRUE)
			}
		})
		
		observeEvent(input$clearSelection, ignoreInit=TRUE, {
			clearSelection(session)
			getSelectedNodes(session)
		})
		
		observeEvent(input$sfn,  ignoreInit=TRUE,{
			selectFirstNeighbors(session)
		})
		
		observeEvent(input$invert_selection,  ignoreInit=TRUE,{
			invertSelection(session)
		})
		
	})
}