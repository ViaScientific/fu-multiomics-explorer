nodeSelectionUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(9, selectizeInput(ns("select_by_name_list"), "Select by ID:", choices = NULL, multiple=TRUE)),
			column(3, actionButton(ns("select_by_name"), "Select",  style = 'margin-top:25px;'))
		),
		actionButton(ns("clearSelection"), "Unselect All"),
		actionButton(ns("invert_selection"), "Invert Selection"),
		actionButton(ns("sfn"), "Highlight Neighbors")
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