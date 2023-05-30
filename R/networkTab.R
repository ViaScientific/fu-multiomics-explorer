networkTabUI <- function(id) {
  ns <- NS(id)
  tagList(
  	box(width=3, title="Options", status='primary', solidHeader = TRUE,
  		h4("Node Selection:"),
  		nodeSelectionUI(ns('node_selection')),
  		h4("Node Visibility:"),
  		visibilityUI(ns('visibility')),
  		h4('Zoom:'),
  		zoomUI(ns('zoom'))
  	),
    box(width=9, title="Network", status='primary', solidHeader = TRUE,
    	cyjShinyOutput(ns('network'), width="100%", height=650)
    )
  )
}

networkTabServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
  	network_input = reactive({
  		paste(readLines('data/clean/full_network.cyjs'), collapse = '')
  	})
  	
  	nodes = reactive({
  		df = fromJSON('data/clean/full_network.cyjs')
  		data.frame(id=df$elements$nodes$data$id, Label=df$elements$nodes$data$label)
  	})
  	
  	nodeSelectionServer('node_selection', nodes)
  	visibilityServer('visibility')
  	zoomServer('zoom')
  	
    output$network <- renderCyjShiny({
      cyjShiny(network_input(), layoutName="cola", styleFile = 'www/style.js')
    })
    
  })
}