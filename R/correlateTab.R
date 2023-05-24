correlateTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width=6, title="Correlation", status='primary', solidHeader = TRUE,
    	fluidRow(
      	column(1, "X-axis:"),
      	column(3, selectizeInput(ns("x_type"), "Data Type:", c("Protein" = "protein", "Metabolite" = "metabolite"))),
      	column(8, selectizeInput(ns("x_value"), "Selection:", choices=NULL)),
    	),
    	fluidRow(
      	column(1, "Y-axis:"),
      	column(3, selectizeInput(ns("y_type"), "Data Type:", c("Protein" = "protein", "Metabolite" = "metabolite"))),
      	column(8, selectizeInput(ns("y_value"), "Selection:", choices=NULL)),
    	),
    	conditionalPanel("input.x_value != '' & input.y_value != ''",
                     scatterplotUI(ns('correlate')),
                     ns=ns
    	)
    )
  )
}

correlateTabServer <- function(id, protein_data, metabolite_data) {
  
  moduleServer(id, function(input, output, session) {
    
  	protein_options = reactive({
  		(protein_data() %>% arrange(Protein) %>% distinct(Protein))$Protein
  	})
  	
  	metabolite_options = reactive({
  		(metabolite_data() %>% arrange(ionTopName) %>% distinct(ionTopName))$ionTopName
  	})
  	
    x_data = reactive({
      req(input$x_value)
      if(input$x_type=='protein') {
        return(protein_data() %>% filter(Protein==input$x_value) %>% dplyr::select(-Protein) %>% rename(!!input$x_value := Value))
      } else if(input$x_type=='metabolite') {
        return(metabolite_data() %>% filter(ionTopName==input$x_value) %>% dplyr::select(-ionTopName) %>% rename(!!input$x_value := Value))
      }
    })
    
    y_data = reactive({
      req(input$y_value)
      if(input$y_type=='protein') {
        return(protein_data() %>% filter(Protein==input$y_value) %>% dplyr::select(-Protein) %>% rename(!!input$y_value := Value))
      } else if(input$y_type=='metabolite') {
        return(metabolite_data() %>% filter(ionTopName==input$y_value) %>% dplyr::select(-ionTopName) %>% rename(!!input$y_value := Value))
      }
    })
    
    merged_data = reactive({
      if(input$x_type=='protein' & input$y_type=='protein') {
        return(x_data() %>% left_join(y_data(), by=c("Donor", "Type")))
      } else {
        return(x_data() %>% left_join(y_data(), by=c("Donor", "Type")))
      }
    })
    
    x_options = reactive({
      if (input$x_type == 'protein') {
        protein_options()
      } else if (input$x_type == 'metabolite') {
        metabolite_options()
      }
    }) 
    
    observeEvent(x_options(), {
      updateSelectizeInput(session, 'x_value', "Selection:", choices=x_options(), server=TRUE)
    })
    
    y_options = reactive({
      if (input$y_type == 'protein') {
        protein_options()
      } else if (input$y_type == 'metabolite') {
        metabolite_options()
      }
    }) 
    
    observeEvent(y_options(), {
      updateSelectizeInput(session, 'y_value', "Selection:", choices=y_options(), server=TRUE)
    })
    
    scatterplotServer('correlate', merged_data, reactive(input$x_value), reactive(input$y_value))
    
  })
}