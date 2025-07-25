crossAssayTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width=6, title="Correlation", status='primary', solidHeader = TRUE,
    	fluidRow(
      	column(1, "X-axis:"),
      	column(3, selectizeInput(ns("x_type"), "Assay Type:", c("Gene" = "gene", "Protein" = "protein", "Metabolite" = "metabolite"), selected="protein")),
      	column(8, selectizeInput(ns("x_value"), "Selection:", choices=NULL)),
    	),
    	fluidRow(
      	column(1, "Y-axis:"),
      	column(3, selectizeInput(ns("y_type"), "Assay Type:", c("Gene" = "gene", "Protein" = "protein", "Metabolite" = "metabolite"), selected='protein')),
      	column(8, selectizeInput(ns("y_value"), "Selection:", choices=NULL)),
    	),
    	conditionalPanel("input.x_value != '' & input.y_value != ''",
                     scatterplotUI(ns('correlate')),
                     ns=ns
    	)
    )
  )
}

crossAssayTabServer <- function(id, transcript_data, protein_data, metabolite_data) {
  
  moduleServer(id, function(input, output, session) {
    
  	gene_options = reactive({
  		(transcript_data() %>% distinct(Gene) %>% arrange(Gene))$Gene
  	})
  	
  	protein_options = reactive({
  		(protein_data() %>% distinct(Protein) %>% arrange(Protein))$Protein
  	})
  	
  	metabolite_options = reactive({
  		(metabolite_data() %>% distinct(ionTopName) %>% arrange(ionTopName))$ionTopName
  	})
  	
  	x_name = reactive({
  		paste0(input$x_value, '.', input$x_type)
  	})
  	
    x_data = reactive({
      req(input$x_value)
    	
    	if(input$x_type=='gene') {
    		return(
    			transcript_data() %>% 
    			filter(Gene==input$x_value) %>% 
    			dplyr::select(-Gene) %>% 
    			rename(!!x_name() := Value)
    		)
    	} else if(input$x_type=='protein') {
        return(
        	protein_data() %>% 
        	filter(Protein==input$x_value) %>% 
        	dplyr::select(-Protein) %>% 
        	rename(!!x_name() := Value)
        )
      } else if(input$x_type=='metabolite') {
        return(
        	metabolite_data() %>% 
        	filter(ionTopName==input$x_value) %>% 
        	dplyr::select(-ionTopName) %>% 
        	rename(!!x_name() := Value)
        )
      }
    })
    
    y_name = reactive({
    	paste0(input$y_value, '.', input$y_type)
    })
    
    y_data = reactive({
      req(input$y_value)
    	if(input$y_type=='gene') {
    		return(
    			transcript_data() %>% 
    				filter(Gene==input$y_value) %>% 
    				dplyr::select(-Gene) %>% 
    				rename(!!y_name() := Value)
    		)
    	} else if(input$y_type=='protein') {
        return(
        	protein_data() %>% 
        	filter(Protein==input$y_value) %>% 
        	dplyr::select(-Protein) %>% 
        	rename(!!y_name() := Value)
        )
      } else if(input$y_type=='metabolite') {
        return(
        	metabolite_data() %>% 
        	filter(ionTopName==input$y_value) %>% 
        	dplyr::select(-ionTopName) %>% 
        	rename(!!y_name() := Value)
        )
      }
    })
    
    merged_data = reactive({
      if((input$x_type=='gene' & input$y_type=='metabolite') | (input$x_type=='metabolite' & input$y_type=='gene')) {
        return(x_data() %>% inner_join(y_data(), by=c("ComparisonID2", "Type")))
      } else {
        return(x_data() %>% inner_join(y_data(), by=c("ComparisonID", "Type")))
      }
    })
    
    x_options = reactive({
      
    	if(input$x_type=='gene') {
    		gene_options()
    	} else if (input$x_type == 'protein') {
        protein_options()
      } else if (input$x_type == 'metabolite') {
        metabolite_options()
      }
    }) 
    
    observeEvent(x_options(), {
      updateSelectizeInput(session, 'x_value', "Selection:", choices=x_options(), server=TRUE)
    })
    
    y_options = reactive({
    	if(input$y_type=='gene') {
    		gene_options()
    	}else if (input$y_type == 'protein') {
        protein_options()
      } else if (input$y_type == 'metabolite') {
        metabolite_options()
      }
    }) 
    
    observeEvent(y_options(), {
      updateSelectizeInput(session, 'y_value', "Selection:", choices=y_options(), server=TRUE)
    })
    
    scatterplotServer('correlate', merged_data, x_name, y_name)
    
  })
}