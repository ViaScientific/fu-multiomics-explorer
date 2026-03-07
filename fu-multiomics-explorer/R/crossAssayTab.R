crossAssayTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    card(
      card_header_with_download_and_settings(
        "Correlation",
        popover(
          title = "Download Options", bs_icon("download"),
          accordion(
            id = ns("download_acc"),
            class = "acc--dense",
            open = TRUE,
            accordion_panel(
              "Plot",
              ggplotDownloadPopoverUI(ns('plot_download')),
            ),
            accordion_panel(
              "Data",
              dfDownloadPopoverUI(ns('data_download'))
            )
          )
        ),
        popover(
          title = "Scatterplot Settings", bs_icon("gear"),
          numericInput(ns("point_size"), "Point Size", value=3),
          selectInput(ns("color_by"), "Color By", choices=NULL),
          selectInput(ns("shape_by"), "Shape By", choices=NULL),
          selectInput(ns("facet_by"), "Group By", choices=NULL),
          input_switch(ns('include_r2'), "Display r2", value = TRUE),
          input_switch(ns('include_reg'), "Display Regression", value = TRUE)
        )
      ),
      card_body(
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
        plotOutput(ns("plot")) %>% withSpinner(image='spinner.gif')
      )
    )
  )
}

crossAssayTabServer <- function(id, transcript_data, protein_data, metabolite_data) {
  
  moduleServer(id, function(input, output, session) {
    
  	gene_options = reactive({
  	  transcript_data() %>% distinct(Gene) %>% arrange(Gene) %>% pull(Gene)
  	})
  	
  	protein_options = reactive({
  	  protein_data() %>% distinct(Protein) %>% arrange(Protein) %>% pull(Protein)
  	})
  	
  	metabolite_options = reactive({
  	  metabolite_data() %>% distinct(ionTopName) %>% arrange(ionTopName) %>% pull(ionTopName)
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
      freezeReactiveValue(input, "x_value")
      updateSelectizeInput(session, 'x_value', "Selection:", choices=x_options(), selected=x_options()[1], server=TRUE)
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
      freezeReactiveValue(input, "y_value")
      updateSelectizeInput(session, 'y_value', "Selection:", choices=y_options(), selected=y_options()[2], server=TRUE)
    })
    
    valid_xy <- reactive({
      req(x_name(), y_name())
      !identical(x_name(), y_name())
    })
    
    column_names = reactive({ colnames(merged_data())})
    
    observeEvent(merged_data(), {
      updateSelectInput(session, 'color_by', "Color By", choices=c('None', column_names()))
      updateSelectInput(session, 'shape_by', "Shape By", choices=c('None', column_names()))
      updateSelectInput(session, 'facet_by', "Group By", choices=c('None', column_names()))
    })
    
    regression = reactive({
      req(valid_xy())
      summary(lm(data=merged_data(), formula = as.formula(paste("`", y_name(), "`~`", x_name(), "`", sep=''))))
    })
    
    scatter_plot = reactive({
      
      if (input$color_by=='None') { color_by_value = '' } else { color_by_value= input$color_by }
      if (input$shape_by=='None') { shape_by_value = '' } else { shape_by_value= input$shape_by }
      
      ggplot(merged_data(), aes(x=!!sym(x_name()), y=!!sym(y_name()), color=!!sym(color_by_value), shape=!!sym(shape_by_value))) +
        {if (input$facet_by!='None') facet_wrap(as.formula(paste('~', input$facet_by)))} +
        theme_classic() +
        theme(strip.background = element_blank()) +
        {if (input$include_reg) geom_abline(slope=regression()$coefficients[2], intercept=regression()$coefficients[1], linetype=2, color='grey')} +
        {if (input$include_r2) annotate(geom="text", x=-Inf, y=Inf, label=paste('r^2 == ', round(regression()$r.squared, 3)), vjust=1, hjust=-.1, parse=TRUE)} +
        geom_point(size=input$point_size)
    })
    
    output$plot <- renderPlot({
      if (identical(x_name(), y_name())) {
        plot.new()
        text(0.5, 0.5, "Please choose different values for X and Y.")
        return(invisible())
      }
      scatter_plot()
    })
    
    download_data = reactive({
      merged_data() %>% select(-any_of(c("ComparisonID", "ComparisonID2")))
    })
    
    ggplotDownloadPopoverServer('plot_download', scatter_plot, "scatter_plot")
    dfDownloadPopoverServer('data_download', download_data, "scatter_plot_data")

  })
}