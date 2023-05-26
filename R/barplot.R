barplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("out")) %>% withSpinner(image='spinner.gif'),
    h3("Figure Options"),
    bsCollapse(id = "figure_options", open = "",
    	bsCollapsePanel("Input Data", 
    		column(3, selectInput(ns("x"), "X", choices=NULL)),
    		column(3, selectInput(ns("y"), "Y", choices=NULL)),
    		column(3, selectInput(ns("fill_by"), "Color By", choices=NULL)),
    		column(3, selectInput(ns("facet_by"), "Group By", choices=NULL)),
    		style = "info"
    	)
    )
  )
}

barplotServer <- function(id, df, default_fill, default_group) {
  
  moduleServer(id, function(input, output, session) {
    
    column_names = reactive({ colnames(df()) })
    
    column_classes = reactive({ sapply(df(), class) })
    
    numeric_columns = reactive({ (data.frame(Column=column_names(), Class=column_classes()) %>% filter(Class == 'numeric'))$Column })
   
    observeEvent(df(), {
      updateSelectInput(session, 'x', "X", choices=column_names(), selected='Donor')
      updateSelectInput(session, 'y', "Y", choices=numeric_columns(), selected='Value')
      updateSelectInput(session, 'fill_by', "Color By", choices=c('None', column_names()), selected=default_fill)
      updateSelectInput(session, 'facet_by', "Group By", choices=c('None', column_names()), selected=default_group)
    })
    
    output$out = renderPlot({
    	
    	req(input$x)
    	
      if (input$fill_by=='None') { fill_by_value = '' } else { fill_by_value=input$fill_by }
      
      ggplot(df(), aes(x=!!sym(input$x), y=!!sym(input$y), fill=!!sym(fill_by_value))) +
        {if (input$facet_by!='None') facet_wrap(as.formula(paste('~', input$facet_by)), scales = 'free_x')} +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5),
        			strip.background = element_blank()) +
        scale_y_continuous(expand=c(0,0)) +
      	geom_bar(stat='identity', position=position_dodge(.9)) +
      	{if(input$y == 'Value' && 'Error' %in% numeric_columns()) geom_errorbar(aes(ymin=Value-Error, ymax=Value+Error), width=.2, position=position_dodge(.9))}
    })
  }
  )
}