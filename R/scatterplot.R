scatterplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("out")) %>% withSpinner(image='spinner.gif'),
    h3("Figure Options"),
    bsCollapse(id = "figure_options", open = "",
    	bsCollapsePanel("Input Data", 
    	                column(6, selectInput(ns("x"), "X", choices=NULL)),
    	                column(6, selectInput(ns("y"), "Y", choices=NULL)),
    	                style = "info"),
    	bsCollapsePanel("Point Options",
    	                column(3, numericInput(ns("point_size"), "Point Size", value=3)),
    	                column(3, selectInput(ns("color_by"), "Color By", choices=NULL)),
    	                column(3, selectInput(ns("shape_by"), "Shape By", choices=NULL)),
    	                column(3, selectInput(ns("facet_by"), "Group By", choices=NULL)),
    	                style = "info"),
    	bsCollapsePanel("Regression",
    	                column(3, checkboxInput(ns("include_r2"), "Display r2", value = TRUE)),
    	                column(3, checkboxInput(ns("include_reg"), "Display Regression", value = TRUE)),
    	                style = "info")
    )
  )
}

scatterplotServer <- function(id, df, default_x, default_y) {
  
  moduleServer(id, function(input, output, session) {
    
    column_names = reactive({ colnames(df())})
    
    column_classes = reactive({ sapply(df(), class)})
    
    numeric_columns = reactive({ (data.frame(Column=column_names(), Class=column_classes()) %>% filter(Class == 'numeric'))$Column})
    
    observeEvent(df(), {
      updateSelectInput(session, 'x', "X", choices=numeric_columns(), selected=default_x())
      updateSelectInput(session, 'y', "Y", choices=numeric_columns(), selected=default_y())
      updateSelectInput(session, 'color_by', "Color By", choices=c('None', column_names()))
      updateSelectInput(session, 'shape_by', "Shape By", choices=c('None', column_names()))
      updateSelectInput(session, 'facet_by', "Group By", choices=c('None', column_names()))
    })
    
    regression = reactive({
      req(input$x)
      req(input$y)
      summary(lm(data=df(), formula = as.formula(paste("`", input$y, "`~`", input$x, "`", sep=''))))
    })
    
    output$out = renderPlot({

      regression()$r.squared
      
      if (input$color_by=='None') { color_by_value = '' } else { color_by_value= input$color_by }
      if (input$shape_by=='None') { shape_by_value = '' } else { shape_by_value= input$shape_by }
     
      ggplot(df(), aes(x=!!sym(input$x), y=!!sym(input$y), color=!!sym(color_by_value), shape=!!sym(shape_by_value))) +
        {if (input$facet_by!='None') facet_wrap(as.formula(paste('~', input$facet_by)))} +
        theme_classic() +
        theme(strip.background = element_blank()) +
        {if (input$include_reg) geom_abline(slope=regression()$coefficients[2], intercept=regression()$coefficients[1], linetype=2, color='grey')} +
        {if (input$include_r2) annotate(geom="text", x=-Inf, y=Inf, label=paste('r^2 == ', round(regression()$r.squared, 3)), vjust=1, hjust=-.1, parse=TRUE)} +
        geom_point(size=input$point_size)
    })
  }
  )
}