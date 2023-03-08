barplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("out")),
    column(3, selectInput(ns("x"), "X", choices=NULL)),
    column(3, selectInput(ns("y"), "Y", choices=NULL)),
    column(3, selectInput(ns("fill_by"), "Color By", choices=NULL)),
    column(3, selectInput(ns("facet_by"), "Group By", choices=NULL))
  )
}

barplotServer <- function(id, df) {
  
  moduleServer(id, function(input, output, session) {
    
    column_names = reactive({ colnames(df()) })
    
    column_classes = reactive({ sapply(df(), class) })
    
    numeric_columns = reactive({ (data.frame(Column=column_names(), Class=column_classes()) %>% filter(Class == 'numeric'))$Column })
    
    observeEvent(df(), {
      updateSelectInput(session, 'x', "X", choices=column_names())
      updateSelectInput(session, 'y', "Y", choices=numeric_columns())
      updateSelectInput(session, 'fill_by', "Color By", choices=c('None', column_names()))
      updateSelectInput(session, 'facet_by', "Group By", choices=c('None', column_names()))
    })
    
    output$out = renderPlot({
      
      if (input$fill_by=='None') { fill_by_value = NULL } else { fill_by_value=input$fill_by }
      
      ggplot(df(), aes_string(x=input$x, y=input$y, fill=fill_by_value)) +
        {if (input$facet_by!='None') facet_wrap(as.formula(paste('~', input$facet_by)))} +
        theme_classic() +
        theme(strip.background = element_blank()) +
        scale_y_continuous(expand=c(0,0)) +
        geom_bar(stat='identity', position='dodge')
    })
  }
  )
}