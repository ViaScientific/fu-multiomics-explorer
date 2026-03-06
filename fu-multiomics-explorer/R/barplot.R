barplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(min_height = '350px',
      card_header_with_download_and_settings(
        uiOutput(ns("card_title")),
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
          title = "Barplot Settings", bs_icon("gear"),
          selectInput(ns("x"), "X", choices=NULL),
          selectInput(ns("y"), "Y", choices=NULL),
          selectInput(ns("fill_by"), "Color By", choices=NULL),
          selectInput(ns("facet_by"), "Group By", choices=NULL)
        )
      ),
      card_body(
        selectizeInput(ns("value"), "Selection:", choices=NULL),
        plotOutput(ns("plot")) %>% withSpinner(image='spinner.gif'),
      )
    )
  )
}

barplotServer <- function(id, input_data, selection_column, title, default_fill, default_group) {
  
  moduleServer(id, function(input, output, session) {
    
    output$card_title <- renderUI({
      paste0(title, " Quantification")
    })
    
    options = reactive({
      input_data() %>% distinct(.data[[selection_column]]) %>% arrange(.data[[selection_column]]) %>% pull(.data[[selection_column]])
    })
    
    filtered_data = reactive({
      req(input$value)
      input_data() %>% filter(.data[[selection_column]] == input$value)
    })
    
    column_names = reactive({ colnames(input_data()) })
    
    column_classes = reactive({ sapply(input_data(), class) })
    
    numeric_columns = reactive({ (data.frame(Column=column_names(), Class=column_classes()) %>% filter(Class == 'numeric'))$Column })
   
    observeEvent(input_data(), {
      updateSelectizeInput(session, 'value', "Selection:", choices=options(), selected='', server=TRUE)
      updateSelectInput(session, 'x', "X", choices=column_names(), selected='Donor')
      updateSelectInput(session, 'y', "Y", choices=numeric_columns(), selected='Value')
      updateSelectInput(session, 'fill_by', "Color By", choices=c('None', column_names()), selected=default_fill)
      updateSelectInput(session, 'facet_by', "Group By", choices=c('None', column_names()), selected=default_group)
    })
    
    barplot = reactive({
    
    	req(input$x)
    	
      if (input$fill_by=='None') { fill_by_value = '' } else { fill_by_value=input$fill_by }
      
      ggplot(filtered_data(), aes(x=.data[[input$x]], y=.data[[input$y]], fill=.data[[fill_by_value]])) +
        {if (input$facet_by!='None') facet_wrap(as.formula(paste('~', input$facet_by)), scales = 'free_x')} +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5),
        			strip.background = element_blank()) +
        scale_y_continuous(expand=c(0,0)) +
      	geom_bar(stat='identity', position=position_dodge(.9)) +
      	{if(input$y == 'Value' && 'Error' %in% numeric_columns()) geom_errorbar(aes(ymin=Value-Error, ymax=Value+Error), width=.2, position=position_dodge(.9))}
    })
    
    output$plot = renderPlot({
      barplot()	
    })
    
    download_data = reactive({
      filtered_data() %>% select(any_of(BARPLOT_DOWNLOAD_COLUMNS))
    })
    
    ggplotDownloadPopoverServer('plot_download', barplot, "barplot")
    
    dfDownloadPopoverServer('data_download', download_data, "barplot_data")
    
  })
}