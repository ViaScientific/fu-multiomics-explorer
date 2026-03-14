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
              ggplotDownloadPopoverUI(ns('plot_download'))
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
        plotOutput(ns("plot")) %>% withSpinner(image='spinner.gif')
      )
    )
  )
}

barplotServer <- function(id, input_data, selection_column, title, y_label, default_fill, default_group) {
  
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
    
    quant_columns = reactive({
      cnames = colnames(input_data())
      return(cnames[cnames %in% QUANTITATIVE_COLUMNS])
    })
    
    observeEvent(input_data(), {
      updateSelectizeInput(session, 'value', "Selection:", choices=options(), selected=options()[1], server=TRUE)
      updateSelectInput(session, 'x', "X", choices=GROUPING_COLUMNS, selected='Donor')
      updateSelectInput(session, 'y', "Y", choices=quant_columns(), selected=y_label)
      updateSelectInput(session, 'fill_by', "Color By", choices=c("None", GROUPING_COLUMNS), selected=default_fill)
      updateSelectInput(session, 'facet_by', "Group By", choices=c("None", GROUPING_COLUMNS), selected=default_group)
    })
    
    barplot = reactive({
    
    	req(input$x)
    	
      if (input$x != 'Donor' & input$fill_by != 'Donor' & input$facet_by != 'Donor') {
        plot.new()
        text(0.5, 0.5, "Donor must selected for at least one of [X, Color By, or Group By]")
        return(invisible())
      }
      
      if (input$x != 'Treatment' & input$fill_by != 'Treatment' & input$facet_by != 'Treatment') {
        plot.new()
        text(0.5, 0.5, "Treatment must be selected for at least one of [X, Color By, or Group By]")
        return(invisible())
      }
      
      if (input$fill_by=='None') { fill_by_value = '' } else { fill_by_value=input$fill_by }
      
      ggplot(filtered_data(), aes(x=.data[[input$x]], y=.data[[input$y]], fill=!!sym(fill_by_value))) +
        {if (input$facet_by!='None') facet_wrap(as.formula(paste('~', input$facet_by)), scales = 'free_x')} +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5),
        			strip.background = element_blank()) +
        scale_y_continuous(expand=c(0,0), name = y_label) +
      	geom_bar(stat='identity', position=position_dodge(.9)) +
      	{if(input$y == y_label && y_label == 'MS Signal Intensity') geom_errorbar(aes(ymin=!!sym(y_label) - Error, ymax=!!sym(y_label) + Error), width=.2, position=position_dodge(.9))}
    })
    
    output$plot = renderPlot({
      barplot()	
    })
    
    download_data = reactive({
      filtered_data() %>% 
        select(any_of(BARPLOT_DOWNLOAD_COLUMNS))
    })
    
    ggplotDownloadPopoverServer('plot_download', barplot, "barplot")
    
    dfDownloadPopoverServer('data_download', download_data, "barplot_data")
    
  })
}