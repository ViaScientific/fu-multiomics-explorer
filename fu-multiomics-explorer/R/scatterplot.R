scatterplotUI <- function(id) {
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
          column(6, selectInput(ns("x"), "X", choices=NULL)),
          column(6, selectInput(ns("y"), "Y", choices=NULL))
        ),
        plotOutput(ns("plot")) %>% withSpinner(image='spinner.gif')
      )
    )
  )
}

scatterplotServer <- function(id, df, selection_column, title) {
  
  moduleServer(id, function(input, output, session) {
    
    output$card_title <- renderUI({
      paste0(title, " Comparison")
    })
    
    options = reactive({
      df() %>% distinct(.data[[selection_column]]) %>% arrange(.data[[selection_column]]) %>% pull(.data[[selection_column]])
    })
    
    observeEvent(options(), {
      updateSelectizeInput(session, 'x', "X:", choices=options(), selected=options()[1], server=TRUE)
      updateSelectizeInput(session, 'y', "Y:", choices=options(), selected=options()[2], server=TRUE)
    })
    
    valid_xy <- reactive({
      req(input$x, input$y)
      !identical(input$x, input$y)
    })
    
    comparison_data = reactive({
      req(valid_xy())
      x = df() %>% filter(.data[[selection_column]] == input$x) %>% rename(!!input$x := Value) %>% select(-any_of(c(!!selection_column, 'Error', 'ComparisonID')))
      y = df() %>% filter(.data[[selection_column]] == input$y) %>% rename(!!input$y := Value) %>% select(-any_of(c(!!selection_column, 'Error', 'ComparisonID')))
      combined = x %>% left_join(y, by=c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender"))
    })
    
    column_names = reactive({ colnames(df())})
    
    observeEvent(df(), {
      updateSelectInput(session, 'color_by', "Color By", choices=c('None', column_names()))
      updateSelectInput(session, 'shape_by', "Shape By", choices=c('None', column_names()))
      updateSelectInput(session, 'facet_by', "Group By", choices=c('None', column_names()))
    })
    
    regression = reactive({
      req(valid_xy())
      summary(lm(data=comparison_data(), formula = as.formula(paste("`", input$y, "`~`", input$x, "`", sep=''))))
    })
    
    scatter_plot = reactive({
      
      if (input$color_by=='None') { color_by_value = '' } else { color_by_value= input$color_by }
      if (input$shape_by=='None') { shape_by_value = '' } else { shape_by_value= input$shape_by }
      
      ggplot(comparison_data(), aes(x=!!sym(input$x), y=!!sym(input$y), color=!!sym(color_by_value), shape=!!sym(shape_by_value))) +
        {if (input$facet_by!='None') facet_wrap(as.formula(paste('~', input$facet_by)))} +
        theme_classic() +
        theme(strip.background = element_blank()) +
        {if (input$include_reg) geom_abline(slope=regression()$coefficients[2], intercept=regression()$coefficients[1], linetype=2, color='grey')} +
        {if (input$include_r2) annotate(geom="text", x=-Inf, y=Inf, label=paste('r^2 == ', round(regression()$r.squared, 3)), vjust=1, hjust=-.1, parse=TRUE)} +
        geom_point(size=input$point_size)
    })
    
    output$plot <- renderPlot({
      req(input$x, input$y)
      
      if (identical(input$x, input$y)) {
        plot.new()
        text(0.5, 0.5, "Please choose different values for X and Y.")
        return(invisible())
      }
      scatter_plot()
    })
    
    download_data = reactive({
      comparison_data() %>% select(any_of(SCATTERPLOT_DOWNLOAD_COLUMNS), !!input$x, !!input$y)
    })
    
    ggplotDownloadPopoverServer('plot_download', scatter_plot, "scatter_plot")
    dfDownloadPopoverServer('data_download', download_data, "scatter_plot_data")
    
  })
}