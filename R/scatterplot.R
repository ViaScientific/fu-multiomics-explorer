#' User Interface for Scatterplot
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the scatterplot visualization.
#' @importFrom shiny NS tagList selectInput numericInput checkboxInput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @export
scatterplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")) %>% withSpinner(image='www/spinner.gif'),
    downloadButton(ns('download')),
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
#' Server Logic for Scatterplot
#'
#' @param id A namespace ID for the module.
#' @param df A reactive data frame to be used for plotting.
#' @param default_x Default variable for the X-axis.
#' @param default_y Default variable for the Y-axis.
#' @return None; used to render the scatterplot and handle interactions.
#' @importFrom shiny moduleServer reactive observeEvent req
#' @importFrom dplyr filter mutate select
#' @importFrom ggplot2 ggplot aes geom_point geom_abline facet_wrap annotate theme_classic theme
#' @importFrom ggplot2 sym element_blank
#' @importFrom stats lm
#' @importFrom shiny downloadHandler renderPlot
#' @importFrom grDevices dev.off
#' @export
scatterplotServer <- function(id, df, default_x, default_y) {
  
  moduleServer(id, function(input, output, session) {
    
    column_names <- reactive({ colnames(df()) })
    
    numeric_columns <- reactive({
      column_classes <- sapply(df(), class)
      (data.frame(Column = names(column_classes), Class = column_classes) %>% 
          filter(Class == "numeric"))$Column
    })
    
    observeEvent(df(), {
      updateSelectInput(session, 'x', "X", choices = numeric_columns(), selected = default_x())
      updateSelectInput(session, 'y', "Y", choices = numeric_columns(), selected = default_y())
      updateSelectInput(session, 'color_by', "Color By", choices = c('None', column_names()))
      updateSelectInput(session, 'shape_by', "Shape By", choices = c('None', column_names()))
      updateSelectInput(session, 'facet_by', "Group By", choices = c('None', column_names()))
    })
    
    validate_columns <- reactive({
      req(input$x, input$y)
      if (!(input$x %in% column_names())) {
        stop(paste("Column", input$x, "is not available in the data."))
      }
      if (!(input$y %in% column_names())) {
        stop(paste("Column", input$y, "is not available in the data."))
      }
    })
    
    regression <- reactive({
      validate_columns()
      summary(lm(data = df(), formula = as.formula(paste("`", input$y, "` ~ `", input$x, "`", sep = ""))))
    })
    
    scatter_plot <- reactive({
      validate_columns()
      
      color_by_value <- if (input$color_by == 'None') '' else input$color_by
      shape_by_value <- if (input$shape_by == 'None') '' else input$shape_by
      
      ggplot(df(), aes(x = !!sym(input$x), y = !!sym(input$y), 
                       color = !!sym(color_by_value), shape = !!sym(shape_by_value))) +
        {if (input$facet_by != 'None') facet_wrap(as.formula(paste('~', input$facet_by)))} +
        theme_classic() +
        theme(strip.background = element_blank()) +
        {if (input$include_reg) geom_abline(slope = regression()$coefficients[2], intercept = regression()$coefficients[1], 
                                            linetype = 2, color = 'grey')} +
        {if (input$include_r2) annotate(geom = "text", x = -Inf, y = Inf, 
                                        label = paste('r^2 == ', round(regression()$r.squared, 3)), 
                                        vjust = 1, hjust = -0.1, parse = TRUE)} +
        geom_point(size = input$point_size)
    })
    
    output$plot <- renderPlot({
      scatter_plot()
    })
    
    output$download <- downloadHandler(
      filename = function() { 'scatter_plot.pdf' },
      content = function(file) {
        pdf(file = file)
        plot(scatter_plot())
        dev.off()
      }
    )
    
  })
}