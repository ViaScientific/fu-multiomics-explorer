#' User Interface for Metabolomic Tab
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the metabolomic data tab.
#' @importFrom shiny NS tagList fluidRow column
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput
#' @importFrom shiny selectizeInput conditionalPanel
#' @export
metabolomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		box(width=6, title="Metabolite Quantification", status='primary', solidHeader = TRUE,
				selectizeInput(ns("value"), "Selection:", choices=NULL),
				barplotUI(ns('barplot'))
		),
		box(width=6, title="Metabolite Comparison", status='primary', solidHeader = TRUE,
				fluidRow(
					column(6, selectizeInput(ns("x"), "X:", choices=NULL)),
					column(6, selectizeInput(ns("y"), "Y:", choices=NULL))
				),
				conditionalPanel("input.x != '' & input.y != ''",
												 scatterplotUI(ns('comparison')),
												 ns=ns
				)
		),
		box(width=6, title="Metadata", status='primary', solidHeader = TRUE,
				DTOutput(ns("metadata")) %>% withSpinner(image='www/spinner.gif')
		)
	)
}

#' Server Logic for Metabolomic Tab
#'
#' @param id A namespace ID for the module.
#' @return Reactive data containing metabolomic information.
#' @importFrom shiny moduleServer reactive observeEvent req
#' @importFrom dplyr %>% mutate select distinct arrange filter left_join rename across
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar theme_classic theme element_text ggtitle scale_y_continuous
#'     position_dodge
#' @importFrom DT renderDT datatable
#' @export
metabolomicTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load metadata
    metadata <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "clean", "metabolomic_metadata.txt", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        
        read.delim(file_path, header = TRUE, sep = '\t') %>%
          mutate(Donor = as.factor(Donor))
      }, error = function(e) {
        message("Error in loading metadata: ", e$message)
        NULL
      })
    })
    
    # Load main data
    data <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "clean", "B1.txt", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        
        read.delim(file_path, header = TRUE) %>%
          pivot_longer(cols = starts_with('D')) %>%
          mutate(Donor = substr(name, 2, 2)) %>%
          mutate(Type = substr(name, 4, 4)) %>%
          dplyr::select(Donor, Type, ionTopName, Value = value) %>%
          mutate(across(c(Donor, Type), factor)) %>%
          left_join(metadata(), by = 'Donor')
      }, error = function(e) {
        message("Error in loading data: ", e$message)
        NULL
      })
    })
    
    # Generate options for dropdown
    options <- reactive({
      req(data())
      tryCatch({
        (data() %>% arrange(ionTopName) %>% distinct(ionTopName))$ionTopName
      }, error = function(e) {
        message("Error in generating options: ", e$message)
        NULL
      })
    })
    
    # Update dropdowns
    observeEvent(options(), {
      updateSelectizeInput(session, 'value', "Selection:", choices = options(), selected = '', server = TRUE)
      updateSelectizeInput(session, 'x', "X:", choices = options(), selected = '', server = TRUE)
      updateSelectizeInput(session, 'y', "Y:", choices = options(), selected = '', server = TRUE)
    })
    
    # Filtered data for barplot
    filtered_data <- reactive({
      req(input$value, data())
      tryCatch({
        data() %>% filter(ionTopName == input$value)
      }, error = function(e) {
        message("Error in filtering data: ", e$message)
        NULL
      })
    })
    
    # Render barplot
    output$out <- renderPlot({
      req(input$value, filtered_data())
      tryCatch({
        ggplot(filtered_data(), aes(x = Donor, y = Value, fill = Type)) +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5)) +
          ggtitle(input$value) +
          scale_y_continuous(expand = c(0, 0)) +
          geom_bar(stat = 'identity', position = position_dodge(0.9))
      }, error = function(e) {
        message("Error in rendering barplot: ", e$message)
        NULL
      })
    })
    
    # Render metadata table
    output$metadata <- renderDT({
      req(metadata())
      tryCatch({
        datatable(metadata(),
                  selection = 'single',
                  rownames = FALSE,
                  colnames = c("Donor", "ID", "Source", "Age", "BMI", "Gender", "ComparisonID", "ComparisonID2"),
                  options = list(
                    dom = 't',
                    columnDefs = list(list(visible = FALSE, targets = c(6, 7)))
                  ))
      }, error = function(e) {
        message("Error in rendering metadata table: ", e$message)
        NULL
      })
    })
    
    # Generate comparison data
    comparison_data <- reactive({
      req(input$x, input$y, data())
      tryCatch({
        x <- data() %>% filter(ionTopName == input$x) %>% rename(!!input$x := Value) %>% select(-ionTopName)
        y <- data() %>% filter(ionTopName == input$y) %>% rename(!!input$y := Value) %>% select(-ionTopName)
        x %>% left_join(y, by = c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender"))
      }, error = function(e) {
        message("Error in generating comparison data: ", e$message)
        NULL
      })
    })
    
    # Call barplot module
    barplotServer('barplot', filtered_data, 'Type', 'None')
    
    # Call scatterplot module
    scatterplotServer('comparison', comparison_data, reactive(input$x), reactive(input$y))
    
    # Return the main data
    return(data)
  })
}