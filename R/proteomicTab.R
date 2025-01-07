#' User Interface for Proteomic Tab
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the proteomic data tab.
#' @importFrom shiny NS tagList fluidRow column
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput
#' @importFrom shiny selectizeInput conditionalPanel
#' @export
proteomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		box(width=6, title="Protein Quantification", status='primary', solidHeader = TRUE,
				selectizeInput(ns("value"), "Selection:", choices=NULL),
				barplotUI(ns('barplot'))
		),
		box(width=6, title="Protein Comparison", status='primary', solidHeader = TRUE,
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
#' Server Logic for Proteomic Tab
#'
#' @param id A namespace ID for the module.
#' @return Reactive data containing processed proteomic data.
#' @importFrom shiny moduleServer reactive observeEvent req
#' @importFrom dplyr %>% mutate filter group_by summarise left_join arrange distinct rename across
#' @importFrom DT renderDT datatable
#' @importFrom stats sd
#' @export
proteomicTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive for metadata
    metadata <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "clean", "proteomic_metadata.txt", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        
        read.delim(file_path, header = TRUE, sep = '\t') %>%
          mutate(Donor = as.factor(Donor))
      }, error = function(e) {
        message("Error in loading metadata: ", e$message)
        NULL
      })
    })
    
    # Reactive for main data
    data <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "processed", "A1_processed.txt", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        
        read.delim(file_path, header = TRUE, sep = '\t') %>%
          mutate(across(c(Donor, Replicate, Type), factor)) %>%
          group_by(Protein, Donor, Type) %>%
          summarise(Error = sd(Value), Value = mean(Value), .groups = 'drop') %>%
          filter(Protein != '') %>%
          left_join(metadata(), by = 'Donor')
      }, error = function(e) {
        message("Error in loading data: ", e$message)
        NULL
      })
    })
    
    # Reactive for dropdown options
    options <- reactive({
      req(data())
      tryCatch({
        (data() %>% arrange(Protein) %>% distinct(Protein))$Protein
      }, error = function(e) {
        message("Error in generating options: ", e$message)
        NULL
      })
    })
    
    # Update dropdown inputs
    observeEvent(options(), {
      updateSelectizeInput(session, 'value', "Selection:", choices = options(), selected = '', server = TRUE)
      updateSelectizeInput(session, 'x', "X:", choices = options(), selected = '', server = TRUE)
      updateSelectizeInput(session, 'y', "Y:", choices = options(), selected = '', server = TRUE)
    })
    
    # Filtered data for barplot
    filtered_data <- reactive({
      req(input$value, data())
      tryCatch({
        data() %>% filter(Protein == input$value)
      }, error = function(e) {
        message("Error in filtering data: ", e$message)
        NULL
      })
    })
    
    # Render metadata table
    output$metadata <- renderDT({
      req(metadata())
      tryCatch({
        datatable(metadata(),
                  selection = 'single',
                  colnames = c("Donor", "ID", "Source", "Age", "BMI", "Gender", "Comparison"),
                  rownames = FALSE,
                  options = list(
                    columnDefs = list(list(visible = FALSE, targets = c(6))),
                    dom = 't'))
      }, error = function(e) {
        message("Error in rendering metadata table: ", e$message)
        NULL
      })
    })
    
    # Reactive for comparison data
    comparison_data <- reactive({
      req(input$x, input$y, data())
      tryCatch({
        x <- data() %>% filter(Protein == input$x) %>% rename(!!input$x := Value) %>% select(-Protein, -Error)
        y <- data() %>% filter(Protein == input$y) %>% rename(!!input$y := Value) %>% select(-Protein, -Error)
        x %>% left_join(y, by = c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender"))
      }, error = function(e) {
        message("Error in generating comparison data: ", e$message)
        NULL
      })
    })
    
    # Call barplot module
    barplotServer('barplot', filtered_data, "Type", "Gender")
    
    # Call scatterplot module
    scatterplotServer('comparison', comparison_data, reactive(input$x), reactive(input$y))
    
    # Return the main data
    return(data)
  })
}