#' Launch the Multiomics Explorer Shiny App
#'
#' @description This function launches the Multiomics Explorer Shiny application, which provides tools for analyzing and visualizing multi-omics data.
#' @export
startApp <- function() {
  if (interactive()) {
    data_dir <- file.path(system.file("extdata", package = "fuMultiomicsExplorer"), "data")
    if (dir.exists(data_dir) && length(list.files(data_dir, recursive = TRUE)) > 0) {
      message("Data exists in: ", data_dir)

    # Set options for the app
    options(
      warn = -1,
      shiny.sanitize.errors = TRUE,
      shiny.host = "0.0.0.0",
      shiny.port = 8789,
      shiny.reactlog = TRUE
    )
    
    # Run the app
    shinyApp(ui = ui, server = server)
    } else {
      message("Make sure to run download_data() to be able ti use the fuMultiomicsExplorer!!!")
    }
  }
}