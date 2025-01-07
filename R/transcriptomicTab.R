#' User Interface for Transcriptomic Tab
#'
#' @param id A namespace ID for the module.
#' @return A UI definition for the transcriptomic data tab.
#' @importFrom shiny NS tagList fluidRow column
#' @importFrom shinydashboard dashboardBody box
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#' @export
transcriptomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		dashboardBody(
			box(width=6,
				title='UMAP', status='primary', solidHeader = TRUE,
				plotOutput(ns("umap_cluster")) %>% withSpinner(image='www/spinner.gif'),
				downloadButton(ns('download_umap_cluster')),
			),
			box(width=6,
				title="Differentially Expressed Genes by Cluster", status='primary', solidHeader = TRUE,
				selectizeInput(ns("cluster"), "Select Cluster:", choices=NULL),
				DTOutput(ns("cluster_DEG")) %>% withSpinner(image='www/spinner.gif')
			),
			box(width=12,
				title="Gene Counts per Cell", status='primary', solidHeader = TRUE,
				selectizeInput(ns("value"), "Select Gene:", choices=NULL),
				fluidRow(
					column(6,
								 plotOutput(ns("umap_count")) %>% withSpinner(image='www/spinner.gif'),
								 downloadButton(ns('download_umap_count')),
					),
					column(6,
								 plotOutput(ns("violin")) %>% withSpinner(image='www/spinner.gif'),
								 downloadButton(ns('download_violin'))
					)
				)
			),
			box(width=6, title="Metadata", status='primary', solidHeader = TRUE,
					DTOutput(ns("metadata")) %>% withSpinner(image='www/spinner.gif')
			)
		)
	)
}

#' Server Logic for Transcriptomic Tab
#'
#' @param id A namespace ID for the module.
#' @return Reactive values containing transcriptomic data.
#' @importFrom shiny moduleServer reactive observeEvent renderPlot downloadHandler req
#' @importFrom shiny Progress
#' @importFrom dplyr %>% mutate left_join rename case_when slice
#' @importFrom tidyr separate pivot_longer
#' @importFrom Seurat Idents DimPlot FeaturePlot VlnPlot
#' @importFrom utils read.delim
#' @importFrom DT renderDT datatable formatSignif formatRound
#' @importFrom shinycssloaders withSpinner 
#' @importFrom grDevices dev.off
#' @export
transcriptomicTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive for loading gene options
    gene_options <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "clean", "sc-rnaseq_features.rds", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        readRDS(file_path)
      }, error = function(e) {
        message("Error in loading gene options: ", e$message)
        NULL
      })
    })
    
    # Update gene selection input
    observeEvent(gene_options(), {
      updateSelectizeInput(session, 'value', "Gene:", choices = gene_options(), selected = 'INS', server = TRUE)
    })
    
    # Reactive for loading Seurat object
    seurat_obj <- reactive({
      req(input$value)  # Ensure input$value is selected
      tryCatch({
        progress <- Progress$new(session, min = 0, max = 1)
        on.exit(progress$close())
        progress$set(message = 'Reading input data', detail = 'about 30 seconds')
        
        file_path <- system.file("extdata", "data", "raw", "seurat_integrated.rds", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        
        rds <- readRDS(file_path)
        rds@meta.data <- rds@meta.data %>% separate(SampleID, into = c("Donor", "Type"), sep = '_')
        rds
      }, error = function(e) {
        message("Error in loading Seurat object: ", e$message)
        NULL
      })
    })
    
    # Reactive for cluster annotations
    cluster_annotations <- reactive({
      req(seurat_obj())
      tryCatch({
        levels(Idents(seurat_obj()))
      }, error = function(e) {
        message("Error in extracting cluster annotations: ", e$message)
        NULL
      })
    })
    
    # Update cluster selection input
    observeEvent(cluster_annotations(), {
      updateSelectizeInput(session, 'cluster', "Cluster:", choices = cluster_annotations(), selected = cluster_annotations()[1], server = TRUE)
    })
    
    # Reactive for loading cluster DEG data
    cluster_DEG <- reactive({
      req(input$cluster)
      tryCatch({
        file_path <- system.file("extdata", "data", "raw", "DEG", "organic", paste0(input$cluster, '_2vs11_organic.csv'), package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        read.delim(file_path, sep = ',', col.names = c("Gene", "p", "avg_log2FC", "pct.1", "pct.2", "adjusted_p"))
      }, error = function(e) {
        message("Error in loading cluster DEG data: ", e$message)
        NULL
      })
    })
    
    # UMAP cluster plot
    umap_cluster <- reactive({
      req(seurat_obj())
      tryCatch({
        DimPlot(seurat_obj(), reduction = "umap")
      }, error = function(e) {
        message("Error in generating UMAP cluster plot: ", e$message)
        NULL
      })
    })
    
    # Render UMAP cluster plot
    output$umap_cluster <- renderPlot({
      umap_cluster()
    })
    
    # Download UMAP cluster plot
    output$download_umap_cluster <- downloadHandler(
      filename = function() { 'umap_by_cluster.pdf' },
      content = function(file) {
        tryCatch({
          pdf(file = file)
          plot(umap_cluster())
          dev.off()
        }, error = function(e) {
          message("Error in downloading UMAP cluster plot: ", e$message)
        })
      }
    )
    
    # UMAP count plot
    umap_count <- reactive({
      req(input$value)
      tryCatch({
        FeaturePlot(seurat_obj(), features = c(input$value))
      }, error = function(e) {
        message("Error in generating UMAP count plot: ", e$message)
        NULL
      })
    })
    
    # Render UMAP count plot
    output$umap_count <- renderPlot({
      umap_count()
    })
    
    # Download UMAP count plot
    output$download_umap_count <- downloadHandler(
      filename = function() { 'umap_by_count.pdf' },
      content = function(file) {
        tryCatch({
          pdf(file = file)
          plot(umap_count())
          dev.off()
        }, error = function(e) {
          message("Error in downloading UMAP count plot: ", e$message)
        })
      }
    )
    
    # Render cluster DEG table
    output$cluster_DEG <- renderDT({
      req(input$cluster)
      tryCatch({
        datatable(cluster_DEG(),
                  selection = 'single',
                  options = list(
                    columnDefs = list(list(visible = FALSE, targets = c(1))),
                    pageLength = 5
                  ),
                  rownames = FALSE,
                  colnames = c("Gene", "p", "Fold Change (log2)", "pct.1", "pct.2", "Adjusted p")) %>%
          formatRound(columns = c('avg_log2FC'), digits = 4) %>%
          formatSignif(columns = c('adjusted_p'), digits = 4)
      }, error = function(e) {
        message("Error in rendering cluster DEG table: ", e$message)
        NULL
      })
    })
    
    # Violin plot
    violin <- reactive({
      req(input$value)
      tryCatch({
        VlnPlot(seurat_obj(), features = c(input$value), split.by = 'Type', slot = 'counts', log = TRUE, split.plot = TRUE)
      }, error = function(e) {
        message("Error in generating violin plot: ", e$message)
        NULL
      })
    })
    
    # Render violin plot
    output$violin <- renderPlot({
      violin()
    })
    
    # Download violin plot
    output$download_violin <- downloadHandler(
      filename = function() { 'violin_plot.pdf' },
      content = function(file) {
        tryCatch({
          pdf(file = file)
          plot(violin())
          dev.off()
        }, error = function(e) {
          message("Error in downloading violin plot: ", e$message)
        })
      }
    )
    
    # Metadata
    metadata <- reactive({
      tryCatch({
        file_path <- system.file("extdata", "data", "clean", "transcriptomic_metadata.txt", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        read.delim(file_path, sep = '\t', header = TRUE)
      }, error = function(e) {
        message("Error in loading metadata: ", e$message)
        NULL
      })
    })
    
    # Render metadata table
    output$metadata <- renderDT({
      tryCatch({
        datatable(metadata(),
                  selection = 'single',
                  colnames = c("Donor", "ID", "Source", "Age", "BMI", "Gender", "Comparison", "Comparison2"),
                  rownames = FALSE,
                  options = list(
                    columnDefs = list(list(visible = FALSE, targets = c(6, 7))),
                    dom = 't'))
      }, error = function(e) {
        message("Error in rendering metadata table: ", e$message)
        NULL
      })
    })
    
    # Reactive for beta data
    beta_data <- reactive({
      req(metadata())
      tryCatch({
        file_path <- system.file("extdata", "data", "raw", "RNA_Count_by_donor_and_samples_beta_cells.csv", package = "fuMultiomicsExplorer")
        if (file_path == "" || !file.exists(file_path)) stop("File not found: ", file_path)
        read.delim(file_path, sep = ',') %>%
          pivot_longer(!gene, names_to = 'group', values_to = 'Value') %>%
          separate(group, into = c("ID", "Type")) %>%
          mutate(Type = case_when(Type == 'G11' ~ "H", Type == 'G2' ~ 'L')) %>%
          rename(Gene = gene) %>%
          left_join(metadata(), by = 'ID')
      }, error = function(e) {
        message("Error in loading beta data: ", e$message)
        NULL
      })
    })
    
    return(beta_data)
  })
}