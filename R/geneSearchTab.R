geneSearchTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
        textOutput(ns("test")),
        plotOutput(ns("umap_type")),
        selectizeInput(ns("cluster"), "Select Cluster:", choices=NULL),
      ),
      column(6,
        DTOutput(ns("cluster_DEG"))
      )
    ),
    fluidRow(
      column(6,
        plotOutput(ns("umap_gene")),
        selectizeInput(ns("value"), "Select Gene:", choices=NULL)
      ),
      column(6,
        plotOutput(ns("violin_gene"))
      )
    )
  )
}

geneSearchTabServer <- function(id, gene_options, seurat_obj) {
  
  moduleServer(id, function(input, output, session) {
    
    cluster_annotations = reactive({
      levels(Idents(seurat_obj()))
    })
    
    observeEvent(gene_options(), {
      updateSelectizeInput(session, 'value', "Gene:", choices=gene_options(), selected='INS', server=TRUE)
    })
    
    observeEvent(selected_cluster_gene(), {
      updateSelectizeInput(session, 'value', "Gene:", choices=gene_options(), selected=selected_cluster_gene(), server=TRUE)
    })

    observeEvent(cluster_annotations(), {
      updateSelectizeInput(session, 'cluster', "Cluster:", choices=cluster_annotations(), server=TRUE)
    })
    
    selected_cluster_gene = reactive({
      req(input$cluster_DEG_rows_selected)
      (cluster_DEG() %>% slice(input$cluster_DEG_rows_selected))$Gene
    })
    
    output$test = renderText({
      selected_cluster_gene()
    })
    
    cluster_DEG = reactive({
      read.delim(paste0('data/raw/DEG/', input$cluster, '_2vs11_organic.csv'), sep=',', col.names = c("Gene", "p", "avg_log2FC", "pct.1", "pct.2", "adjusted_p"))
    })
    
    output$umap_type = renderPlot({
      DimPlot(seurat_obj(), reduction = "umap")
    })
    
    output$umap_gene = renderPlot({
      FeaturePlot(seurat_obj(), features = c(input$value))
    })
    
    output$cluster_DEG = renderDT({
      datatable(cluster_DEG(), 
                selection='single',
                options=list(
                  columnDefs = list(list(visible=FALSE, targets=c(1)))
                ),
                rownames=FALSE,
                colnames=c("Gene", "p", "Fold Change (log2)", "pct.1", "pct.2", "Adjusted p")
      ) %>% 
      formatRound(columns=c('avg_log2FC'), digits=4) %>%
      formatSignif(columns = c('adjusted_p'), digits = 4)
    })
    
    output$violin_gene = renderPlot({
      VlnPlot(seurat_obj(), features = c(input$value), slot='counts', log=TRUE)
    })
    
  }
  )
}