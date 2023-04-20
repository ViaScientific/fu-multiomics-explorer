geneSearchTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
      	fluidRow(
      		h3("UMAP", align='center')
      	),
      	fluidRow(style = 'border-style: groove;',
        	plotOutput(ns("umap_type")) %>% withSpinner()
      	)
      ),
      column(6, 
      	fluidRow(
      		h3("Differentially Expressed Genes by Cluster", align='center'),
      	),	
      	fluidRow(style = 'border-style: groove;',
      		selectizeInput(ns("cluster"), "Select Cluster:", choices=NULL),
        	DTOutput(ns("cluster_DEG")) %>% withSpinner()
      	)
      )
    ),
    fluidRow(
    	h3("Gene Counts per Cell", align='center')
    ),
    fluidRow(style = 'border-style: groove;',
    	fluidRow(
    		column(11, offset=.5,
    			selectizeInput(ns("value"), "Select Gene:", choices=NULL)
    		)
    	),
    	fluidRow(
    		column(6,
    	    plotOutput(ns("umap_gene")) %>% withSpinner()
    	  ),
    	  column(6,
    	    plotOutput(ns("violin_gene")) %>% withSpinner()
    	  )
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
    
    cluster_DEG = reactive({
      read.delim(paste0('data/raw/DEG/organic/', input$cluster, '_2vs11_organic.csv'), sep=',', col.names = c("Gene", "p", "avg_log2FC", "pct.1", "pct.2", "adjusted_p"))
    })
    
    output$umap_type = renderPlot({
      DimPlot(seurat_obj(), reduction = "umap")
    })
    
    output$umap_gene = renderPlot({
      FeaturePlot(seurat_obj(), features = c(input$value), split.by = 'Type') & theme(legend.position = c(0.1,0.2))
    })
    
    output$cluster_DEG = renderDT({
      datatable(cluster_DEG(), 
                selection='single',
                options=list(
                  columnDefs = list(list(visible=FALSE, targets=c(1))),
                  pageLength = 5
                ),
                rownames=FALSE,
                colnames=c("Gene", "p", "Fold Change (log2)", "pct.1", "pct.2", "Adjusted p")
      ) %>% 
      formatRound(columns=c('avg_log2FC'), digits=4) %>%
      formatSignif(columns = c('adjusted_p'), digits = 4)
    })
    
    output$violin_gene = renderPlot({
      VlnPlot(seurat_obj(), features = c(input$value), split.by='Type', slot='counts', log=TRUE)
    })
    
  }
  )
}