transcriptomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
	  div(
      layout_columns(
        card(min_height = '350px',
          card_header_with_download_and_settings(
            "UMAP",
            popover(
              title = "Download Options", bs_icon("download"),
              ggplotDownloadPopoverUI(ns('umap_cluster_download'))
            )
          ),
          card_body(
            plotOutput(ns("umap_cluster")) %>% withSpinner(image='spinner.gif'),
          )
        ),
        card(min_height = '350px',
          card_header_with_download_and_settings(
            "Differentially Expressed Genes by Cluster",
            popover(
              title = "Download Options", bs_icon("download"),
              dfDownloadPopoverUI(ns('marker_gene_df_download'))
            )
          ),
          card_body(
            selectizeInput(ns("cluster"), "Select Cluster:", choices=NULL),
            DTOutput(ns("cluster_DEG")) %>% withSpinner(image='spinner.gif')
          )
        ),
        col_widths = c(6,6),
        gap = "0.5rem"
      )
	  ),
	  div(
      layout_columns(
	      card(height = '550px',
	        card_header_with_download_and_settings(
	          "Gene Counts per Cell",
	          popover(
	            title = "Download Options", bs_icon("download"),
	            accordion(
	              id = ns("download_acc"),
	              class = "acc--dense",
	              open = TRUE,
	              accordion_panel(
	                "UMAP",
	                ggplotDownloadPopoverUI(ns('umap_count_download'))
	              ),
	              accordion_panel(
	                "Boxplots",
	                ggplotDownloadPopoverUI(ns('violin_download'))
	              )
	            )
	          )
	        ),
	        card_body(
	          selectizeInput(ns("value"), "Select Gene:", choices=NULL),
	          layout_columns(
              plotOutput(ns("umap_count")) %>% withSpinner(image='spinner.gif'),
              plotOutput(ns("violin")) %>% withSpinner(image='spinner.gif')
	          )
	        )
	      ),
	      metadataTableUI(ns('metadata')),
	      col_widths = c(8,4)
      )
	  )
	)
}

transcriptomicTabServer <- function(id) {
	
	moduleServer(id, function(input, output, session) {
		
		gene_options = reactive({
			readRDS(clean_data_path('sc-rnaseq_features.rds'))
		})
		
		observeEvent(gene_options(), {
		  updateSelectizeInput(session, 'value', "Gene:", choices=gene_options(), selected='INS', server=TRUE)
		})
		
		seurat_obj = reactive({
			progress <- Progress$new(session, min=0, max=1)
			on.exit(progress$close())
			
			progress$set(message = 'Reading input data',
									 detail = 'about 30 seconds')
			
			rds = readRDS(raw_data_path('seurat_integrated.rds'))
			rds@meta.data = rds@meta.data %>% separate(SampleID, into=c("Donor", "Type"), sep='_') 
			return(rds)
		})
		
		cluster_annotations = reactive({
			levels(Idents(seurat_obj()))
		})
		
		selected_cluster_gene = reactive({
		  req(cluster_DEG())
		  req(input$cluster_DEG_rows_selected)
		  (cluster_DEG() %>% slice(input$cluster_DEG_rows_selected))$Gene
		})
		
		observeEvent(selected_cluster_gene(), {
			updateSelectizeInput(session, 'value', "Select Gene:", choices=gene_options(), selected=selected_cluster_gene(), server=TRUE)
		})
		
		observeEvent(cluster_annotations(), {
		  req(cluster_annotations())
			updateSelectizeInput(session, 'cluster', "Select Cluster:", choices=cluster_annotations(), selected=cluster_annotations()[1], server=TRUE)
		})
		
		cluster_DEG = reactive({
		  req(input$cluster)
		  read.delim(cluster_path(paste0(input$cluster, '_2vs11_organic.csv')), sep=',', col.names = c("Gene", "p", "avg_log2FC", "pct.1", "pct.2", "adjusted_p"))
		})
		
		umap_cluster = reactive({
			DimPlot(seurat_obj(), reduction = "umap")
		})
		
		output$umap_cluster = renderPlot({
			umap_cluster()	
		})
		
		ggplotDownloadPopoverServer('umap_cluster_download', umap_cluster, "umap_by_cluster")
		
		umap_count = reactive({
		  req(input$value)
			FeaturePlot(seurat_obj(), features = c(input$value))
		})
		
		output$umap_count = renderPlot({
			umap_count()	
		})
		
		ggplotDownloadHandlerServer('umap_count_download', umap_count, "umap_by_count")
		
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
		
		dfDownloadHandlerServer('marker_gene_df_download', cluster_DEG, paste0(input$cluster, "_marker_genes"))
		
		violin = reactive({
		  req(input$value)
			VlnPlot(seurat_obj(), features = c(input$value), split.by='Type', slot='counts', log=TRUE)
		})
		
		output$violin = renderPlot({
			violin()	
		})
		
		ggplotDownloadHandlerServer('violin_download', violin, "boxplots_plot")
		
		metadata = reactive({
			read.delim(clean_data_path('transcriptomic_metadata.txt'), sep='\t', header=TRUE)
		})
		
		metadataTableServer('metadata', metadata)
		
		beta_data = reactive({
			read.delim(raw_data_path('RNA_Count_by_donor_and_samples_beta_cells.csv'), sep=',') %>%
			pivot_longer(!gene, names_to='group', values_to='Value') %>%
			separate(group, into=c("ID", "Type")) %>%
			mutate(Type = case_when(Type == 'G11' ~ "H",
															Type == 'G2' ~ 'L'
			)) %>% 
			rename(Gene=gene) %>%
			left_join(metadata(), by='ID')
		})
		
		return(beta_data)
		
	})
}