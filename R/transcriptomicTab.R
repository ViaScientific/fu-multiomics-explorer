transcriptomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		dashboardBody(
	
			box(width=6,
				title='UMAP', status='primary', solidHeader = TRUE,
				plotOutput(ns("umap_type")) %>% withSpinner(image='spinner.gif')
			),
			box(width=6,
				title="Differentially Expressed Genes by Cluster", status='primary', solidHeader = TRUE,
				selectizeInput(ns("cluster"), "Select Cluster:", choices=NULL),
				DTOutput(ns("cluster_DEG")) %>% withSpinner(image='spinner.gif')
			),
			box(width=12,
					title="Gene Counts per Cell", status='primary', solidHeader = TRUE,
					selectizeInput(ns("value"), "Select Gene:", choices=NULL),
					fluidRow(
						column(6,
									 plotOutput(ns("umap_gene")) %>% withSpinner(image='spinner.gif')
						),
						column(6,
									 plotOutput(ns("violin_gene")) %>% withSpinner(image='spinner.gif')
						)
					)
			)
		)
	)
}

transcriptomicTabServer <- function(id) {
	
	moduleServer(id, function(input, output, session) {
		
		gene_options = reactive({
			readRDS('data/clean/sc-rnaseq_features.rds')
		})
		
		observeEvent(gene_options(), {
		  print("C1")
		  updateSelectizeInput(session, 'value', "Gene:", choices=gene_options(), selected='INS', server=TRUE)
		  print("C2")
		})
		
		seurat_obj = reactive({
			progress <- Progress$new(session, min=0, max=1)
			on.exit(progress$close())
			
			progress$set(message = 'Reading input data',
									 detail = 'about 30 seconds')
			
			rds = readRDS('data/raw/seurat_integrated.rds')
			rds@meta.data = rds@meta.data %>% separate(SampleID, into=c("Donor", "Type"), sep='_') 
			return(rds)
		})
		
		cluster_annotations = reactive({
		  print("A")
			levels(Idents(seurat_obj()))
		})
		
		selected_cluster_gene = reactive({
		  print("J1")
		  req(cluster_DEG())
		  req(input$cluster_DEG_rows_selected)
		  print("J2")
		  (cluster_DEG() %>% slice(input$cluster_DEG_rows_selected))$Gene
		})
		
		observeEvent(selected_cluster_gene(), {
		  print("I1")
			updateSelectizeInput(session, 'value', "Gene:", choices=gene_options(), selected=selected_cluster_gene(), server=TRUE)
			print("I2")
		})
		
		observeEvent(cluster_annotations(), {
		  print('B1')
		  req(cluster_annotations())
		  print("B2")
		  print(cluster_annotations())
			updateSelectizeInput(session, 'cluster', "Cluster:", choices=cluster_annotations(), selected=cluster_annotations()[1], server=TRUE)
		})
		
		cluster_DEG = reactive({
		  print('D1')
		  req(input$cluster)
		  print("D2")
			read.delim(paste0('data/raw/DEG/organic/', input$cluster, '_2vs11_organic.csv'), sep=',', col.names = c("Gene", "p", "avg_log2FC", "pct.1", "pct.2", "adjusted_p"))
		})
		
		output$umap_type = renderPlot({
		  print("E1")
			DimPlot(seurat_obj(), reduction = "umap")
		})
		
		output$umap_gene = renderPlot({
		  print("F1")
		  req(input$value)
		  print("F2")
			FeaturePlot(seurat_obj(), features = c(input$value))
		})
		
		output$cluster_DEG = renderDT({
		  print("G1")
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
		  print("H1")
		  req(input$value)
		  print("H2")
			VlnPlot(seurat_obj(), features = c(input$value), split.by='Type', slot='counts', log=TRUE)
		})
	})
}