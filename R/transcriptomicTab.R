transcriptomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		dashboardBody(
	
			box(width=6,
				title='UMAP', status='primary', solidHeader = TRUE,
				plotOutput(ns("umap_cluster")) %>% withSpinner(image='spinner.gif'),
				downloadButton(ns('download_umap_cluster')),
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
								 plotOutput(ns("umap_count")) %>% withSpinner(image='spinner.gif'),
								 downloadButton(ns('download_umap_count')),
					),
					column(6,
								 plotOutput(ns("violin")) %>% withSpinner(image='spinner.gif'),
								 downloadButton(ns('download_violin'))
					)
				)
			),
			box(width=6, title="Metadata", status='primary', solidHeader = TRUE,
					DTOutput(ns("metadata")) %>% withSpinner(image='spinner.gif')
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
		  updateSelectizeInput(session, 'value', "Gene:", choices=gene_options(), selected='INS', server=TRUE)
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
			levels(Idents(seurat_obj()))
		})
		
		selected_cluster_gene = reactive({
		  req(cluster_DEG())
		  req(input$cluster_DEG_rows_selected)
		  (cluster_DEG() %>% slice(input$cluster_DEG_rows_selected))$Gene
		})
		
		observeEvent(selected_cluster_gene(), {
			updateSelectizeInput(session, 'value', "Gene:", choices=gene_options(), selected=selected_cluster_gene(), server=TRUE)
		})
		
		observeEvent(cluster_annotations(), {
		  req(cluster_annotations())
			updateSelectizeInput(session, 'cluster', "Cluster:", choices=cluster_annotations(), selected=cluster_annotations()[1], server=TRUE)
		})
		
		cluster_DEG = reactive({
		  req(input$cluster)
			read.delim(paste0('data/raw/DEG/organic/', input$cluster, '_2vs11_organic.csv'), sep=',', col.names = c("Gene", "p", "avg_log2FC", "pct.1", "pct.2", "adjusted_p"))
		})
		
		umap_cluster = reactive({
			DimPlot(seurat_obj(), reduction = "umap")
		})
		
		output$umap_cluster = renderPlot({
			umap_cluster()	
		})
		
		output$download_umap_cluster <- downloadHandler(
			filename = function() {'umap_by_cluster.pdf'},
			content = function(file) {
				pdf(file=file)
				plot(umap_cluster())
				dev.off()
			}
		)
		
		umap_count = reactive({
		  req(input$value)
			FeaturePlot(seurat_obj(), features = c(input$value))
		})
		
		output$umap_count = renderPlot({
			umap_count()	
		})
		
		output$download_umap_count <- downloadHandler(
			filename = function() {'umap_by_count.pdf'},
			content = function(file) {
				pdf(file=file)
				plot(umap_count())
				dev.off()
			}
		)
		
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
		
		violin = reactive({
		  req(input$value)
			VlnPlot(seurat_obj(), features = c(input$value), split.by='Type', slot='counts', log=TRUE)
		})
		
		output$violin = renderPlot({
			violin()	
		})
		
		output$download_violin <- downloadHandler(
			filename = function() {'violin_plot.pdf'},
			content = function(file) {
				pdf(file=file)
				plot(violin())
				dev.off()
			}
		)
		
		metadata = reactive({
			read.delim('data/clean/transcriptomic_metadata.txt', sep='\t', header=TRUE)
		})
		
		beta_data = reactive({
			read.delim('data/raw/RNA_Count_by_donor_and_samples_beta_cells.csv', sep=',') %>%
			pivot_longer(!gene, names_to='group', values_to='Value') %>%
			separate(group, into=c("ID", "Type")) %>%
			mutate(Type = case_when(Type == 'G11' ~ "H",
															Type == 'G2' ~ 'L'
			)) %>% 
			rename(Gene=gene) %>%
			left_join(metadata(), by='ID')
		})
		
		output$metadata = renderDT({
			datatable(metadata(),
								selection='single',
								colnames=c("Donor", "ID", "Source", "Age", "BMI", "Gender", "Comparison", "Comparison2"),
								rownames=FALSE,
								options=list(
									columnDefs = list(list(visible=FALSE, targets=c(6,7))),
									dom='t')
			)
		})
		
		return(beta_data)
		
	})
}