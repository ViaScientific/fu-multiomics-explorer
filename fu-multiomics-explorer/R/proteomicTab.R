proteomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
	  layout_columns(
		  barplotUI(ns('barplot')),
		  scatterplotUI(ns('comparison')),
		  col_widths = c(6,6)
	  )
		#box(width=6, title="Protein Comparison", status='primary', solidHeader = TRUE,
		#		fluidRow(
		#			column(6, selectizeInput(ns("x"), "X:", choices=NULL)),
		#			column(6, selectizeInput(ns("y"), "Y:", choices=NULL))
		#		),
		#		conditionalPanel("input.x != '' & input.y != ''",
		#										 scatterplotUI(ns('comparison')),
		#										 ns=ns
		#		)
		#),
		#box(width=6, title="Metadata", status='primary', solidHeader = TRUE,
		#		DTOutput(ns("metadata")) %>% withSpinner(image='spinner.gif')
		#)
	)
}

proteomicTabServer <- function(id) {
	
	moduleServer(id, function(input, output, session) {
		
		metadata = reactive({
			read.delim(file=clean_data_path('proteomic_metadata.txt'), header=TRUE, sep='\t') %>%
			mutate(Donor= as.factor(Donor))
		})

		raw_data = reactive({
		  read.delim(file=processed_data_path('A1_processed.txt'), header=TRUE, sep='\t') %>%
		    mutate(across(c(Donor, Replicate, Type), factor)) %>%
		    group_by(Protein, Donor, Type)
		})
		
		data = reactive({
        raw_data() %>%
				summarise(Error = sd(Value), Value=mean(Value), .groups='drop') %>%
				filter(Protein != '') %>%
				left_join(metadata(), by='Donor')
		})
		
		barplotServer('barplot', data, "Protein", "Protein", "Type", "Gender")
		
		#scatterplot_download_data = reactive({
		#  req(input$x)
		#  req(input$y)
		#  raw_data() %>% filter(Protein == input$x | Protein == input$y)
		#})
		
		scatterplotServer('comparison', data, "Protein", "Protein")
		
		#output$metadata = renderDT({
		#	datatable(metadata(),
		#		selection='single',
		#		colnames=c("Donor", "ID", "Source", "Age", "BMI", "Gender", "Comparison"),
		#		rownames=FALSE,
		#		options=list(
		#		  columnDefs = list(list(visible=FALSE, targets=c(6))),
		#		  dom='t')
		#	)
		#})
		
		
		return(data)
		
	})
}