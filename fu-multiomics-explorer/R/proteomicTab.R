proteomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		barplotUI(ns('barplot'))
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
		#
		#observeEvent(data(), {
		#  updateSelectizeInput(session, 'x', "X:", choices=options(), selected='', server=TRUE)
		#  updateSelectizeInput(session, 'y', "Y:", choices=options(), selected='', server=TRUE)
		#})
		#
		#comparison_data = reactive({
		#	req(input$x)
		#	req(input$y)
		#	x = data() %>% filter(Protein == input$x) %>% rename(!!input$x := Value) %>% select(-Protein, -Error)
		#	y = data() %>% filter(Protein == input$y) %>% rename(!!input$y := Value) %>% select(-Protein, -Error)
		#	combined = x %>% left_join(y, by=c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender"))
		#})
		#
		#
		#scatterplot_download_data = reactive({
		#  req(input$x)
		#  req(input$y)
		#  raw_data() %>% filter(Protein == input$x | Protein == input$y)
		#})
		
		#scatterplotServer('comparison', comparison_data, reactive(input$x), reactive(input$y), scatterplot_download_data)
		
		return(data)
		
	})
}