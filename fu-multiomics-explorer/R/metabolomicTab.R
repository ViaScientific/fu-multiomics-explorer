metabolomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
	  barplotUI(ns('barplot'))
		#box(width=6, title="Metabolite Comparison", status='primary', solidHeader = TRUE,
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

metabolomicTabServer <- function(id) {

	moduleServer(id, function(input, output, session) {

		metadata = reactive({
			read.delim(file=clean_data_path('metabolomic_metadata.txt'), header=TRUE, sep='\t') %>%
				mutate(Donor=as.factor(Donor))
		})
		
		raw_data = reactive({
			read.delim(file=clean_data_path('B1.txt'), header=TRUE) %>%
				pivot_longer(cols=starts_with('D')) %>%
				mutate(Donor=substr(name, 2,2)) %>%
				mutate(Type=substr(name, 4,4)) %>%
				dplyr::select(Donor, Type, ionTopName, Value=value) %>%
				mutate(across(c(Donor, Type), factor)) %>%
				left_join(metadata(), by='Donor')
		})
				
		barplotServer('barplot', raw_data, 'ionTopName', 'Metabolite', 'Type', 'None')
		
		#output$metadata = renderDT({
		#	datatable(metadata(),
		#						selection='single',
		#						rownames=FALSE,
		#						colnames=c("Donor", "ID", "Source", "Age", "BMI", "Gender", "ComparisonID", "ComparisonID2"),
		#						options=list(
		#						  dom='t',
		#						  columnDefs = list(list(visible=FALSE, targets=c(6,7)))          
		#						)
		#	)
		#})
		#
		#observeEvent(raw_data(), {
		#  updateSelectizeInput(session, 'x', "X:", choices=options(), selected='', server=TRUE)
		#  updateSelectizeInput(session, 'y', "Y:", choices=options(), selected='', server=TRUE)
		#})
		#
		#comparison_data = reactive({
		#	req(input$x)
		#	req(input$y)
		#	x = raw_data() %>% filter(ionTopName == input$x) %>% rename(!!input$x := Value) %>% select(-ionTopName)
		#	y = raw_data() %>% filter(ionTopName == input$y) %>% rename(!!input$y := Value) %>% select(-ionTopName)
		#	combined = x %>% left_join(y, by=c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender"))
		#})
		#
		#scatterplotServer('comparison', comparison_data, reactive(input$x), reactive(input$y), download_data)
		
		return(data)
	})
}