proteomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		box(width=6, title="Protein Quantification", status='primary', solidHeader = TRUE,
				selectizeInput(ns("value"), "Selection:", choices=NULL),
				barplotUI(ns('barplot')),
		),
		box(width=6, title="Metadata", status='primary', solidHeader = TRUE,
				DTOutput(ns("metadata")) %>% withSpinner(image='spinner.gif')
		)
	)
}

proteomicTabServer <- function(id) {
	
	moduleServer(id, function(input, output, session) {
		
		metadata = reactive({
			read.delim(file='data/clean/proteomic_metadata.txt', header=TRUE, sep='\t') %>%
			mutate(Donor=as.factor(Donor))
		})

		data = reactive({
			read.delim(file='data/processed/A1_processed.txt', header=TRUE, sep='\t') %>%
				mutate(across(c(Donor, Replicate, Type), factor)) %>%
				group_by(Protein, Donor, Type) %>%
				summarise(Error = sd(Value), Value=mean(Value), .groups='drop') %>%
				filter(Protein != '') %>%
				left_join(metadata(), by='Donor')
		})
		
		options = reactive({
			(data() %>% arrange(Protein) %>% distinct(Protein))$Protein
		})
		
		observeEvent(data(), {
			updateSelectizeInput(session, 'value', "Selection:", choices=options(), selected='', server=TRUE)
		})
		
		filtered_data = reactive({
			req(input$value)
			data() %>% filter(Protein == input$value)
		})
		
		#output$out = renderPlot({
		#	req(input$value)
		#	ggplot(filtered_data(), aes(x=Donor, y=Value, fill=Type)) +
		#		theme_classic() +
		#		theme(plot.title = element_text(hjust = 0.5)) +
		#		ggtitle(input$value) +
		#		scale_y_continuous(expand=c(0,0)) +
		#		geom_bar(stat='identity', position=position_dodge(.9)) +
		#		geom_errorbar(aes(ymin=Value-Error, ymax=Value+Error), width=.2, position=position_dodge(.9))
		#})
		
		output$metadata = renderDT({
			datatable(metadata(),
				selection='single',
				rownames=FALSE,
				options=list(dom='t'),
				colnames=c("Donor", "ID", "Source", "Age", "BMI", "Gender")
			)
			
		})
		
		barplotServer('barplot', filtered_data)
		
		return(data)
		
	})
}