proteomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		box(width=6, title="Protein Quantification", status='primary', solidHeader = TRUE,
			selectizeInput(ns("value"), "Selection:", choices=NULL),
			plotOutput(ns("out")) %>% withSpinner(image='spinner.gif')
		)
	)
}

proteomicTabServer <- function(id) {
	
	moduleServer(id, function(input, output, session) {
		
		data = reactive({
			read.delim(file='data/processed/A1_processed.txt', header=TRUE, sep='\t') %>%
				mutate(across(c(Donor, Replicate, Type), factor)) %>%
				group_by(Protein, Donor, Type) %>%
				summarise(Error = sd(Value), Value=mean(Value), .groups='drop')
		})
		
		options = reactive({
			(data() %>% arrange(Protein) %>% distinct(Protein))$Protein
		})
		
		observeEvent(data(), {
			updateSelectizeInput(session, 'value', "Selection:", choices=options(), selected='', server=TRUE)
		})
		
		filtered_data = reactive({
			data() %>% filter(Protein == input$value)
		})
		
		output$out = renderPlot({
			req(input$value)
			ggplot(filtered_data(), aes(x=Donor, y=Value, fill=Type)) +
				theme_classic() +
				theme(plot.title = element_text(hjust = 0.5)) +
				ggtitle(input$value) +
				scale_y_continuous(expand=c(0,0)) +
				geom_bar(stat='identity', position=position_dodge(.9)) +
				geom_errorbar(aes(ymin=Value-Error, ymax=Value+Error), width=.2, position=position_dodge(.9))
		})
		
		return(data)
		
	})
}