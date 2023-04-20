proteinSearchTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			selectizeInput(ns("value"), "Selection:", choices=NULL)
		),
		fluidRow(
			plotOutput(ns("out")) %>% withSpinner()
		)
	)
}

proteinSearchTabServer <- function(id, protein_options, protein_processed_data) {
	
	moduleServer(id, function(input, output, session) {
	
		observeEvent(protein_processed_data(), {
			updateSelectizeInput(session, 'value', "Selection:", choices=protein_options(), selected='', server=TRUE)
		})
	
		data = reactive({
			read.delim(file='data/processed/A1_processed.txt', header=TRUE, sep='\t') %>%
				mutate(across(c(Donor, Replicate, Type), factor)) %>%
				group_by(Protein, Donor, Type) %>%
				summarise(Error = sd(Value), Value=mean(Value), .groups='drop')
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
	})
}