metabolomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		box(width=6, title="Metabolite Quantification", status='primary', solidHeader = TRUE,
			selectizeInput(ns("value"), "Selection:", choices=NULL),
			plotOutput(ns("out")) %>% withSpinner(image='spinner.gif')
		),
		box(width=6, title="Metadata", status='primary', solidHeader = TRUE,
				DTOutput(ns("metadata")) %>% withSpinner(image='spinner.gif')
		)
	)
}

metabolomicTabServer <- function(id) {

	moduleServer(id, function(input, output, session) {

		metadata = reactive({
			read.delim(file='data/clean/metabolomic_metadata.txt', header=TRUE, sep='\t') %>%
				mutate(Donor=as.factor(Donor))
		})
		
		data = reactive({
			read.delim(file='data/clean/B1.txt', header=TRUE) %>%
				pivot_longer(cols=starts_with('D')) %>%
				mutate(Donor=substr(name, 2,2)) %>%
				mutate(Type=substr(name, 4,4)) %>%
				dplyr::select(Donor, Type, ionTopName, Value=value) %>%
				mutate(across(c(Donor, Type), factor)) %>%
				left_join(metadata(), by='Donor')
		})
				
		options = reactive({
			(data() %>% arrange(ionTopName) %>% distinct(ionTopName))$ionTopName
		})
		
		observeEvent(data(), {
			updateSelectizeInput(session, 'value', "Selection:", choices=options(), selected='', server=TRUE)
		})

		filtered_data = reactive({
			data() %>% filter(ionTopName == input$value)
		})

		output$out = renderPlot({
			req(input$value)
			ggplot(filtered_data(), aes(x=Donor, y=Value, fill=Type)) +
				theme_classic() +
				theme(plot.title = element_text(hjust = 0.5)) +
				ggtitle(input$value) +
				scale_y_continuous(expand=c(0,0)) +
				geom_bar(stat='identity', position=position_dodge(.9))
		})
		
		output$metadata = renderDT({
			datatable(metadata(),
								selection='single',
								rownames=FALSE,
								options=list(dom='t'),
								colnames=c("Donor", "ID", "Source", "Age", "BMI", "Gender")
			)
		})
		
		return(data)
	})
}