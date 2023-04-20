metaboliteSearchTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			selectizeInput(ns("value"), "Selection:", choices=NULL)
		),
		fluidRow(
			plotOutput(ns("out"))  %>% withSpinner()
		)
	)
}

metaboliteSearchTabServer <- function(id, metabolite_options, metabolite_processed_data) {

	moduleServer(id, function(input, output, session) {

		observeEvent(metabolite_processed_data(), {
			updateSelectizeInput(session, 'value', "Selection:", choices=metabolite_options(), selected='', server=TRUE)
		})

		data = reactive({
				read.delim(file='data/clean/B1.txt', header=TRUE) %>%
					pivot_longer(cols=starts_with('D')) %>%
					mutate(Donor=substr(name, 2,2)) %>%
					mutate(Type=substr(name, 4,4)) %>%
					dplyr::select(Donor, Type, ionTopName, Value=value) %>%
					mutate(across(c(Donor, Type), factor))
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
	})
}