metabolomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		box(width=6, title="Metabolite Quantification", status='primary', solidHeader = TRUE,
				selectizeInput(ns("value"), "Selection:", choices=NULL),
				barplotUI(ns('barplot'))
		),
		box(width=6, title="Metabolite Comparison", status='primary', solidHeader = TRUE,
				fluidRow(
					column(6, selectizeInput(ns("x"), "X:", choices=NULL)),
					column(6, selectizeInput(ns("y"), "Y:", choices=NULL))
				),
				conditionalPanel("input.x != '' & input.y != ''",
												 scatterplotUI(ns('comparison')),
												 ns=ns
				)
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
			updateSelectizeInput(session, 'x', "X:", choices=options(), selected='', server=TRUE)
			updateSelectizeInput(session, 'y', "Y:", choices=options(), selected='', server=TRUE)
		})

		filtered_data = reactive({
			req(input$value)
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
		
		comparison_data = reactive({
			req(input$x)
			req(input$y)
			x = data() %>% filter(ionTopName == input$x) %>% rename(!!input$x := Value) %>% select(-ionTopName)
			y = data() %>% filter(ionTopName == input$y) %>% rename(!!input$y := Value) %>% select(-ionTopName)
			combined = x %>% left_join(y, by=c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender"))
		})
		
		barplotServer('barplot', filtered_data)
		
		scatterplotServer('comparison', comparison_data, reactive(input$x), reactive(input$y))
		
		return(data)
	})
}