proteomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
		box(width=6, title="Protein Quantification", status='primary', solidHeader = TRUE,
				selectizeInput(ns("value"), "Selection:", choices=NULL),
				barplotUI(ns('barplot'))
		),
		box(width=6, title="Protein Comparison", status='primary', solidHeader = TRUE,
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
			updateSelectizeInput(session, 'x', "X:", choices=options(), selected='', server=TRUE)
			updateSelectizeInput(session, 'y', "Y:", choices=options(), selected='', server=TRUE)
		})
		
		filtered_data = reactive({
			req(input$value)
			data() %>% filter(Protein == input$value)
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
			x = data() %>% filter(Protein == input$x) %>% rename(!!input$x := Value) %>% select(-Protein, -Error)
			y = data() %>% filter(Protein == input$y) %>% rename(!!input$y := Value) %>% select(-Protein, -Error)
			combined = x %>% left_join(y, by=c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender"))
		})
		
		barplotServer('barplot', filtered_data, "Type", "Gender")
		
		scatterplotServer('comparison', comparison_data, reactive(input$x), reactive(input$y))
		
		return(data)
		
	})
}