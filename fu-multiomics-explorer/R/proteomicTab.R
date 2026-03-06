proteomicTabUI <- function(id) {
	ns <- NS(id)
	tagList(
	  div(
	    layout_columns(
		    barplotUI(ns('barplot')),
		    scatterplotUI(ns('comparison')),
		    col_widths = c(6,6)
	    )
	  ),
	  div(
	    layout_columns(
	      metadataTableUI(ns('metadata')),
	      col_widths = c(6)
	    )
	  )
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
		scatterplotServer('comparison', data, "Protein", "Protein")
		metadataTableServer('metadata', metadata)
		
		return(data)
		
	})
}