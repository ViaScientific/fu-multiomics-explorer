metabolomicTabUI <- function(id) {
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
		scatterplotServer('comparison', raw_data, "ionTopName", "Metabolite")
		metadataTableServer('metadata', metadata)
	
		return(data)
		
	})
}