metadataTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header_with_download_and_settings(
        "Metadata",
        popover(
          title = "Download Options", bs_icon("download"),
          dfDownloadPopoverUI(ns('download'))
        )
      ),
      card_body(
        DTOutput(ns("metadata")) %>% withSpinner(image='spinner.gif')
      )
    )
  )
}

metadataTableServer <- function(id, raw_metadata) {
  
  moduleServer(id, function(input, output, session) {

    metadata = reactive({
      raw_metadata() %>% select(any_of(METADATA_COLUMNS))
    })
    
    output$metadata = renderDT({
    	datatable(metadata(),
    		selection='single',
    		rownames=FALSE,
    		options=list(dom='t')
    	)
    })
    
    dfDownloadPopoverServer('download', metadata, "metadata")
    
  })
}