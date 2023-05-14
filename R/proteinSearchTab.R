proteinSearchTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      selectizeInput(ns("value"), "Selection:", choices=NULL),
    ),
    fluidRow(
        renderPlot(ns('protein_barplot'))
    )
  )
}

proteinSearchTabServer <- function(id, protein_options, protein_data_processed) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(protein_data_processed(), {
      updateSelectizeInput(session, 'value', "Selection:", choices=protein_options(), selected='', server=TRUE)
    })

    protein_data = reactive({
      read.delim(file='data/processed/A1_processed.txt', header=TRUE, sep='\t') %>%
      mutate(across(c(Donor, Replicate, Type), factor)) %>%
      group_by(Protein, Donor, Type) %>%
      summarise(Error=sd(Value), Value=mean(Value), .groups='drop')
    })
    
    data_filtered = reactive({
      protein_data %>% filter(Protein == input$value)
    })
    
    output$protein_barplot = renderPlot({
      req(input$value)
      ggplot(data_filtered(), aes(x=Donor, y=Value, fill=Type))+
        theme_classic() +
        theme(strip.background = element_blank()) +
        scale_y_continuous(expand=c(0,0)) +
        geom_bar(stat='identity', position=position_dodge(.9)) +
        geom_errorbar(aes(ymin=Value-Error, ymax=Value+Error), position=position_dodge(.9), width=.2)
    })
    
    
    #barplotServer("barplot", data_filtered)
  
    }
  )
}