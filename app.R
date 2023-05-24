library(shiny)
library(shinyBS)
library(Seurat)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)
library(igraph)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(shinydashboard)

ui = fluidPage( 

	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
	),			
	
	navbarPage("Multiomics Explorer",
			
		tabPanel("Transcriptomic Data",
			transcriptomicTabUI('geneSearch_tab')
		),
		tabPanel("Proteomic Data",
			proteomicTabUI('proteinSearch_tab')
		),
		tabPanel("Metabolomic Data",
			metabolomicTabUI('metaboliteSearch_tab')
		),
		tabPanel("Correlate",
			correlateTabUI('correlate_tab')
		),
		tabPanel("Network",
			networkTabUI('network_tab')
		)
  )
)

server <- function(input, output, session) {

  transcriptomicTabServer('geneSearch_tab')
  
  protein_data = proteomicTabServer('proteinSearch_tab') 
  
  metabolite_data = metabolomicTabServer('metaboliteSearch_tab')
  
  correlateTabServer('correlate_tab', protein_data, metabolite_data)

  networkTabServer('network_tab')
  
  }
options(shiny.host = "0.0.0.0", shiny.port = 8789)
shinyApp(ui = ui, server = server)