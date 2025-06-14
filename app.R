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
			transcriptomicTabUI('transcriptomic_tab')
		),
		tabPanel("Proteomic Data",
			proteomicTabUI('proteomic_tab')
		),
		tabPanel("Metabolomic Data",
			metabolomicTabUI('metabolomic_tab')
		),
		tabPanel("Linkage Assay",
			crossAssayTabUI('crossAssay_tab')
		),
		tabPanel("Network",
			networkTabUI('network_tab')
		)
  )
)

server <- function(input, output, session) {

  transcript_data = transcriptomicTabServer('transcriptomic_tab')
  
  protein_data = proteomicTabServer('proteomic_tab') 
  
  metabolite_data = metabolomicTabServer('metabolomic_tab')
  
  crossAssayTabServer('crossAssay_tab', transcript_data, protein_data, metabolite_data)

  networkTabServer('network_tab')
}

options(shiny.host = "0.0.0.0", shiny.port = 8789, shiny.reactlog = TRUE)
shinyApp(ui = ui, server = server)