library(shiny)
library(tidyverse)
library(DT)
library(shinyBS)
library(Seurat)

ui <- fluidPage(
  titlePanel("Multiomics Explorer"),
  #sidebarLayout(
    #sidebarPanel(
    #  fileInput('rnaseq_input',"Transcriptomic Input"),
    #  fileInput('protein_input',"Proteomic Input"),
    #  fileInput('metabolite_input',"Metabolomic Input"),
    #  width=2
    #),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Search",
                           br(),
                           searchTabUI('search_tab')
                  ),
                  tabPanel("Gene Search",
                           br(),
                           geneSearchTabUI('geneSearch_tab')
                  ),
                  tabPanel("Protein Search",
                           br(),
                           DTOutput('protein_clean')
                  ),
                  tabPanel("Metabolite Search",
                           br(),
                           DTOutput('metabolic_clean')
                  ),
                  tabPanel("Correlate",
                           br(),
                           correlateTabUI('correlate_tab')
                  ),
                  tabPanel("Network",
                           
                  )
      )
    )
  #)
)

server <- function(input, output, session) {
  
  gene_options = reactive({
    readRDS('data/clean/sc-rnaseq_features.rds')
  })
  
  seurat_obj = reactive({
    rds = readRDS('data/raw/seurat_integrated.rds')
    return(rds)
  })
  
  protein_data_clean = reactive({
    read.delim(file='data/clean/A1.txt', header=TRUE, sep='\t') %>% rename(Protein=X)
  })
  
  protein_data_processed = reactive({
    read.delim(file='data/processed/A1_processed.txt', header=TRUE, sep='\t') %>%
      mutate(across(c(Donor, Replicate, Type), factor))
  })

  protein_options = reactive({
    (protein_data_processed() %>% arrange(Protein) %>% distinct(Protein))$Protein
  })
  
  output$protein_clean = renderDT({
    datatable(protein_data_clean(), filter='top')
  })
  
  metabolic_data_clean = reactive({
    df = read.delim(file='data/clean/B1.txt', header=TRUE)
  })
  
  metabolic_data_processed = reactive({
    metabolic_data_clean() %>%
      pivot_longer(cols=starts_with('D')) %>%
      mutate(Donor=substr(name, 2,2)) %>%
      mutate(Type=substr(name, 4,4)) %>%
      select(Donor, Type, ionTopName, Value=value) %>%
      mutate(across(c(Donor, Type), factor))
  })
  
  metabolic_options = reactive({
    (metabolic_data_processed() %>% arrange(ionTopName) %>% distinct(ionTopName))$ionTopName
  })
  
  output$metabolic_clean = renderDT({
    datatable(metabolic_data_clean(), filter='top')
  })
  
  searchTabServer('search_tab', gene_options, protein_options, metabolic_options, protein_data_processed, metabolic_data_processed)
 
  geneSearchTabServer('geneSearch_tab', gene_options, seurat_obj)
   
  correlateTabServer('correlate_tab', gene_options, protein_options, metabolic_options, protein_data_processed, metabolic_data_processed)
}
options(shiny.host = "0.0.0.0", shiny.port = 8789)
shinyApp(ui = ui, server = server)