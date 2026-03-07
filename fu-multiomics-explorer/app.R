source("global.R", local = FALSE)

options(scipen = 4) 

ui = tagList( 

  useShinyjs(),
  
  page_navbar(
    title='Multiomics Explorer', id='nav_id', fillable=TRUE, navbar_options = navbar_options(theme = "dark"),
    theme = bs_theme(
      version = 5, bootswatch = "cosmo",
      tooltip_max_width = "none",
      "navbar-padding-y" = "0.2rem",
      "nav-link-padding-y" = "0.25rem",
      "nav-link-font-size" = "0.9rem",
      "nav-link-line-height" = "1"
    ) |> bs_add_rules("
        .acc--dense {
          --bs-accordion-btn-padding-y: 0.25rem;
          --bs-accordion-btn-padding-x: 0.6rem;
          --bs-accordion-btn-font-size: 0.88rem;
          --bs-accordion-body-padding-y: 0.35rem;
          --bs-accordion-body-padding-x: 0.6rem;
          --bs-accordion-btn-icon-width: 0.85rem;
        }
        .acc--dense .accordion-button {
          line-height: 1.1;
        }
        .acc--dense .accordion-body > * {
          margin-bottom: 0.4rem;
        }
        .acc--dense .accordion-body > *:last-child {
          margin-bottom: 0;
        }
        .acc--dense .mb-3 {
          margin-bottom: 0.5rem !important;
        }
        .acc--dense .form-label {
          margin-bottom: 0.25rem;
        }
        .acc--dense .btn {
          padding: 0.25rem 0.5rem;
        }
        .table > :not(caption) > * > * {
          padding: 0.4rem 0.6rem;
        }
        .table {
          font-size: 0.9rem;
        }
        
        .btn,
        .btn-sm,
        .btn-group-sm > .btn {
          --bs-btn-padding-y: 0.25rem;
          --bs-btn-padding-x: 0.5rem;
          --bs-btn-font-size: 0.85rem;
          --bs-btn-border-radius: 0.2rem;
        }
        
        .btn-secondary {
          --bs-btn-bg: #e9ecef;
          --bs-btn-border-color: #dee2e6;
          --bs-btn-color: #495057;
        
          --bs-btn-hover-bg: #dde2e6;
          --bs-btn-hover-border-color: #ced4da;
          --bs-btn-hover-color: #343a40;
        
          --bs-btn-active-bg: #d3d9df;
          --bs-btn-active-border-color: #c6ccd2;
        }
    "),

		nav_panel("Transcriptomic Data",
			transcriptomicTabUI('transcriptomic_tab')
		),
		nav_panel("Proteomic Data",
			proteomicTabUI('proteomic_tab')
		),
		nav_panel("Metabolomic Data",
			metabolomicTabUI('metabolomic_tab')
		),
		nav_panel("Linkage Assay",
			crossAssayTabUI('crossAssay_tab')
		),
		nav_panel("Network",
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