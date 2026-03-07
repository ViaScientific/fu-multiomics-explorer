library(shiny)
library(Seurat)
library(cyjShiny)
library(jsonlite)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(svglite)
library(shinyjs)
library(bslib)
library(bsicons)

APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DATA_DIR <- Sys.getenv("DATA_DIR", unset = file.path(APP_DIR, "data"))
DATA_DIR <- normalizePath(DATA_DIR, winslash = "/", mustWork = FALSE)

raw_data_path <- function(...) file.path(DATA_DIR, "raw", ...)
clean_data_path <- function(...) file.path(DATA_DIR, "clean", ...)
processed_data_path <- function(...) file.path(DATA_DIR, "processed", ...)
cluster_path <- function(...) file.path(DATA_DIR, "raw", "DEG", "organic", ...)

BARPLOT_DOWNLOAD_COLUMNS = c("Protein", "ionTopName", "Donor", "Type", "Value", "Error", "ID", "Source", "Age", "BMI", "Gender")
SCATTERPLOT_DOWNLOAD_COLUMNS = c("Donor", "Type", "ID", "Source", "Age", "BMI", "Gender")
METADATA_COLUMNS = c("Donor", "ID", "Source", "Age", "BMI", "Gender")

card_header_with_download_and_settings <- function(title, ...) {
  card_header(
    div(
      class = "d-flex justify-content-between align-items-center w-100",
      div(title),
      div(class = "d-flex gap-2", ...)
    )
  )
}