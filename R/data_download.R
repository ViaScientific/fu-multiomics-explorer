#' Download and Extract Data
#'
#'
#' @description Downloads and extracts the data required for the package.
#' @param url url for the data located
#' @param dest_dir to store the data
#' @importFrom utils download.file untar
#' @export

download_and_extract <- function(url, dest_dir) {
  # Check if the destination directory already exists and contains files
  if (dir.exists(dest_dir) && length(list.files(dest_dir, recursive = TRUE)) > 0) {
    message("Data already exists in: ", dest_dir)
    return(invisible(TRUE))
  }
  
  # Create the destination directory if it doesn't exist
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  
  # Temporary file for downloading the tar.gz archive
  temp_tar <- tempfile(fileext = ".tar.gz")
  
  # Inform the user about the download time
  message("Downloading data, it can take ~10 minutes depending on your network speed...")
  
  # Attempt to download the file
  tryCatch({
    download.file(url, temp_tar, mode = "wb", method = "libcurl")
    message("Download completed.")
    
    # Extract the tar.gz archive to the destination directory
    message("Extracting data...")
    untar(temp_tar, exdir = dest_dir)
    message("Data successfully downloaded and extracted to: ", dest_dir)
  }, error = function(e) {
    message("Failed to download or extract data: ", e$message)
  })
}
#' Download data
#'
#' @description Downloads data to the installed package directory.
#' @export
download_data <- function() {
  options(timeout = 21000)  # Set timeout to 10 minutes
  
  data_url <- "https://umms.dolphinnext.com/fulab/fu-multiomics-explorer-main/data/data.tar.gz"  # Replace with your actual URL
  data_dir <- file.path(system.file("extdata", package = "fuMultiomicsExplorer"), "data")
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  download_and_extract(data_url, data_dir)
  message("Data downloaded and extracted to: ", data_dir)
}