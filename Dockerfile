FROM rocker/shiny:4.5.0
LABEL author="zach@viascientific.com" description="Docker image containing all requirements for Fu Multiomics App"

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN mkdir -p /mnt /export

# Install System libraries
RUN apt-get update --fix-missing && \
    apt-get install -y gcc unzip curl make zlib1g-dev libglpk-dev libgmp3-dev libxml2-dev pandoc libicu-dev vim libx11-dev libcurl4-openssl-dev libssl-dev libfreetype6-dev libjpeg-dev libpng-dev libtiff-dev libwebp-dev libfontconfig1-dev libfribidi-dev libharfbuzz-dev libxml2-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
    
# Install required R packages
RUN R -e 'install.packages(c("shiny", "Seurat", "graph", "jsonlite", "igraph", "dplyr", "tidyr", "ggplot2", "readr", "tibble", "DT", "shinycssloaders", "svglite", "shinyjs", "data.table", "BiocManager", "bslib", "bsicons"), \
    repos="https://packagemanager.posit.co/cran/__linux__/noble/2026-03-01")'
RUN R -e "BiocManager::install(version = '3.22')"
RUN R -e 'BiocManager::install(c("graph"))'
RUN R -e "install.packages(c('cyjShiny'))"

ENV DATA_DIR=/home/app_data/

# Copy app directory onto image
ADD fu-multiomics-explorer /fu-multiomics-explorer/

CMD ["R", "-e", "shiny::runApp('/fu-multiomics-explorer')"]
