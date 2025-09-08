FROM rocker/shiny:4.3.0
LABEL author="zach@viascientific.com" description="Docker image containing all requirements for Fu Multiomics App"

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN mkdir -p /mnt /export

# Install System libraries
RUN apt-get update --fix-missing && \
    apt-get install -y gcc unzip curl make zlib1g-dev libglpk-dev libgmp3-dev libxml2-dev pandoc libicu-dev vim libx11-dev libcurl4-openssl-dev libssl-dev libfreetype6-dev libjpeg-dev libpng-dev libtiff-dev libwebp-dev libfontconfig1-dev libfribidi-dev libharfbuzz-dev libxml2-dev

# Install required R packages
RUN R -e 'install.packages(c("shiny", "shinyBS", "Seurat", "htmlwidgets", "graph", "jsonlite", "igraph", "tidyverse", "DT", "shinycssloaders", "shinydashboard", "svglite", "shinyjs", "data.table", "BiocManager"), \
    repos="https://packagemanager.posit.co/cran/__linux__/focal/2023-04-01")'
RUN R -e "BiocManager::install(version = '3.18')"
RUN R -e 'BiocManager::install(c("graph"))'
RUN R -e "install.packages(c('cyjShiny'))"

# Copy app directory onto image
ADD fu-multiomics-explorer /fu-multiomics-explorer/

CMD ["R", "-e", "shiny::runApp('/fu-multiomics-explorer')"]
