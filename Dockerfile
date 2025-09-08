FROM rocker/shiny:4.4.0
LABEL author="zach@viascientific.com" description="Docker image containing all requirements for the Via Scientific GSEA explorer App"

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN mkdir -p /mnt /export

# Install System libraries
RUN apt-get update --fix-missing && \
    apt-get install -y gcc unzip curl make zlib1g-dev libglpk-dev libgmp3-dev libxml2-dev pandoc libicu-dev vim

# Install required R packages
RUN R -e 'install.packages(c("shiny", "BiocManager", "dplyr", "DT", "ggplot2", "shinycssloaders", "igraph", "shinyjs", "bslib", "stringr", "rhandsontable", "forcats", "purrr", "htmltools", "yaml", "tidyr"), \
    repos="https://packagemanager.posit.co/cran/__linux__/focal/2024-05-01")'
RUN R -e "BiocManager::install(version = '3.19', ask=FALSE)"
RUN R -e "BiocManager::install(c('fgsea', 'clusterProfiler', 'org.Hs.eg.db', 'org.Mm.eg.db', 'qvalue'))"

# Copy app directory onto image
ADD gsea-explorer /gsea-explorer/

CMD ["R", "-e", "shiny::runApp('/gsea-explorer')"]
