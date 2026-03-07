FROM rocker/shiny:4.5.0 AS builder
LABEL author="zach@viascientific.com" description="Builder image for Fu Multiomics App"

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran pkg-config ca-certificates curl git libcurl4-openssl-dev libglpk-dev libssl-dev libxml2-dev libicu-dev zlib1g-dev libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libtiff-dev libwebp-dev libharfbuzz-dev libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("shiny", "Seurat", "jsonlite", "igraph", "dplyr", "tidyr", "ggplot2", "readr", "tibble", "DT", "shinycssloaders", "svglite", "shinyjs", "data.table", "BiocManager", "bslib", "bsicons"), \
    repos="https://packagemanager.posit.co/cran/__linux__/noble/2026-03-01")'
RUN R -e "BiocManager::install(version = '3.22')"
RUN R -e 'BiocManager::install(c("graph"))'
RUN R -e "install.packages(c('cyjShiny'))"

RUN R -q -e 'library(shiny); library(Seurat); library(cyjShiny); library(jsonlite); library(dplyr); library(tidyr); library(ggplot2); library(readr); library(tibble); library(DT); library(shinycssloaders); library(svglite); library(shinyjs); library(bslib); library(bsicons)'

FROM rocker/shiny:4.5.0
LABEL author="zach@viascientific.com" description="Runtime image for Fu Multiomics App"

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV DATA_DIR=/home/app_data

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4 libssl3 libxml2 libicu74 zlib1g libglpk40 libfontconfig1 libfreetype6 libpng16-16 libjpeg-turbo8 libtiff6 libwebp7 libharfbuzz0b libfribidi0 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy app directory onto image
ADD fu-multiomics-explorer /fu-multiomics-explorer/

CMD ["R", "-e", "shiny::runApp('/fu-multiomics-explorer')"]