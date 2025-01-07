# fuMultiomicsExplorer 

**fuMultiomicsExplorer** is an R package that provides tools for analyzing and visualizing multi-omics data, including transcriptomics, proteomics, and metabolomics. This package features interactive visualizations and modular components designed to explore complex biological datasets effectively.

---

## Features

- **Transcriptomic Data Analysis**
  - UMAP visualizations
  - Differentially expressed genes (DEGs) by cluster
  - Gene count and metadata display

- **Proteomic Data Analysis**
  - Protein quantification
  - Protein comparison
  - Metadata exploration

- **Metabolomic Data Analysis**
  - Metabolite quantification
  - Metabolite comparison
  - Associated metadata visualization

- **Cross-Assay Linkage**
  - Correlation analysis between genes, proteins, and metabolites

- **Network Visualization**
  - Interactive network plots
  - Node selection, visibility controls, and zooming

---

## Installation

To install the **fuMultiomicsExplorer** package from GitHub, use the following commands:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install fuMultiomicsExplorer from GitHub
devtools::install_github("viascientific/fuMultiomicsExplorer")
```

---

## Data Requirements

The package requires specific datasets to function. Ensure that the data directory is correctly set up during installation. 
After installation make sure to run data download function. 

```r
library(fuMultiomicsExplorer)
download_data()
```

The app will automatically download the required data if not found locally.

to check where the data downloaded;

```r
system.file(package = "fuMultiomicsExplorer")
```

For example, if the data should be where the package installed following by

```r
{your_installation_dir}/extdata/data
```

If you have issues downloading data, you can use the link below to download and after extraction make sure to put the folders under the directory above.

```r
https://umms.dolphinnext.com/fulab/fu-multiomics-explorer-main/data/data.tar.gz
```


---

## Usage

### Starting the App

To start the **fuMultiomicsExplorer** Shiny application, use one of the following methods:

#### Method 1: Using `startApp()`

```r
library(fuMultiomicsExplorer)
startApp()
```

This will launch the app in your default web browser.

#### Method 2: Using `runApp`
Make sure to clone the project from github and go to the directory to run with with runApp command.

```r
runApp("R")
```

Ensure you navigate to the directory containing the `app.R` file before running the above command.

---

## Functionalities

### 1. Transcriptomic Data

- Visualize UMAP clusters.
- Explore differentially expressed genes (DEGs) by cluster.
- Download cluster-based UMAP plots and gene count visualizations.
- Examine metadata associated with transcriptomic samples.

### 2. Proteomic Data

- Quantify proteins across different conditions.
- Compare protein levels between selected samples.
- Visualize and download bar plots and scatter plots.
- Explore metadata for proteomic samples.

### 3. Metabolomic Data

- Quantify metabolite levels across conditions.
- Compare metabolite levels between selected samples.
- Display metadata associated with metabolomic samples.

### 4. Cross-Assay Linkage

- Correlate genes, proteins, and metabolites.
- Visualize relationships using scatter plots.

### 5. Network Visualization

- Interactive network exploration.
- Highlight nodes, hide/unhide elements, and zoom into areas of interest.
- Customize network layouts and styles.

---

## Example Workflow

1. **Launch the app**:
   ```r
   startApp()
   ```

2. **Select a data type** (e.g., Transcriptomics, Proteomics, Metabolomics).

3. **Explore the data**:
   - Use dropdown menus to select specific clusters, proteins, or metabolites.
   - Generate and download visualizations.

4. **Perform cross-assay linkage**:
   - Correlate data between different omics layers.

5. **Visualize networks**:
   - Interact with networks to identify key nodes and relationships.

---

## Issues and Contributions

If you encounter any issues or have suggestions for improvement, please report them via the [GitHub Issues](https://github.com/viascientific/fuMultiomicsExplorer/issues) page.

---

## License

This package is licensed under the **GPL-3 License**.

---

## Citation

If you use **fuMultiomicsExplorer** in your research, please cite it as follows:

```
Author Names. fuMultiomicsExplorer: An R package for interactive multi-omics data analysis and visualization. Version 1.0.0. URL: https://github.com/viascientific/fuMultiomicsExplorer
```

