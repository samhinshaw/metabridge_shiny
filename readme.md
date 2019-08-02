# MetaBridge

## Description

MetaBridge is a data integration system for integration of metabolomics data with other omics types, chiefly transcriptomics and proteomics.

## Contents

## Usage

MetaBridge is designed to facilitate integrative analysis by identifying the enzymes that directly interact with metabolites of interest. A pipeline may be designed as such:

![Pipeline Schema](./figure.png)

### Tutorial

To learn how to use MetaBridge as part of a network-based integrative analysis workflow, please read our [tutorial](./tutorial/tutorial.md).

## Installation (@async branch)

MetaBridge@async uses Promises, a framework for asynchronous programming in R. From the promises readme:

A promise library for R. https://rstudio.github.io/promises

```r
devtools::install_github("rstudio/promises")
```

Neither the CRAN nor GitHub master branch versions of Shiny support promises. Until support is merged, you'll have to install from this branch:

```r
devtools::install_github("rstudio/shiny@async")
devtools::install_github("rstudio/DT@async")
```