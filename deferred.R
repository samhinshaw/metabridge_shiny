
# Load packages -----------------------------------------------------------

library(tools)
library(magrittr)
library(rlang)
library(DT)
# install.packages("BiocManager")
# BiocManager::install("pathview")
library(pathview)
library(shinycssloaders)
library(tidyverse)


# Load example data -------------------------------------------------------

load("data/examples.RData")


# Load KEGG database files ------------------------------------------------

load("data/k00_keggCompounds.RData")
# load('data/k01_keggEnzymes.RData')
load("data/k02b_keggEnzymeShortNames.RData") # Use the shorter names for now

load("data/k03_keggGenes.RData")
# For the moment, only keep enzyme-gene relationships
keggGenes <- keggGenes %>%
  dplyr::select(-KEGG) %>%
  unique()

load("data/k04_keggPathways.RData")
load("data/k05_keggHumanPathways.RData")


# Load MetaCyc database files ---------------------------------------------

# Updated to MetaCyc v23, on September 19th, 2019
load("data/m01_metaCycDBLinks_v23.RData")
load("data/m02_metaCycCompoundsReactions_v23.RData")
load("data/m03_metaCycReactionsGenes_v23.RData")
load("data/m04_metaCycGeneIDs_v23.RData")
load("data/m05_metaCycPathways_v23.RData")


# Load HumanCyc cross-references ------------------------------------------

load("data/humanRefs.RData")


# Utility functions -------------------------------------------------------

source(file.path("functions", "utilityFunctions.R"), local = TRUE)$value


# Source primary mapping functions -----------------------------------------

source(file.path("functions", "mapGenerally.R"), local = TRUE)$value
source(file.path("functions", "mapPathways.R"), local = TRUE)$value
source(file.path("functions", "visualizePathways.R"), local = TRUE)$value


# Source app/UI functions -------------------------------------------------

source(file.path("functions", "alertFunctions.R"), local = TRUE)$value
source(file.path("functions", "generateTables.R"), local = TRUE)$value


# Set global DataTables options -------------------------------------------

options(
  DT.options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 15, 20),
    # autoWidth = TRUE,
    scrollX = "100%",
    # Argument to make sure DT doesn't overflow vertical scrolling options
    scrollY = "300px",
    scrollCollapse = TRUE,
    paging = FALSE,
    dom = "tir"
  )
)
