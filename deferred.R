library(tools)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)
library(rlang)
library(DT)
library(pathview)
library(shinycssloaders)
library(promises)
library(future)
plan(multiprocess)

# EXAMINE THIS ###############
# load KEGGDB??
# load('data/sysdata.rda')
# ^^^^^^^^^^^^^^^^^^^^^^^^

# Load Example Data
load("data/examples.RData")

# Load KEGG Database Files
# Import Enzymes & Enzyme Names here once scraping has been completed.
#
load("data/k00_keggCompounds.RData")
# load('data/k01_keggEnzymes.RData')
load("data/k02b_keggEnzymeShortNames.RData") # use the shorter names for now!
load("data/k03_keggGenes.RData")
# for the moment, only keep enzyme - gene relationships
keggGenes %<>% dplyr::select(-KEGG) %>% unique()
load("data/k04_keggPathways.RData")
load("data/k05_keggHumanPathways.RData")

# Load MetaCyc Database Files
load("data/m01_metaCycDBLinks.RData")
load("data/m02_metaCycCompoundsReactions.RData")
load("data/m03_metaCycReactionsGenes.RData")
# load('data/m04_metaCycGeneIDs.RData')
# temporarily import new cleaned files
load("database_updates/cleaned_files/m04_metaCycGeneIDs.RData")
load("data/m05_metaCycPathways.RData")

# Load HumanCyc Cross-References
load("data/humanRefs.RData")

# Utility functions
source(file.path("functions", "utilityFunctions.R"), local = TRUE)$value

# Source Mapping Functions
source(file.path("functions", "mapGenerally.R"), local = TRUE)$value
source(file.path("functions", "mapPathways.R"), local = TRUE)$value
source(file.path("functions", "visualizePathways.R"), local = TRUE)$value

# Source App Functions
source(file.path("functions", "alertFunctions.R"), local = TRUE)$value
source(file.path("functions", "generateTables.R"), local = TRUE)$value

# Set global DataTables options:

options(
  DT.options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 15, 20),
    # autoWidth = TRUE,
    scrollX = "100%",
    # AMAZING! Crucial argument to make sure DT doesn't overflow
    # vertical scrolling options
    scrollY = "250px",
    scrollCollapse = TRUE,
    paging = FALSE,
    dom = "tir"
  )
)
