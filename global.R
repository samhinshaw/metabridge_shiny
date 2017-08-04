## Load Packages
library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(magrittr)
library(DT)
library(rlang)
library(tools)
library(pathview)

# Load System Data
# This has since been re-saved to individual files.
load('data/sysdata.rda')

# Load Example Data
load('data/name_map.RData')

# Load KEGG Database Files
# Import Enzymes & Enzyme Names here once scraping has been completed.
load('data/k01_keggEnzymes.RData')
load('data/k03_keggPathways.RData')

# Load MetaCyc Database Files
load('data/m01_metaCycDBLinks.RData')
load('data/m02_metaCycDB.RData')
load('data/m03_metaCycGeneIDs.RData')
load('data/m04_metaCycPathways.RData')

# Load HumanCyc Cross-References
load('data/humanRefs.RData')

# Source Mapping Functions
source('mapGenerally.R', local = TRUE)$value
source('pathwayMapping.R', local = TRUE)$value

# Source App Functions
source('alertFunctions.R', local = TRUE)$value
source('generateTables.R', local = TRUE)$value