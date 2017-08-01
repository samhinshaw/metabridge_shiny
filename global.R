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

## Load System Data
load('data/sysdata.rda')

##### Read In Example KEGG Pathways ######
load('data/examplePathways.RData') 
# Will load full pathway set here
##########################################

load('data/name_map.RData')
load(file = 'data/reactions_pathways_readable.RData')
load(file = 'data/reactions_pathways.RData')

source('mapGenerally.R', local = TRUE)$value
source('pathwayMapping.R', local = TRUE)$value
source('alertFunctions.R', local = TRUE)$value
