## Load Packages
library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(magrittr)
library(DT)

## Load System Data
load('data/sysdata.rda')
load('data/name_map.RData')

source('mapGenerally.R', local = TRUE)
source('alertFunctions.R', local = TRUE)