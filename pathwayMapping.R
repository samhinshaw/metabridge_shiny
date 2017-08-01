
mapKEGGPathways <- function(idType, selectedRow, summaryTable, fullTable) {
  
  ### Quote necessary variables for dplyr
  
  # To be treated like a variable
  namedIDType <- as.name(idType)
  
  ### Pull the selected row and extract its compound ID
  selectedMetab <- summaryTable[as.numeric(rownames(summaryTable)) == 
                                           selectedRow,] %>% 
    extract2(input$idType)
  
  # To be treated like a character string
  quotedMetab <- rlang::enquo(selectedMetab)
  
  ### Pull the selected row and extract its compound Name
  selectedMetabName <- summaryTable[as.numeric(rownames(summaryTable)) == 
                                               selectedRow,] %>% 
    extract2('Compound')
  
  ### Pull out the pathways that our compound is present in from the
  ### metabPathways object stored in `data/`
  pathwaysOfInterest <- exampleKEGGPathways %>%
    filter(rlang::UQ(namedIDType) == rlang::UQ(quotedMetab))
  
  ## Find all the genes that compound interacts with (from our initial mapping
  ## table)
  genesOfInterest <- mappedMetabolites() %>% 
    filter(rlang::UQ(namedIDType) == rlang::UQ(quotedMetab)) %>% 
    magrittr::extract2("Official Gene Symbol")
  
  return(list(
    'selectedCompound' = selectedMetab,
    'selectedCompoundName' = selectedMetabName,
    'genesOfSelectedCompound' = genesOfInterest,
    'pathwaysOfSelectedCompound' = pathwaysOfInterest
  ))
}

mapMetaCycPathways <- function(idType, selectedRow, summaryTable, fullTable) {
  ###############################################
  #   Get Name out of Summary (clicked) Table   #
  ###############################################
  
  # To be treated like a variable
  namedIDType <- as.name(idType)
  
  ### Pull the selected row and extract its compound ID
  selectedMetab <- summaryTable[as.numeric(rownames(summaryTable)) == 
                                           selectedRow,] %>% 
    extract2(idType)
  
  # To be treated like a character string
  quotedMetab <- rlang::enquo(selectedMetab)
  
  ### Pull the selected row and extract its compound Name
  selectedMetabName <- summaryTable[as.numeric(rownames(summaryTable)) == 
                                               selectedRow,] %>% 
    extract2('Compound')
  
  #######################################
  #  Get Info from Full Mapping Table   #
  #######################################
  
  selectedReaction <- fullTable %>% 
    filter(rlang::UQ(namedIDType) == rlang::UQ(quotedMetab)) %>% 
    extract2('Reaction')
  
  quotedSelectedReaction <- rlang::enquo(selectedReaction)
  
  pathwaysOfInterest <- metaCycPathways %>%
  filter(reaction %in% rlang::UQ(selectedReaction))
  
  alert(paste0("There are ", nrow(pathwaysOfInterest), " pathways."))
  
}

generalPathwayMapping <- function(db, idType, selectedRow, summaryTable, fullTable) {
  if (db == "KEGG") {
    ## Do something
    mapKEGGPathways(idType = idType, selectedRow = selectedRow, 
                    summaryTable = summaryTable, 
                    fullTable = fullTable)
  } else if (db == "MetaCyc") {
    ## Do something else
    mapMetaCycPathways(idType = idType, selectedRow = selectedRow, 
                       summaryTable = summaryTable,
                       fullTable = fullTable)
  }
}