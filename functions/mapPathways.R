

mapKEGGPathways <-
  function(idType,
           selectedRow,
           summaryTable,
           fullTable) {
    ### Quote necessary variables for dplyr
    
    # To be treated like a variable
    namedIDType <- as.name(idType)
    bareKEGG <- as.name('bareKEGG')
    
    ### Pull the selected row and extract its compound ID
    selectedMetab <-
      summaryTable[as.numeric(rownames(summaryTable)) ==
                     selectedRow, ] %>%
      extract2(idType) %>% str_extract('C[0-9]{5}')
    
    # To be treated like a character string
    quotedMetab <- rlang::enquo(selectedMetab)
    
    ### Pull the selected row and extract its compound Name
    selectedMetabName <-
      summaryTable[as.numeric(rownames(summaryTable)) ==
                     selectedRow, ] %>%
      extract2('Compound')
    
    ### Pull out the pathways that our compound is present in from the
    ### metabPathways object stored in `data/`
    pathwaysOfInterest <- keggPathways %>%
      filter(rlang::UQ(namedIDType) == rlang::UQ(quotedMetab)) %>%
      filter_("id %in% keggHumanPathways")
    
    ## Find all the genes that compound interacts with (from our initial mapping
    ## table)
    genesOfInterest <- fullTable %>%
      filter(rlang::UQ(bareKEGG) == rlang::UQ(quotedMetab)) %>%
      magrittr::extract2("Gene")
    
    return(
      list(
        'selectedCompound' = selectedMetab,
        'selectedCompoundName' = selectedMetabName,
        'genesOfSelectedCompound' = genesOfInterest,
        'pathwaysOfSelectedCompound' = pathwaysOfInterest
      )
    )
  }

mapMetaCycPathways <-
  function(idType,
           selectedRow,
           summaryTable,
           fullTable) {
    ###############################################
    #   Get Name out of Summary (clicked) Table   #
    ###############################################
    
    # To be treated like a variable
    namedIDType <- as.name(idType)
    
    ### Pull the selected row and extract its compound ID
    selectedMetab <-
      summaryTable[as.numeric(rownames(summaryTable)) ==
                     selectedRow, ] %>%
      extract2(idType)
    
    # To be treated like a character string
    quotedMetab <- rlang::enquo(selectedMetab)
    
    ### Pull the selected row and extract its compound Name
    selectedMetabName <-
      summaryTable[as.numeric(rownames(summaryTable)) ==
                     selectedRow, ] %>%
      extract2('Compound')
    
    #######################################
    #  Get Info from Full Mapping Table   #
    #######################################
    
    genesOfInterest <- fullTable %>%
      filter(rlang::UQ(namedIDType) == rlang::UQ(quotedMetab)) %>%
      magrittr::extract2("Official Gene Symbol")
    
    selectedReaction <- fullTable %>%
      filter(rlang::UQ(namedIDType) == rlang::UQ(quotedMetab)) %>%
      extract2('Reaction')
    
    quotedSelectedReaction <- rlang::enquo(selectedReaction)
    
    pathwaysOfInterest <- metaCycPathways %>%
      filter(reaction %in% rlang::UQ(selectedReaction))
    
    return(
      list(
        'selectedCompound' = selectedMetab,
        'selectedCompoundName' = selectedMetabName,
        'genesOfSelectedCompound' = genesOfInterest,
        'pathwaysOfSelectedCompound' = pathwaysOfInterest
      )
    )
    
  }

generalPathwayMapping <-
  function(db,
           idType,
           selectedRow,
           summaryTable,
           fullTable) {
    if (db == "KEGG") {
      ## If KEGG was chosen, just use the KEGG Compound IDs
      mapKEGGPathways(
        idType = 'KEGG',
        selectedRow = selectedRow,
        summaryTable = summaryTable,
        fullTable = fullTable
      )
    } else if (db == "MetaCyc") {
      ## If MetaCyc was chosen, use the selected ID Type
      mapMetaCycPathways(
        idType = idType,
        selectedRow = selectedRow,
        summaryTable = summaryTable,
        fullTable = fullTable
      )
    }
  }