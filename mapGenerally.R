## Define Functions

mapMetaCyc <- function(importDF, col, idType) {
  
  enquotedCol <- enquo(col)
  quotedCol <- quo(col)
  quotedID <- quo(idType)
  
  #######################
  #                     #
  #       MetaCyc       #
  #                     #
  #######################
  
  ## Let's attempt using tryCatch to handle errors for each joining step.
  
    # Extract the column of interest (col) and construct a new tibble with which 
    # we will mape our metabolites to enzymes and then genes. As character just in
    # case IDs such as PubChem IDs are interpreted as integers
  mappingDF <- tryCatch({
    mappingDF <- data_frame(UQ(col) := importDF %>% use_series(UQ(col)) %>% as.character())
  }, warning = function(warningMessage) {
    warningMessage
  }, error = function(errorMessage) {
    errorMessage
  }, finally = {
    mappingDF
  })
  
  # If exited with error, return previous step with relevant error message
  if (!is.data.frame(mappingDF)) {
    mappingAlert('We were unable to properly import your data.', paste0(mappingDF))
    return(importDF)
  }
  if (nrow(mappingDF) == 0) {
    mappingAlert('We were unable to properly import your data.', paste0(mappingDF))
    return(importDF)
  }
  
  # Now map these to the MetaCyc Object IDs
  mappedDF <- tryCatch({
    mappedDF <- inner_join(mappingDF, metaCycDBLinks, by = setNames(nm = c(col = idType))) %>% 
      select_(col, "Compound") %>% rename_("compound" = "Compound")
  }, warning = function(warningMessage) {
    warningMessage
  }, error = function(errorMessage) {
    errorMessage
  }, finally = {
    mappedDF
  })
  
  # If exited with error, return previous step with relevant error message
  if (!is.data.frame(mappedDF)) {
    mappingAlert('We were unable to map your metabolites to MetaCyc Compound IDs.', paste0(mappedDF))
    return(mappingDF)
  }
  if (nrow(mappedDF) == 0) {
    mappingAlert(paste0('We were unable to find any matches in the MetaCyc ',
                        'database for the compound IDs you provided.'), paste0(mappedDF))
    # cat(mappedDF)
    return(mappingDF)
  }
  
  # Finally, join the reaction-gene table!
  fullyMappedDF <- tryCatch({
    fullyMappedDF <- inner_join(mappedDF, metaCycDB, by = "compound")
  }, warning = function(w) {
    'error'
  }, error = function(e) {
    'error'
  }, finally = {
    fullyMappedDF
  })
  
  # If exited with error, return previous step with relevant error message
  if (!is.data.frame(fullyMappedDF)) {
    mappingAlert('We were unable to map your compounds to any reactions.', paste0(fullyMappedDF))
    return(mappedDF)
  }
  if (nrow(fullyMappedDF) == 0) {
    mappingAlert('We were unable to map your compounds to any reactions.', paste0(fullyMappedDF))
    return(mappedDF)
  }
  
    ## Finally, finally, map biocyc gene IDs to ensembl gene IDs
  mappedDFEnsembl <- tryCatch({
    mappedDFEnsembl <- inner_join(fullyMappedDF, metaCycGeneIDs, 
                                  by = c("gene" = "Object ID"))
  }, warning = function(warningMessage) {
    warningMessage
  }, error = function(errorMessage) {
    errorMessage
  }, finally = {
    mappedDFEnsembl
  })
  
  # If exited with error, return previous step with relevant error message
  if (!is.data.frame(mappedDFEnsembl)) {
    mappingAlert('There was an error mapping your compounds to human gene IDs.', paste0(mappedDFEnsembl))
    return(fullyMappedDF)
  }
  if (nrow(mappedDFEnsembl) == 0) {
    mappingAlert(paste0('We were unable to find any matches for ',
                        'human gene IDs given the enzymes mapped.'), paste0(mappedDFEnsembl))
    return(fullyMappedDF)
  }
  
  # print('Returning Final Step Mapped DF')
  return(mappedDFEnsembl)
}

mapKEGG <- function(importDF, col, idType) {
  #######################
  #                     #
  #        KEGG         #
  #                     #
  #######################
  
  importedVector <- extract2(importDF, col)
  
  # If KEGG compound IDs were not supplied, we'll use the MetaCyc database to
  # map the given IDs to their KEGG compound IDs
  
  if (idType != "KEGG") {
    keggIDs <- metaCycDBLinks %>%
      ## This `filter_()` should use rlang
      dplyr::filter_(paste0(col, " %in% importedVector")) %>% 
      magrittr::extract2("KEGG") %>% unique()
    keggIDsToMap <- keggIDs
  } else if (idType == "KEGG") {
    keggIDsToMap <- importedVector
  } else {
    alert("Something went wrong when mapping against the KEGG database.
          Probably an error with the idType parameter.")
  }
  
  keggGenesOfInterest <- keggDB %>%
    dplyr::filter_("KEGG %in% importedVector") %>% 
    rename_('Enzyme' = 'enzymes', 'Compound' = 'KEGG', 'Gene' = 'genes') %>% 
    mutate(Compound = paste0('<a target="_blank" href="http://www.genome.jp/dbget-bin/www_bget?cpd:',
                             Compound, '">', Compound, '</a>')) %>% 
    mutate(Enzyme = paste0('<a target="_blank" href="http://www.genome.jp/dbget-bin/www_bget?ec:', 
                           Enzyme, '">', Enzyme, '</a>'))
  
  if (nrow(keggGenesOfInterest) != 0) {
    return(keggGenesOfInterest)
  } else if (nrow(keggIDsToMap) != 0) {
    return(keggIDsToMap)
  } else {
    return(importDF %>% extract2(col))
  }
}


#######################################################
#                                                     #
#        Map Generally Against EitheE Database        #
#                                                     #
#######################################################

mapGenerally <- function(importDF, col, db, idType) {
  if (db == "KEGG") {
    mappedMetabolites <- mapKEGG(importDF = importDF, col = col, idType = idType)
  } else if (db == "MetaCyc") {
    mappedMetabolites <- mapMetaCyc(importDF = importDF, col = col, idType = idType)
  } else {
    alert("Something went wrong when mapping generally
          Probably an error with the database parameter.")
  }
  return(mappedMetabolites)
}