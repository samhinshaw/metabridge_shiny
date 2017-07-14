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
  
  ## Suppress Errors for this chunk, because we already know that there may be
  ## some mapping issues cropping up with our joins.
  
  suppressWarnings({
    # Extract the column of interest (col) and construct a new tibble with which 
    # we will mape our metabolites to enzymes and then genes. As character just in
    # case IDs such as PubChem IDs are interpreted as integers
    mappingDF <- data_frame(UQ(col) := importDF %>% use_series(UQ(col)) %>% as.character())

        # Now map these to the MetaCyc Object IDs
    mappedDF <- inner_join(mappingDF, metaCycDBLinks, by = setNames(nm = c(col = idType))) %>% 
      select_(col, "Compound") %>% rename_("compound" = "Compound")
    
    # Finally, join the reaction-gene table!
    
    fullyMappedDF <- inner_join(mappedDF, metaCycDB, by = "compound")
    
    
    ## Finally, finally, map biocyc gene IDs to ensembl gene IDs
    
    mappedDFEnsembl <- inner_join(fullyMappedDF, metaCycGeneIDs, 
                                  by = c("gene" = "Object ID"))
  })
  
  
  
  ## Now we step up the chian!
  if (nrow(mappedDFEnsembl) != 0) {
    return(mappedDFEnsembl)
  } else if (nrow(fullyMappedDF) != 0) {
    return(fullyMappedDF)
  } else if (nrow(mappedDF) != 0) {
    return(mappedDF)
  } else if (nrow(mappingDF) != 0) {
    return(mappingDF)
  }
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
    suppressWarnings({
      keggIDs <- metaCycDBLinks %>%
        dplyr::filter_(paste0(col, " %in% importedVector")) %>% ## This should use rlang
        magrittr::extract2("KEGG") %>% unique()
      keggIDsToMap <- keggIDs
    })
  } else if (idType == "KEGG") {
    keggIDsToMap <- importedVector
  } else {
    stop("Something went wrong when mapping against the KEGG database Probably an error with the idType parameter.")
  }
  suppressWarnings({
    keggGenesOfInterest <- keggDB %>%
      dplyr::filter_("KEGG %in% importedVector")
  })
  
  if (nrow(keggGenesOfInterest) != 0) {
    return(keggGenesOfInterest)
  } else if (nrow(keggIDsToMap) != 0) {
    return(keggIDsToMap)
  } else {
    return(importDF %>% magrittr::use_series(col))
  }
}

mapGenerally <- function(importDF, col, db, idType) {
  if (db == "KEGG") {
    mappedMetabolites <- mapKEGG(importDF = importDF, col = col, idType = idType)
  } else if (db == "MetaCyc") {
    mappedMetabolites <- mapMetaCyc(importDF = importDF, col = col, idType = idType)
  } else {
    stop("Something went wrong when mapping generally. Probably an error with the database parameter.")
  }
  return(mappedMetabolites)
}