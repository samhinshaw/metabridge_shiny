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
  
  # Note: in tryCatch, finally is always evaluated! 
  mappingDF <- tryCatch({
    # The return value in this chunk is the actual value 
    # that will be returned in case there is no condition 
    # (e.g. warning or error). 
    # You don't need to state the return value via `return()` as code 
    # in the "try" part is not wrapped insided a function (unlike that
    # for the condition handlers for warnings and error below)
    this <- data_frame(UQ(col) := importDF %>% use_series(UQ(col)) %>% as.character())
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(status = 'empty', data = importDF, 
           message = 'We were unable to properly import your data.',
           suggest = 'Try changing your mapping parameters.')
    } else {
      list(status = 'success', data = this, 
           message = 'Your metabolites have been successfully mapped!', 
           suggest = NULL)
    }
  }, warning = function(warningMessage) {
    list(status = 'warn', data = this, 
         message = 'We were unable to properly import your data.',
         suggest = 'Try changing your mapping parameters.')
  }, error = function(errorMessage) {
    list(status = 'error', data = importDF, 
         message = 'We were unable to properly import your data.',
         suggest = 'Try changing your mapping parameters.')
  })
  # NOTE:
  # Everything in finally should be executed at the end,
  # regardless of success or error.
  
  if (mappingDF$status == 'error' | mappingDF$status == 'empty') {
    return(mappingDF)
  }
  
  # Now map these to the MetaCyc Object IDs
  mappedToObjects <- tryCatch({
    this <- inner_join(mappingDF$data, metaCycDBLinks, by = setNames(nm = c(col = idType))) %>% 
      select_(col, "Compound") %>% rename_("compound" = "Compound")
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(status = 'empty', data = mappingDF$data, 
           message = paste0('We were unable to find any matches in the MetaCyc ',
                            'database for the compound IDs you provided.'), 
           suggest = 'Try using a different compound ID or mapping via KEGG.')
    } else {
      list(status = 'success', data = this, 
           message = 'Your metabolites have been successfully mapped!', 
           suggest = NULL)
    }
  }, warning = function(warningMessage) {
    list(status = 'warn', data = this, 
         message = 'There was an unspecified error in mapping your compounds.',
         suggest = NULL)
  }, error = function(errorMessage) {
    list(status = 'error', data = mappingDF$data, 
         message = 'We were unable to map your metabolites to MetaCyc Compound IDs.', 
         suggest = 'Try changing your mapping parameters.')
  })
  
  # If tryCatch exited with status != 0, stop here
  if (mappedToObjects$status == 'error' | mappedToObjects$status == 'empty') {
    return(mappedToObjects)
  }

  # Finally, join the reaction-gene table!
  mappedToReactions <- tryCatch({
    this <- inner_join(mappedToObjects$data, metaCycDB, by = "compound")
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(status = 'empty', data = mappedToObjects$data, 
           message = 'We were unable to map your compounds to any reactions.', 
           suggest = 'Try using a different compound ID or mapping via KEGG.')
    } else {
      list(status = 'success', data = this, 
           message = 'Your metabolites have been successfully mapped!', 
           suggest = NULL)
    }
  }, warning = function(warningMessage) {
    list(status = 'warn', data = this, 
         message = 'Your compounds were mapped, but there may have been a problem.', 
         suggest = NULL)
  }, error = function(errorMessage) {
    list(status = 'error', data = mappedToObjects$data, 
         message = 'We were unable to map your compounds to any reactions.', 
         suggest = 'Try changing your mapping parameters.')
  })
  
  if (mappedToReactions$status == 'error' | mappedToReactions$status == 'empty') {
    return(mappedToReactions)
  }

    ## Finally, finally, map biocyc gene IDs to ensembl gene IDs
  mappedToEnsembl <- tryCatch({
    this <- inner_join(mappedToReactions$data, metaCycGeneIDs,
                       by = c("gene" = "Object ID"))
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(status = 'empty', data = mappedToReactions$data, 
           message = paste0('We were unable to find any matches for ',
                            'human gene IDs given the enzymes mapped.'), 
           suggest = 'Try using a different compound ID or mapping via KEGG.')
    } else {
      list(status = 'success', data = this, 
           message = 'Your metabolites have been successfully mapped!', 
           suggest = NULL)
    }  
  }, warning = function(warningMessage) {
    list(status = 'warn', data = this, 
         message = 'Your compounds were mapped, but there may have been a problem.', 
         suggest = NULL)
  }, error = function(errorMessage) {
    list(status = 'error', data = mappedToReactions$data, 
         error = 'There was an error mapping your compounds to human gene IDs.',
         suggest = 'Try changing your mapping parameters.')
  })

  return(mappedToEnsembl)
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