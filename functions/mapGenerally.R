# Only load packages on demand
library(stringr)
library(magrittr)

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
    this <-
      data_frame(
        UQ(idType) := importDF %>% use_series(UQ(col)) %>% as.character() %>% notNAs() %>% notEmpty() %>% str_trim()
      )
    # names(this)[1] <- UQ(quotedID)
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = 'empty',
        data = importDF,
        message = 'We were unable to properly import your data.',
        suggest = 'Try changing your mapping parameters.'
      )
    } else {
      list(
        status = 'success',
        data = this,
        message = 'Your metabolites have been successfully mapped!',
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = 'warn',
      data = this,
      internalMessage = warningMessage,
      message = 'We were unable to properly import your data.',
      suggest = 'Try changing your mapping parameters.'
    )
  }, error = function(errorMessage) {
    list(
      status = 'error',
      data = importDF,
      internalMessage = errorMessage,
      message = 'We were unable to properly import your data.',
      suggest = 'Try changing your mapping parameters.'
    )
  })
  # NOTE:
  # Everything in finally should be executed at the end,
  # regardless of success or error.
  
  if (mappingDF$status == 'error' | mappingDF$status == 'empty') {
    return(mappingDF)
  }
  
  # Now map these to the MetaCyc Object IDs
  mappedToObjects <- tryCatch({
    this <-
      inner_join(mappingDF$data, metaCycDBLinks, by = UQ(idType)) %>%
      dplyr::select_(idType, "Compound") %>% rename_("compound" = "Compound")
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = 'empty',
        data = mappingDF$data,
        message = paste0(
          'We were unable to find any matches in the MetaCyc ',
          'database for the compound IDs you provided.'
        ),
        suggest = 'Try using a different compound ID or mapping via KEGG.'
      )
    } else {
      list(
        status = 'success',
        data = this,
        message = 'Your metabolites have been successfully mapped!',
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = 'warn',
      data = this,
      internalMessage = warningMessage,
      message = 'There was an unspecified error in mapping your compounds.',
      suggest = NULL
    )
  }, error = function(errorMessage) {
    list(
      status = 'error',
      data = mappingDF$data,
      internalMessage = errorMessage,
      message = 'We were unable to map your metabolites to MetaCyc Compound IDs.',
      suggest = 'Try changing your mapping parameters.'
    )
  })
  
  # If tryCatch exited with status != 0, stop here
  if (mappedToObjects$status == 'error' |
      mappedToObjects$status == 'empty') {
    return(mappedToObjects)
  }
  
  # Finally, join the reaction-gene table!
  mappedToReactions <- tryCatch({
    this <-
      inner_join(mappedToObjects$data, metaCycDB, by = "compound") %>%
      rename_(
        "Reaction" = "reaction",
        "Compound" = "compound",
        "MetaCyc Gene ID" = "gene",
        "Official Gene Symbol" = "geneName"
      )
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = 'empty',
        data = mappedToObjects$data,
        message = 'We were unable to map your compounds to any reactions.',
        suggest = 'Try using a different compound ID or mapping via KEGG.'
      )
    } else {
      list(
        status = 'success',
        data = this,
        message = 'Your metabolites have been successfully mapped!',
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = 'warn',
      data = this,
      internalMessage = warningMessage,
      message = 'Your compounds were mapped, but there may have been a problem.',
      suggest = NULL
    )
  }, error = function(errorMessage) {
    list(
      status = 'error',
      data = mappedToObjects$data,
      internalMessage = errorMessage,
      message = 'We were unable to map your compounds to any reactions.',
      suggest = 'Try changing your mapping parameters.'
    )
  })
  
  if (mappedToReactions$status == 'error' |
      mappedToReactions$status == 'empty') {
    return(mappedToReactions)
  }
  
  ## Finally, finally, map biocyc gene IDs to ensembl gene IDs
  mappedToEnsembl <- tryCatch({
    this <- left_join(mappedToReactions$data,
                      metaCycGeneIDs,
                      by = c("MetaCyc Gene ID" = "Object ID")) %>%
      dplyr::select_(
        idType,
        "Compound",
        "`MetaCyc Gene ID`",
        "`Official Gene Symbol`",
        "Ensembl",
        "Reaction"
      ) %>%
      rename_("Ensembl Gene ID" = "Ensembl") %>%
      # filter out rows where no gene IDs are present
      filter(!(
        is.na(`MetaCyc Gene ID`) &
          is.na(`Official Gene Symbol`) & is.na(`Ensembl Gene ID`)
      ))
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = 'empty',
        data = mappedToReactions$data,
        message = paste0(
          'We were unable to find any matches for ',
          'human gene IDs given the enzymes mapped.'
        ),
        suggest = 'Try using a different compound ID or mapping via KEGG.'
      )
    } else {
      list(
        status = 'success',
        data = this,
        message = 'Your metabolites have been successfully mapped!',
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = 'warn',
      data = this,
      internalMessage = warningMessage,
      message = 'Your compounds were mapped, but there may have been a problem.',
      suggest = NULL
    )
  }, error = function(errorMessage) {
    list(
      status = 'error',
      data = mappedToReactions$data,
      internalMessage = errorMessage,
      message = 'There was an error mapping your compounds to human gene IDs.',
      suggest = 'Try changing your mapping parameters.'
    )
  })
  
  return(mappedToEnsembl)
}

#######################
#                     #
#        KEGG         #
#                     #
#######################

mapKEGG <- function(importDF, col, idType) {
  quotedIDtype <- rlang::enquo(idType)
  namedIDtype <- as.name(idType)
  
  keggName <- as.name('KEGG')
  keggQuote <- rlang::quo('KEGG')
  
  # If KEGG compound IDs were not supplied, we'll use the MetaCyc database to
  # map the given IDs to their KEGG compound IDs
  
  mappingDF <- tryCatch({
    # The return value in this chunk is the actual value
    # that will be returned in case there is no condition
    # (e.g. warning or error).
    # You don't need to state the return value via `return()` as code
    # in the "try" part is not wrapped insided a function (unlike that
    # for the condition handlers for warnings and error below)
    this <-
      data_frame(
        UQ(namedIDtype) := extract2(importDF, col) %>% notNAs() %>% notEmpty() %>% str_trim()
      )
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = 'empty',
        data = importDF,
        message = 'We were unable to properly import your data.',
        suggest = 'Try changing your mapping parameters.'
      )
    } else {
      list(
        status = 'success',
        data = this,
        message = 'Your metabolites have been successfully mapped!',
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = 'warn',
      data = this,
      internalMessage = warningMessage,
      message = 'We were unable to properly import your data.',
      suggest = 'Try changing your mapping parameters.'
    )
  }, error = function(errorMessage) {
    list(
      status = 'error',
      data = importDF,
      internalMessage = errorMessage,
      message = 'We were unable to properly import your data.',
      suggest = 'Try changing your mapping parameters.'
    )
  })
  
  if (idType != "KEGG") {
    keggIDs <- tryCatch({
      this <- metaCycDBLinks %>%
        dplyr::filter(UQ(namedIDtype) %in% extract2(mappingDF$data, UQ(quotedIDtype)))
      
      # Check to see if join failed silently
      if (nrow(this) == 0) {
        list(
          status = 'empty',
          data = importDF,
          message = paste0(
            'We were unable to map the ',
            idType,
            ' IDs you provided to KEGG compound IDs.'
          ),
          suggest = 'Try using a different compound ID or mapping via MetaCyc'
        )
      } else {
        list(
          status = 'success',
          data = this,
          message = 'Your metabolites have been successfully mapped!',
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = 'warn',
        data = this,
        internalMessage = warningMessage,
        message = 'Your compounds were mapped, but there may have been a problem.',
        suggest = NULL
      )
    }, error = function(errorMessage) {
      list(
        status = 'error',
        data = importDF,
        internalMessage = errorMessage,
        message = paste0(
          'We were unable to map the ',
          idType,
          ' IDs you provided to KEGG compound IDs.'
        ),
        suggest = 'Try using a different compound ID or mapping via MetaCyc'
      )
    })
  } else if (idType == "KEGG") {
    ## Join compound name (to be scraped) to compound IDs here. Name the column 'Compound'
    this <- left_join(mappingDF$data, keggCompounds, by = 'KEGG')
    keggIDs <- list(
      status = 'success',
      data = this,
      message = 'Your metabolites have been successfully mapped!',
      suggest = NULL
    )
    
  }
  
  keggEnzymesOfInterest <- tryCatch({
    this <- left_join(keggIDs$data, keggEnzymeNames, by = "KEGG")
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = 'empty',
        data = keggIDs$data,
        message = paste0(
          'We were unable to find any matches for ',
          'the compounds you supplied. Here are the KEGG ',
          'compound IDs we queried.'
        ),
        suggest = 'Try using a different compound ID or mapping via MetaCyc'
      )
    } else {
      list(
        status = 'success',
        data = this,
        message = 'Your metabolites have been successfully mapped!',
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = 'warn',
      data = this,
      internalMessage = warningMessage,
      message = 'Your compounds were mapped, but there may have been a problem.',
      suggest = NULL
    )
  }, error = function(errorMessage) {
    list(
      status = 'error',
      data = keggIDs$data,
      internalMessage = errorMessage,
      message = paste0(
        'There was an error mapping your compounds via KEGG. ',
        'Here are the KEGG compound IDs we queried.'
      ),
      suggest = 'Try changing your mapping parameters.'
    )
  })
  
  keggGenesOfInterest <- tryCatch({
    keggGeneDB <- keggDB %>% select_('KEGG', 'genes')
    this <-
      left_join(keggEnzymesOfInterest$data, keggGeneDB, by = "KEGG") %>%
      rename_(
        'Gene' = 'genes',
        'bareKEGG' = 'KEGG',
        'bareEnzyme' = 'enzymes',
        'Enzyme Name' = 'enzymeName'
      ) %>%
      mutate(
        KEGG = paste0(
          '<a target="_blank" href="http://www.genome.jp/dbget-bin/www_bget?cpd:',
          bareKEGG,
          '">',
          bareKEGG,
          '</a>'
        )
      ) %>%
      mutate(
        Enzyme = paste0(
          '<a target="_blank" href="http://www.genome.jp/dbget-bin/www_bget?ec:',
          bareEnzyme,
          '">',
          bareEnzyme,
          '</a>'
        )
      ) %>%
      select_(
        'KEGG',
        idType,
        'Compound',
        'Enzyme',
        '`Enzyme Name`',
        'Gene',
        'bareKEGG',
        'bareEnzyme'
      )
    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = 'empty',
        data = keggEnzymesOfInterest$data,
        message = paste0(
          'We were unable to match the enzymes your compounds ',
          'interact with to any human genes. Here are the enzymes ',
          'and their directly interacting enzymes. '
        ),
        suggest = 'Try using a different compound ID or mapping via MetaCyc'
      )
    } else {
      list(
        status = 'success',
        data = this,
        message = 'Your metabolites have been successfully mapped!',
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = 'warn',
      data = this,
      internalMessage = warningMessage,
      message = 'Your compounds were mapped, but there may have been a problem.',
      suggest = NULL
    )
  }, error = function(errorMessage) {
    list(
      status = 'error',
      data = keggEnzymesOfInterest$data,
      internalMessage = errorMessage,
      message = paste0(
        'There was an error mapping your compounds via KEGG. ',
        'Here are the enzymes ',
        'and their directly interacting enzymes. '
      ),
      suggest = 'Try changing your mapping parameters.'
    )
  })
  
  
  
  
  return(keggGenesOfInterest)
}


#######################################################
#                                                     #
#        Map Generally Against Either Database        #
#                                                     #
#######################################################

mapGenerally <- function(importDF, col, db, idType) {
  if (db == "KEGG") {
    mappedMetabolites <-
      mapKEGG(importDF = importDF,
              col = col,
              idType = idType)
  } else if (db == "MetaCyc") {
    mappedMetabolites <-
      mapMetaCyc(importDF = importDF,
                 col = col,
                 idType = idType)
  } else {
    mappingAlert(
      status  = "error",
      message = paste0(
        "Something went wrong when mapping generally, ",
        "probably an error with the database parameter."
      ),
      suggest = "Please tweet to us to report this issue!"
    )
  }
  return(mappedMetabolites)
}