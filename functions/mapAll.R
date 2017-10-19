## Define Functions

mapAll <- function(compoundList, db = "both", mapKEGGIDs = TRUE) {
  
  # Pull out all compound IDs based on regular expression matching
  CAS_IDs <- compoundList %>% stringr::str_extract('^[0-9]*-[0-9]{2}-[0-9]{1,2}$') %>%
    unique() %>% notNAs()
  HMDB_IDs <- compoundList %>% stringr::str_extract('^HMDB[0-9]{5}$') %>%
    unique() %>% notNAs()
  KEGG_IDs <- compoundList %>% stringr::str_extract('^[CDcG][0-9]{5}$') %>%
    unique() %>% notNAs()
  PubChem_IDs <- compoundList %>% stringr::str_extract('^[0-9]{1,9}$') %>%
    unique() %>% notNAs()
  
  #######################
  #                     #
  #       MetaCyc       #
  #                     #
  #######################
  
  
  # Now map these to the MetaCyc Object IDs
  
  ## CAS  ##
  metaCycIDs_fromCAS <- metaCycDBLinks %>%
    dplyr::filter_("CAS %in% CAS_IDs") %>%
    magrittr::extract2("Compound") %>% unique() %>% notNAs()
  
  ## HMDB ##
  metaCycIDs_fromHMDB <- metaCycDBLinks %>%
    dplyr::filter_("HMDB %in% HMDB_IDs") %>%
    magrittr::extract2("Compound") %>% unique() %>% notNAs()
  
  ## KEGG ##
  metaCycIDs_fromKEGG <- metaCycDBLinks %>%
    dplyr::filter_("KEGG %in% KEGG_IDs") %>%
    magrittr::extract2("Compound") %>% unique() %>% notNAs()
  
  ## PubChem ##
  metaCycIDs_fromPubChem <- metaCycDBLinks %>%
    dplyr::filter_("PubChem %in% PubChem_IDs") %>%
    magrittr::extract2("Compound") %>% unique() %>% notNAs()
  
  ## Concatenate Results ##
  allMetaCycIDs <- c(metaCycIDs_fromCAS, metaCycIDs_fromHMDB,
                     metaCycIDs_fromKEGG, metaCycIDs_fromPubChem) %>% unique()
  
  # Finally, dplyr::filter the reaction-gene table!
  
  reactionsAndGenesOfInterest <- metaCycDB %>%
    dplyr::filter_("compound %in% allMetaCycIDs")
  
  metaCycGenesOfInterest <- reactionsAndGenesOfInterest %>%
    magrittr::extract2("gene") %>% unique() %>% notNAs()
  
  ## Finally, finally, map biocyc gene IDs to ensembl gene IDs
  
  metaCycEnsemblGenesOfInterest <- metaCycGeneIDs %>%
    dplyr::filter_("`Object ID` %in% metaCycGenesOfInterest") %>%
    magrittr::extract2("Ensembl") %>% unique() %>% notNAs()
  
  #######################
  #                     #
  #        KEGG         #
  #                     #
  #######################
  
  # If we want to, we can take ALL the IDs supplied and map them to KEGG
  # compound IDs via the MetaCyc database. Otherwise, we'll just use the KEGG
  # IDs that were provided
  if (mapKEGGIDs == TRUE) {
    
    keggIDs_fromCAS <- metaCycDBLinks %>%
      dplyr::filter_("CAS %in% CAS_IDs") %>%
      magrittr::extract2("KEGG") %>% unique() %>% notNAs()
    
    keggIDs_fromHMDB <- metaCycDBLinks %>%
      dplyr::filter_("HMDB %in% HMDB_IDs") %>%
      magrittr::extract2("KEGG") %>% unique() %>% notNAs()
    
    keggIDs_fromPubChem <- metaCycDBLinks %>%
      dplyr::filter_("PubChem %in% PubChem_IDs") %>%
      magrittr::extract2("KEGG") %>% unique() %>% notNAs()
    
    KEGG_IDs <- c(KEGG_IDs, keggIDs_fromCAS,
                  keggIDs_fromHMDB, keggIDs_fromPubChem) %>% unique() %>% notNAs()
  }
  
  keggGenesOfInterest <- keggDB %>%
    dplyr::filter_("KEGG %in% KEGG_IDs") %>%
    magrittr::extract2("genes") %>% unique() %>% notNAs()
  
  
  #######################
  #                     #
  #       Return        #
  #                     #
  #######################
  
  if (tolower(db) == 'both') {
    return(list('MetaCyc' = metaCycEnsemblGenesOfInterest, 'KEGG' = keggGenesOfInterest))
  } else if (tolower(db) == 'kegg') {
    return(keggGenesOfInterest)
  } else if (tolower(db) == 'biocyc' | tolower(db) == 'metacyc') {
    return(metaCycEnsemblGenesOfInterest)
  } else {
    stop("Please specify a database: 'kegg', 'metacyc'/'biocyc', or 'both'")
  }
  
}
