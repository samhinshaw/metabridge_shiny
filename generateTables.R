generateSummaryTable <- function(mappingObject, idType, dbChosen) {
  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data)) {
    return(NULL)
  } else if (mappingObject$status == "error" | mappingObject$status == "empty") {
    return(mappingObject$data)
  } else if (dbChosen == 'MetaCyc') {
    mappingObject$data %>% group_by_(idType, 'Compound') %>% summarize(
      "# of Reactions"                    = n_distinct(Reaction,               na.rm = TRUE),
      "# of Genes (MetaCyc Gene ID)"      = n_distinct(`MetaCyc Gene ID`,      na.rm = TRUE),
      "# of Genes (Official Gene Symbol)" = n_distinct(`Official Gene Symbol`, na.rm = TRUE),
      "# of Genes (Ensembl Gene ID)"      = n_distinct(`Ensembl Gene ID`,      na.rm = TRUE)
    )
  } else if (dbChosen == 'KEGG') {
    mappingObject$data %>% 
      group_by_('Compound', idType, 'KEGG') %>% summarize(
        "# of Enzymes" = n_distinct(Enzyme, na.rm = TRUE),
        "# of Genes"   = n_distinct(Gene, na.rm = TRUE)
      )
  }
}

generateSelectedMetabTable <- function(mappingObject, summaryTable, selectedRows, dbChosen) {
  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data) | is.null(selectedRows) | is.null(summaryTable)) {
    return(NULL)
  } else if (){
    return(NULL)
  } else {
    
    ### Quote necessary variables for dplyr
    namedIDType <- as.name(idTypeOfInterest())
    quotedIDType <- rlang::quo(idTypeOfInterest())
    pastedIDType <- paste0(idTypeOfInterest())
    pastedDB <- paste0(dbChosen)
    
    ### Pull the selected row and extract its compound ID
    selectedMetab <- summaryTable[as.numeric(rownames(summaryTable)) == 
                                             selectedRows,]
    
    # If mapped against the KEGG database, pull out the KEGG cpd ID (even if
    # not what was supplied), and extract the ID from the HTML contents of the
    # cell
    if (pastedDB == 'KEGG') {
      selectedMetab %<>%
        extract2('KEGG') %>% str_extract('C[0-9]{5}')
      
      quotedSelectedMetab <- rlang::enquo(selectedMetab)
      
      namedIDType <- as.name('bareKEGG')
      
      filteredMappedMetaboliteTable <- mappedMetabolites() %>%
        filter(rlang::UQ(namedIDType) == rlang::UQ(quotedSelectedMetab))
    } else {
      selectedMetab %<>% 
        extract2(pastedIDType)
      
      quotedSelectedMetab <- rlang::enquo(selectedMetab)
      
      filteredMappedMetaboliteTable <- mappedMetabolites() %>%
        filter(rlang::UQ(namedIDType) == rlang::UQ(quotedSelectedMetab))
    }
    
    return(filteredMappedMetaboliteTable)
  }
}