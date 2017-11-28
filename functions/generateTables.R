generateSummaryTable <- function(mappingObject, idType, dbChosen) {
  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data)) {
    return(NULL)
  } else if (mappingObject$status == "error" |
             mappingObject$status == "empty") {
    return(mappingObject$data)
  } else if (dbChosen == 'MetaCyc') {
    mappingObject$data %>% group_by_(idType, 'Compound') %>% summarize(
      "# Reactions" = n_distinct(`Reaction`, na.rm = TRUE),
      "# Genes (MetaCyc)" = n_distinct(`MetaCyc Gene`, na.rm = TRUE),
      "# Genes (HGNC)" = n_distinct(`HGNC`, na.rm = TRUE),
      "# Genes (Ensembl)" = n_distinct(`Ensembl`, na.rm = TRUE)
    ) %>% ungroup()
  } else if (dbChosen == 'KEGG') {
    mappingObject$data %>%
      group_by_('KEGG', idType, 'Compound') %>% summarize(
        "# Enzymes" = n_distinct(`Enzyme`, na.rm = TRUE),
        "# Genes (HGNC)" = n_distinct(`HGNC`, na.rm = TRUE),
        "# Genes (Entrez)" = n_distinct(`Entrez`, na.rm = TRUE)
      ) %>% ungroup()
  }
}

generateMetaCycMetabTable <- function(mappingObject,
                                      summaryTable,
                                      selectedRows,
                                      idType) {
    # Should never be null since we're not responding until map button is
    # clicked, but good to have just in case
    if (is.null(mappingObject$data) |
        is.null(selectedRows) | is.null(summaryTable)) {
      return(data.frame())
    } else {
      ### Quote necessary variables for dplyr
      namedIDType <- as.name(idType)
      quotedIDType <- rlang::quo(idType)
      pastedIDType <- paste0(idType)
      
      ### Pull the selected row and extract its compound ID
      selectedMetab <-
        summaryTable[as.numeric(rownames(summaryTable)) ==
                       selectedRows, ]
      
      selectedMetab %<>%
        extract2(pastedIDType)
      
      quotedSelectedMetab <- rlang::enquo(selectedMetab)
      
      filteredMappedMetaboliteTable <- mappingObject$data %>%
        dplyr::filter(rlang::UQ(namedIDType) == rlang::UQ(quotedSelectedMetab))
      
      return(filteredMappedMetaboliteTable)
    }
  }

generateKEGGMetabTable <- function(mappingObject,
                                   summaryTable,
                                   selectedRows,
                                   idType) {
    # Should never be null since we're not responding until map button is
    # clicked, but good to have just in case
    if (is.null(mappingObject$data) |
        is.null(selectedRows) | is.null(summaryTable)) {
      return(data.frame())
    } else {
      ### Quote necessary variables for dplyr
      namedIDType <- as.name(idType)
      quotedIDType <- rlang::quo(idType)
      pastedIDType <- paste0(idType)
      
      ### Pull the selected row and extract its compound ID
      selectedMetab <-
        summaryTable[as.numeric(rownames(summaryTable)) ==
                       selectedRows, ]
      
      # If mapped against the KEGG database, pull out the KEGG cpd ID (even if
      # not what was supplied), and extract the ID from the HTML contents of the
      # cell
      selectedMetab %<>%
        extract2('KEGG') %>% 
        # make sure we've just got the kegg compound ID
        str_extract('C[0-9]{5}')
      
      quotedSelectedMetab <- rlang::enquo(selectedMetab)
      
      namedIDType <- as.name('KEGG')
      
      filteredMappedMetaboliteTable <- mappingObject$data %>%
        dplyr::filter(rlang::UQ(namedIDType) == rlang::UQ(quotedSelectedMetab))
      
      
      return(filteredMappedMetaboliteTable)
    }
  }

# generateFullTable <- function(mappingObject) {
#
# }