generateSummaryTable <- function(mappingObject, idType, dbChosen) {
  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data)) {
    return(NULL)
  } else if (mappingObject$status == "error" |
    mappingObject$status == "empty") {
    return(mappingObject$data)
  } else if (dbChosen == "MetaCyc") {
    table <- mappingObject$data %>% group_by_(idType, "Compound") %>% summarize(
      "# Reactions" = n_distinct(`Reaction`, na.rm = TRUE),
      "# Genes (MetaCyc)" = n_distinct(`MetaCyc Gene`, na.rm = TRUE),
      "# Gene Names" = n_distinct(`Gene Name`, na.rm = TRUE),
      "# Genes (Ensembl)" = n_distinct(`Ensembl`, na.rm = TRUE)
    ) %>% ungroup()
    return(
      list(
        "table" = table,
        "dbChosen" = "MetaCyc"
      )
    )
  } else if (dbChosen == "KEGG") {
    table <- mappingObject$data %>%
      group_by_("KEGG", idType, "Compound") %>%
      summarize(
        "# Enzymes" = n_distinct(`Enzyme`, na.rm = TRUE),
        "# Gene Names" = n_distinct(`Gene Name`, na.rm = TRUE),
        "# Genes (Entrez)" = n_distinct(`Entrez`, na.rm = TRUE)
      ) %>%
      ungroup()
    return(
      list(
        "table" = table,
        "dbChosen" = "KEGG"
      )
    )
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
      summaryTable[as.numeric(rownames(summaryTable)) == selectedRows, ]

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
      extract2("KEGG") %>%
      # make sure we've just got the kegg compound ID
      str_extract("C[0-9]{5}")

    quotedSelectedMetab <- rlang::enquo(selectedMetab)

    namedIDType <- as.name("KEGG")

    filteredMappedMetaboliteTable <- mappingObject$data %>%
      dplyr::filter(rlang::UQ(namedIDType) == rlang::UQ(quotedSelectedMetab))


    return(filteredMappedMetaboliteTable)
  }
}

hyperlinkTable <- function(table, dbChosen) {
  # keggCompoundURL <- "http://www.genome.jp/dbget-bin/www_bget?cpd:"
  # keggEnzymeURL <- "http://www.genome.jp/dbget-bin/www_bget?ec:"

  # hmdbURL <- "http://www.hmdb.ca/metabolites/"

  # metaCycCompoundURL <- "https://metacyc.org/compound?orgid=META&id="
  # metaCycReactionURL <- "https://metacyc.org/META/NEW-IMAGE?type=REACTION&object="
  # metaCycGeneURL <- "https://metacyc.org/gene?orgid=META&id="

  if ("KEGG" %in% colnames(table)) {
    table %<>%
      mutate(
        KEGG = paste0(
          '<a target="_blank" href="',
          "http://www.genome.jp/dbget-bin/www_bget?cpd:",
          KEGG, '">', KEGG, "</a>"
        )
      )
  }

  if ("Enzyme" %in% colnames(table) && dbChosen == "KEGG") {
    table %<>%
      mutate(
        Enzyme = paste0(
          '<a target="_blank" href="',
          "http://www.genome.jp/dbget-bin/www_bget?ec:",
          Enzyme, '">', Enzyme, "</a>"
        )
      )
  }

  if ("HMDB" %in% colnames(table)) {
    table %<>%
      mutate(
        HMDB = paste0(
          '<a target="_blank" href="',
          "http://www.hmdb.ca/metabolites/",
          HMDB, '">', HMDB, "</a>"
        )
      )
  }

  if ("Compound" %in% colnames(table) && dbChosen == "MetaCyc") {
    table %<>%
      mutate(
        Compound = paste0(
          '<a target="_blank" href="',
          "https://metacyc.org/compound?orgid=META&id=",
          Compound, '">', Compound, "</a>"
        )
      )
  }

  if ("Reaction" %in% colnames(table) && dbChosen == "MetaCyc") {
    table %<>%
      mutate(
        Reaction = paste0(
          '<a target="_blank" href="',
          "https://metacyc.org/META/NEW-IMAGE?type=REACTION&object=",
          Reaction, '">', Reaction, "</a>"
        )
      )
  }

  if ("MetaCyc Gene" %in% colnames(table) && dbChosen == "MetaCyc") {
    table %<>%
      mutate(
        `MetaCyc Gene` = paste0(
          '<a target="_blank" href="',
          "https://metacyc.org/gene?orgid=META&id=",
          `MetaCyc Gene`, '">', `MetaCyc Gene`, "</a>"
        )
      )
  }

  if ("Ensembl" %in% colnames(table)) {
    table %<>%
      rowwise() %>%
      mutate(
        Ensembl = ifelse(
          is.na(Ensembl),
          NA,
          paste0(
            '<a target="_blank" href="',
            "http://www.ensembl.org/id/",
            Ensembl, '">', Ensembl, "</a>"
          )
        )
      ) %>%
      ungroup()
  }

  if ("Entrez" %in% colnames(table)) {
    table %<>%
      rowwise() %>%
      mutate(
        Entrez = ifelse(
          is.na(Entrez),
          NA,
          paste0(
            '<a target="_blank" href="',
            "https://www.ncbi.nlm.nih.gov/gene/",
            Entrez, '">', Entrez, "</a>"
          )
        )
      ) %>%
      ungroup()
  }


  return(table)
}
