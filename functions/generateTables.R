#' generateSummaryTable
#'
#' @param mappingObject Metabolite-containing object produced by mapping step
#' @param idType Selected ID type
#' @param dbChosen Database chosen for the mapping
#'
#' @return
#' @export
#'
#' @examples
#'
#' Generates a nice DT table summarizing the mapping results
#'
generateSummaryTable <- function(mappingObject,
                                 idType,
                                 dbChosen) {

  # Should never be null since we're not responding until map button is clicked,
  # but good to have just in case
  if (is.null(mappingObject$data)) {
    return(NULL)


  # Statement of there was an error or silent failure
  } else if (mappingObject$status == "error" | mappingObject$status == "empty") {
    return(mappingObject$data)


  # Summary if MetaCyc was the selected database
  } else if (dbChosen == "MetaCyc") {

    table <- mappingObject$data %>%
      group_by(!!sym(idType), Compound) %>%
      summarize(
        "# Reactions" = n_distinct(`Reaction`, na.rm = TRUE),
        "# Genes (MetaCyc)" = n_distinct(`MetaCyc Gene`, na.rm = TRUE),
        "# Gene Names" = n_distinct(`Gene Name`, na.rm = TRUE),
        "# Genes (Ensembl)" = n_distinct(`Ensembl`, na.rm = TRUE)
      ) %>% ungroup()

    # Return the results for the user, and provide database info
    return(list("table" = table, "dbChosen" = "MetaCyc"))


  # Summary table if KEGG was the chosen database
  } else if (dbChosen == "KEGG") {

    table <- mappingObject$data %>%
      group_by(KEGG, !!sym(idType), Compound) %>%
      summarize(
        "# Enzymes" = n_distinct(`Enzyme`, na.rm = TRUE),
        "# Gene Names" = n_distinct(`Gene Name`, na.rm = TRUE),
        "# Genes (Entrez)" = n_distinct(`Entrez`, na.rm = TRUE)
      ) %>% ungroup()

    # Return the results for the user, and provide database info
    return(list("table" = table, "dbChosen" = "KEGG"))
  }

}




#' generateMetaCycMetabTable
#'
#' @param mappingObject Object produced by mapping function mapGenerally()
#' @param summaryTable Summary table produced by above function
#'   generateSummaryTable()
#' @param selectedRows Row selected by the user containing a metabolite that we
#'   will grab info for
#' @param idType ID type of selected row (done by the user)
#'
#' @return
#' @export
#'
#' @examples
#'
#' Generate MetaCyc table for the user-selected row
#'
generateMetaCycMetabTable <- function(mappingObject,
                                      summaryTable,
                                      selectedRows,
                                      idType) {

  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data) | is.null(selectedRows) | is.null(summaryTable)) {

    # Returns an empty data frame
    return(data.frame())

  # If the mapping object is not NULL, continue as planned
  } else {

    # Quote necessary variables for dplyr
    namedIDType <- as.name(idType)
    quotedIDType <- quo(idType)
    pastedIDType <- paste0(idType)

    # Pull the selected row and extract its compound ID
    selectedMetab <-
      summaryTable[as.numeric(rownames(summaryTable)) == selectedRows, ]

    # Extract the paritcular ID from the selected row
    selectedMetab <- selectedMetab %>%
      extract2(pastedIDType)

    # Quote for NSE
    quotedSelectedMetab <- enquo(selectedMetab)

    # Filter the mapping object for the selected metabolite
    filteredMappedMetaboliteTable <- mappingObject$data %>%
      filter(!!(namedIDType) == !!(quotedSelectedMetab))

    # Return the filtered table to the user (i.e. the details behind the summary
    # for that paritular metabolite)
    return(filteredMappedMetaboliteTable)
  }
}




#' generateKEGGMetabTable
#'
#' @param mappingObject Full results from the metabolite mapping
#' @param summaryTable Summary table as made by generateSummarytable()
#' @param selectedRows Row selected by the user with desired metabolite
#' @param idType ID type for the selected metabolite
#'
#' @return
#' @export
#'
#' @examples
#'
#' Generate the more detailed table for a particular metabolite if the chosen
#' database was KEGG
#'
generateKEGGMetabTable <- function(mappingObject,
                                   summaryTable,
                                   selectedRows,
                                   idType) {

  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data) | is.null(selectedRows) | is.null(summaryTable)) {
    return(data.frame())

  # Continue if there was no failure
  } else {

    # Quote necessary variables for dplyr
    namedIDType <- as.name(idType)
    quotedIDType <- quo(idType)
    pastedIDType <- paste0(idType)

    # Pull the selected row and extract its compound ID
    selectedMetab <-
      summaryTable[as.numeric(rownames(summaryTable)) == selectedRows, ]

    # If mapping against the KEGG database, pull out the KEGG CPD ID (even if
    # not what was supplied), and extract the ID from the HTML contents of the
    # cell
    selectedMetab <- selectedMetab %>%
      extract2("KEGG") %>%
      # Make sure we've just got the kegg compound ID
      str_extract("C[0-9]{5}")

    # Quote for NSE
    quotedSelectedMetab <- enquo(selectedMetab)

    # Establish column name for filtering step
    namedIDType <- as.name("KEGG")

    # Filter the full mapping table based on chosen compund
    filteredMappedMetaboliteTable <- mappingObject$data %>%
      filter(!!(namedIDType) == !!(quotedSelectedMetab))


    return(filteredMappedMetaboliteTable)
  }
}




#' hyperlinkTable
#'
#' @param table Table of mapped metaboltes
#' @param dbChosen Chosen database
#'
#' @return
#' @export
#'
#' @examples
#'
#' Generates a hyperlink column based on the IDs present
#'
hyperlinkTable <- function(table, dbChosen) {

  # keggCompoundURL <- "http://www.genome.jp/dbget-bin/www_bget?cpd:"
  # keggEnzymeURL <- "http://www.genome.jp/dbget-bin/www_bget?ec:"

  # hmdbURL <- "http://www.hmdb.ca/metabolites/"

  # metaCycCompoundURL <- "https://metacyc.org/compound?orgid=META&id="
  # metaCycReactionURL <- "https://metacyc.org/META/NEW-IMAGE?type=REACTION&object="
  # metaCycGeneURL <- "https://metacyc.org/gene?orgid=META&id="


  # If KEGG IDs, either database
  if ("KEGG" %in% colnames(table)) {
    table <- table %>%
      mutate(KEGG = paste0(
        '<a target="_blank" href="',
        "http://www.genome.jp/dbget-bin/www_bget?cpd:",
        KEGG, '">', KEGG, "</a>"
      ))
  }

  # Enzyme names and KEGG database
  if ("Enzyme" %in% colnames(table) && dbChosen == "KEGG") {
    table <- table %>%
      mutate(Enzyme = paste0(
        '<a target="_blank" href="',
        "http://www.genome.jp/dbget-bin/www_bget?ec:",
        Enzyme, '">', Enzyme, "</a>"
      ))
  }

  # HMDB IDs, either database
  if ("HMDB" %in% colnames(table)) {
    table <- table %>%
      mutate(HMDB = paste0(
        '<a target="_blank" href="',
        "http://www.hmdb.ca/metabolites/",
        HMDB, '">', HMDB, "</a>"
      ))
  }

  # Compund names and MetaCyc database
  if ("Compound" %in% colnames(table) && dbChosen == "MetaCyc") {
    table <- table %>%
      mutate(Compound = paste0(
        '<a target="_blank" href="',
        "https://metacyc.org/compound?orgid=META&id=",
        Compound, '">', Compound, "</a>"
      ))
  }

  # Reaction name and MetaCyc database
  if ("Reaction" %in% colnames(table) && dbChosen == "MetaCyc") {
    table <- table %>%
      mutate(Reaction = paste0(
        '<a target="_blank" href="',
        "https://metacyc.org/META/NEW-IMAGE?type=REACTION&object=",
        Reaction, '">', Reaction, "</a>"
      ))
  }

  # MetaCyc Gene and MetaCyc database
  if ("MetaCyc Gene" %in% colnames(table) && dbChosen == "MetaCyc") {
    table <- table %>%
      mutate(`MetaCyc Gene` = paste0(
        '<a target="_blank" href="',
        "https://metacyc.org/gene?orgid=META&id=",
        `MetaCyc Gene`, '">', `MetaCyc Gene`, "</a>"
      ))
  }

  # Ensembl gene, either database
  if ("Ensembl" %in% colnames(table)) {
    table <- table %>%
      rowwise() %>%
      mutate(Ensembl = ifelse(is.na(Ensembl), NA, paste0(
        '<a target="_blank" href="',
        "http://www.ensembl.org/id/",
        Ensembl, '">', Ensembl, "</a>"
      ))) %>%
      ungroup()
  }

  # Entrez gene and eiter database
  if ("Entrez" %in% colnames(table)) {
    table <- table %>%
      rowwise() %>%
      mutate(Entrez = ifelse(is.na(Entrez), NA, paste0(
        '<a target="_blank" href="',
        "https://www.ncbi.nlm.nih.gov/gene/",
        Entrez, '">', Entrez, "</a>"
      ))) %>%
      ungroup()
  }


  return(table)
}
