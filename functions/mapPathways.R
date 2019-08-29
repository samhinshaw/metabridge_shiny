#' mapKEGGPathways
#'
#' @param idType ID type/format being used in the mapping
#' @param selectedRow Row which is selected/highlighted by the user
#' @param summaryTable Summary table prodiced by earlier mapping
#' @param fullTable Full table...?
#'
#' @return
#' @export
#'
#' @examples
#'
#' Map to pathways when the selected database is KEGG for later visualization
#'
mapKEGGPathways <- function(idType,
                            selectedRow,
                            summaryTable,
                            fullTable) {

  # Get Name out of Summary (clicked) Table

  # To be treated like a variable
  namedIDType <- as.name(idType)
  KEGGname <- as.name("KEGG")

  # Pull the selected row and extract its compound ID
  selectedMetab <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2(idType) %>%
    str_extract("C[0-9]{5}")

  # To be treated like a character string
  quotedMetab <- enquo(selectedMetab)

  # Pull the selected row and extract its compound Name
  selectedMetabName <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2("Compound")

  # Pull out the pathways that our compound is present in from the metabPathways
  # object stored in `data/`

  pathwaysOfInterest <- keggPathways %>%
    dplyr::filter(!!(namedIDType) == !!(quotedMetab)) %>%
    filter(id %in% keggHumanPathways)

  # Find all the genes that compound interacts with (from our initial mapping
  # table)
  genesOfInterest <- fullTable %>%
    dplyr::filter(!!(KEGGname) == !!(quotedMetab)) %>%
    magrittr::extract2("Gene Name")

  return(list(
    "selectedCompound" = selectedMetab,
    "selectedCompoundName" = selectedMetabName,
    "genesOfSelectedCompound" = genesOfInterest,
    "pathwaysOfSelectedCompound" = pathwaysOfInterest
  ))
}




#' MapMetaCycPathways
#'
#' @param idType ID type being used in the mapping
#' @param selectedRow Row selected or highlighted by the user
#' @param summaryTable Summary table produced in earlier mapping step
#' @param fullTable
#'
#' @return
#' @export
#'
#' @examples
#'
#' Maps pathways if MetaCyc was the selected database for previous mapping step
#'
mapMetaCycPathways <- function(idType,
                               selectedRow,
                               summaryTable,
                               fullTable) {

  # Get Name out of Summary (clicked) Table

  # To be treated like a variable
  namedIDType <- as.name(idType)

  # Pull the selected row and extract its compound ID
  selectedMetab <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2(idType)

  # To be treated like a character string
  quotedMetab <- enquo(selectedMetab)

  # Pull the selected row and extract its compound Name
  selectedMetabName <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2("Compound")

  # Get Info from Full Mapping Table
  genesOfInterest <- fullTable %>%
    dplyr::filter(!!(namedIDType) == !!(quotedMetab)) %>%
    magrittr::extract2("Official Gene Symbol")

  selectedReaction <- fullTable %>%
    dplyr::filter(!!(namedIDType) == !!(quotedMetab)) %>%
    extract2("Reaction")

  quotedSelectedReaction <- enquo(selectedReaction)

  pathwaysOfInterest <- metaCycPathways %>%
    dplyr::filter(reaction %in% !!(selectedReaction))

  return(list(
    "selectedCompound" = selectedMetab,
    "selectedCompoundName" = selectedMetabName,
    "genesOfSelectedCompound" = genesOfInterest,
    "pathwaysOfSelectedCompound" = pathwaysOfInterest
  ))
}




#' generalPathwayMapping
#'
#' @param db Selected databse, one of KEGG or MetaCyc
#' @param idType Selected ID type used for mapping
#' @param selectedRow Highlighted row from the user with the metabolite to get
#'                    mapping info for
#' @param summaryTable Summary table; the output of the earlier mnapping step
#' @param fullTable
#'
#' @return
#' @export
#'
#' @examples
#'
#' Calls one of the above function to map pathways depending on chosen database
#'
generalPathwayMapping <- function(db,
                                  idType,
                                  selectedRow,
                                  summaryTable,
                                  fullTable) {

  # If KEGG was chosen, just use the KEGG Compound IDs
  if (db == "KEGG") {
    mapKEGGPathways(
      idType = "KEGG",
      selectedRow = selectedRow,
      summaryTable = summaryTable,
      fullTable = fullTable
    )

  # If MetaCyc was chosen, use the selected ID Type
  } else if (db == "MetaCyc") {
    mapMetaCycPathways(
      idType = idType,
      selectedRow = selectedRow,
      summaryTable = summaryTable,
      fullTable = fullTable
    )
  }
}
