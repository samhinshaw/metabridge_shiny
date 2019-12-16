#' visualizePathview
#'
#' @param pathway Patwhay to make the diagram of
#' @param genes Genes to be included in the pathway map
#' @param cpd Data being used to generate the pathway map
#'
#' @return Create PNG file of pathway which will be displayed to the user.
#' @export
#'
#' Given a selected pathway, calls pathview to show the enzyme/metabolite
#' interactions involving the selected genes
#'
visualizePathview <- function(pathway, genes, cpd) {

  # Generate the PNG of desired pathway, and suppress messages from pathview()
  suppressWarnings({
    pathview(
      gene.data = genes,
      cpd.data = cpd,
      pathway.id = pathway,
      gene.idtype = "SYMBOL",
      species = "hsa",
      kegg.dir = "pathways"
    )
  })

  filename <- paste0("hsa", pathway, ".pathview.png")
  return(filename)
}
