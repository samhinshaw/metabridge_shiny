#' visualizePathview
#'
#' @param pathway
#' @param genes
#' @param cpd
#'
#' @return filename
#' @export
#'
#' @examples
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
