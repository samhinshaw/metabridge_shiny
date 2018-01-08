visualizePathview <- function(pathway, genes, cpd) {

  # Generate the PNG
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
