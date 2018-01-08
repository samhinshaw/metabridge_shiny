library(tidyverse)
library(pathview)
# TEST
theNameOfKEGG <- quo(KEGG)

testResults <- mapMetaCyc(importDF = name_map, col = "KEGG", idType = "KEGG")$data
miniTestResults <- testResults %>%
  dplyr::filter(UQ(theNameOfKEGG) == "C00300")

pathview(
  gene.data = miniTestResults$`Official Gene Symbol`,
  cpd.data = miniTestResults$KEGG,
  pathway.id = c("00260", "00330"),
  gene.idtype = "SYMBOL",
  species = "hsa",
  kegg.dir = file.path("pathways")
)
