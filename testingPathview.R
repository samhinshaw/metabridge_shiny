library(tidyverse)
library(pathview)
# TEST
theNameOfKEGG <- quo(KEGG)

testResults <- mapMetaCyc(importDF = name_map, col = 'KEGG', idType = 'KEGG')$data
miniTestResults <- testResults %>% 
  filter(UQ(theNameOfKEGG) == "C00300")

pathview(
  gene.data = miniTestResults$`Official Gene Symbol`, 
  cpd.data = miniTestResults$KEGG,
  pathway.id = "00260",
  gene.idtype = "SYMBOL",
  species = "hsa"
)
pathview(
  gene.data = miniTestResults$`Official Gene Symbol`, 
  cpd.data = miniTestResults$KEGG,
  pathway.id = "00330",
  gene.idtype = "SYMBOL",
  species = "hsa"
)
pathview(
  gene.data = miniTestResults$`Official Gene Symbol`, 
  cpd.data = miniTestResults$KEGG,
  pathway.id = "01100",
  gene.idtype = "SYMBOL",
  species = "hsa"
)
