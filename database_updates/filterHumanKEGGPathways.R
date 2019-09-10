
# Generate list of pathways based on all the supported pathways in Pathview

keggHumanPathways <- list.files(path = "pathways") %>%
  grep(pattern = ".png$", value = TRUE) %>%
  str_replace(".png$", "") %>%
  str_replace("^hsa", "")

save(keggHumanPathways, file = "data/k05_keggHumanPathways.RData")
