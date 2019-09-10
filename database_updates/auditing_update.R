# Testing updated database file


# Load libraries and source functions -------------------------------------

library(tidyverse)
source("functions/utilityFunctions.R")


# Load all the new database files -----------------------------------------

one <- read_tsv("database_updates/1-compounds-ids.tsv")

two <- read_tsv("database_updates/2-compounds-reactions.tsv") %>%
  select(-X4) %>%
  rename("reaction" = ID, "compoundName" = Name, "compoundID" = Matches)

three <- read_tsv("database_updates/3-reactions-genes.tsv") %>%
  select(-X4) %>%
  rename("geneID" = "ID", "geneName" = "Name", "reaction" = "Matches")

four <- read_tsv("database_updates/4-genes-ids.tsv")

five <- read_tsv("database_updates/5-pathways-reactions.tsv")


# Testing joins -----------------------------------------------------------

innerJoin <- inner_join(two, three)
fullJoin <- full_join(two, three)


# Test HMDB ID cleaning function ------------------------------------------

lactate <- read_csv("example_data/lactate.csv")

one %>%
  filter(HMDB == matchHMDB(lactate$hmdbID[1]))
