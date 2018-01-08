## Some database testing
library(tidyverse)
source('functions/utilityFunctions.R')

one <- read_tsv('database_updates/1_compounds_and_IDs.tsv')

two <- read_tsv('database_updates/2_reactions_and_compounds.tsv')
two %<>% select(-X4)
two %<>% rename('reaction' = 'ID', 'compoundName' = 'Name', 'compoundID' = 'Matches')

three <- read_tsv('database_updates/3_reactions_and_genes.tsv')
three %<>% select(-X4)
three %<>% rename('geneID' = 'ID', 'geneName' = 'Name', 'reaction' = 'Matches')

four <- read_tsv('database_updates/4_genes_and_IDs.tsv')
five <- read_tsv('database_updates/5_pathways_and_reactions.tsv')

innerJoin <- inner_join(two, three)
fullJoin <- full_join(two, three)

lactate <- read_csv('example_data/lactate.csv')

one %>% 
  filter(HMDB == matchHMDB(lactate$hmdbID[1]))
