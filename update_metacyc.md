Generating MetaCyc SmartTable
================

Create Table 1: Compounds and IDs
---------------------------------

1.  Go to www.metacyc.org and log in.
2.  Navigate to \[SmartTables\] -&gt; \[Special SmartTables\].
3.  Choose \[All compounds of MetaCyc\].
4.  If you see a warning message, make sure you create a writeable copy
    of the SmartTable. You should then see three options: \[Add
    Transform Column\], \[Add Property Column\], and \[Enrichments\].
5.  \[Add Property Column\] -&gt; \[Database Links\]. Choose:
    -   \[CAS\]
    -   \[HMDB\]
    -   \[KEGG LIGAND\]
    -   \[PubChem-compound\]
6.  On the right-hand pane, under \[Operations\], choose \[Export &gt;\]
    -&gt; \[&gt; to Spreadsheet File\], choose \[frame IDs\] as the
    “format type for values in spreadsheet”, and export the SmartTable.

Start MetaCyc pathway-tools
---------------------------

1.  Download and install pathway-tools (currently version 21.0)
2.  Run pathway-tools, and in the visual interface click ‘MetaCyc’

Create Table 2: Reactions of Compounds
--------------------------------------

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -&gt; \[Containing All\] -&gt; \[Compounds\]
2.  Once list of all compounds appears (currently 15950 Total), choose
    \[SmartTables\] -&gt; \[Transform SmartTable\] -&gt; \[Reactions of
    Compounds\]
3.  Save the SmartTable: \[SmartTables\] -&gt; \[Export SmartTable\]
    -&gt; \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

Create Table 3: Genes of Reactions
----------------------------------

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -&gt; \[Containing All\] -&gt; \[Reactions\]
2.  Once the list fo all reactions appears (currently 15311 Reactions),
    choose \[SmartTables\] -&gt; \[Transform SmartTable\] -&gt; \[Genes
    of a Reaction\]
3.  Save the SmartTable: \[SmartTables\] -&gt; \[Export SmartTable\]
    -&gt; \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

Create Table 4: Compounds and IDs
---------------------------------

1.  Go to www.metacyc.org and log in.
2.  Navigate to \[SmartTables\] -&gt; \[Special SmartTables\].
3.  Choose \[All genes of MetaCyc\].
4.  If you see a warning message, make sure you create a writeable copy
    of the SmartTable. You should then see three options: \[Add
    Transform Column\], \[Add Property Column\], and \[Enrichments\].
5.  \[Add Property Column\] -&gt; \[Database Links\]. Choose:
    -   \[Ensembl Human\]
    -   \[GeneCards\] - Official Gene Symbol
6.  On the right-hand pane, under \[Operations\], choose \[Export &gt;\]
    -&gt; \[&gt; to Spreadsheet File\], choose \[frame IDs\] as the
    “format type for values in spreadsheet”, and export the SmartTable.

Create Table 5: Pathways and Reactions
--------------------------------------

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -&gt; \[Containing All\] -&gt; \[Pathways\]
2.  Once list of all compounds appears (currently 2903 Total), choose
    \[SmartTables\] -&gt; \[Transform SmartTable\] -&gt; \[Reactions of
    pathway\]
3.  Save the SmartTable: \[SmartTables\] -&gt; \[Export SmartTable\]
    -&gt; \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

Munge Tables
------------

Step 1: Cross-referencing Databases
-----------------------------------

Unfortunately, all of these tables require munging! The have been
formatted for display with HTML entities and anchor tags.

``` r
library(tidyverse)
library(stringr)
library(magrittr)
library(rprojroot)

rootDir <- rprojroot::find_rstudio_root_file()
updateDir <- file.path(rootDir, 'database_updates')
exampleDir <- file.path(rootDir, 'example_data')

compoundIDs <- read_tsv(file.path(updateDir, '1_compounds_and_IDs.tsv'))

compoundIDs %<>% 
  # Create new columns that represent 'web' columns
  mutate(webHMDB = HMDB) %>% mutate(webCAS = CAS) %>% 
  mutate(webKEGG = KEGG) %>% mutate(webPubChem = PubChem) %>% 
  # Strip out opening anchor tag
  mutate(HMDB = str_replace(HMDB, "^<a href='.*'>", "")) %>% 
  mutate(CAS = str_replace(CAS, "^<a href='.*'>", "")) %>% 
  mutate(KEGG = str_replace(KEGG, "^<a href='.*'>", "")) %>% 
  mutate(PubChem = str_replace(PubChem, "^<a href='.*'>", "")) %>% 
  # Strip out closing anchor tag
  mutate(HMDB = str_replace(HMDB, "<\\/a>$", "")) %>% 
  mutate(CAS = str_replace(CAS, "<\\/a>$", "")) %>% 
  mutate(KEGG = str_replace(KEGG, "<\\/a>$", "")) %>% 
  mutate(PubChem = str_replace(PubChem, "<\\/a>$", "")) %>% 
  # Make sure there is no trailing whitespace
  mutate(HMDB = str_trim(HMDB)) %>% 
  mutate(CAS = str_trim(CAS)) %>% 
  mutate(KEGG = str_trim(KEGG)) %>% 
  mutate(PubChem = str_trim(PubChem))
```

Looks good for the first dataset. Let’s give it a test!

``` r
lactate <- read_csv(file.path(exampleDir, 'lactate.csv'))

# MetaCyc only supports the older format, 5 digit HMDB IDs. If we detect your
# HMDB IDs are in the newer 7 digit formar, we will trim the leading characters
# if they are zeros. If they are not zeros, we will return an error.
matchHMDB <- function(hmdbID) {
  # Make sure the ID is a character amd starts with 'HMDB' or 'hmdb'
  # Look at the syntax very carefully here, the parens are IMPORTANT
  if (!is.character(hmdbID) | !(str_detect(hmdbID, '^HMDB') | str_detect(hmdbID, '^hmdb'))) {
    return(NA)
  # If the ID is in the new, 7 digit format, check the leading digits
  } else if (nchar(hmdbID) == 11) {
    # If the leading characters are 00, simply trim the string
    if (str_sub(hmdbID, start = 5, end = 6) == '00') {
      newID <- paste0('HMDB', str_sub(hmdbID, start = -5, end = -1))
      return(newID)
      # Otherwise, return an error
    } else {
      return(NA)
    }
    # Otherwise, if the ID is in the older, 5-digit format, simply return the ID as-is.
  } else if (nchar(hmdbID) == 9) {
    # Do this **anyways** because it'll ensure we have capital letters at the start of the ID
    newID <- paste0('HMDB', str_sub(hmdbID, start = -5, end = -1))
    return(newID)
    # If there is an edge case where the ID is not 9 or 11 charactesr in length(), also return NA
  } else {
    return(NA)
  }
}
```

``` r
matchHMDB('HMDB0000190')  # HMDB00190
matchHMDB('HMDB00190')    # HMDB00190
matchHMDB(200)            # NA
matchHMDB('HMDB00000190') # NA
matchHMDB('superlong')    # NA
matchHMDB('hmdb0000190')  # HMDB00190
matchHMDB('hmdb00190')    # HMDB00190
matchHMDB('hmdbWRONG')    # hmdbWRONG # not currently supporting EVERY edge case
```

``` r
compoundIDs %>% 
  dplyr::filter(HMDB == matchHMDB('HMDB0000190'))
```

    ## # A tibble: 1 x 9
    ##    Compound     CAS      HMDB   KEGG PubChem
    ##       <chr>   <chr>     <chr>  <chr>   <chr>
    ## 1 L-LACTATE 79-33-4 HMDB00190 C00186 5460161
    ## # ... with 4 more variables: webHMDB <chr>, webCAS <chr>, webKEGG <chr>,
    ## #   webPubChem <chr>

``` r
compoundIDs %>% 
  dplyr::filter(HMDB == lactate %>% extract2('hmdbID') %>% matchHMDB())
```

    ## # A tibble: 1 x 9
    ##    Compound     CAS      HMDB   KEGG PubChem
    ##       <chr>   <chr>     <chr>  <chr>   <chr>
    ## 1 L-LACTATE 79-33-4 HMDB00190 C00186 5460161
    ## # ... with 4 more variables: webHMDB <chr>, webCAS <chr>, webKEGG <chr>,
    ## #   webPubChem <chr>

``` r
# Good, let's save that
linkedLactate <- compoundIDs %>% 
  dplyr::filter(HMDB == lactate %>% extract2('hmdbID') %>% matchHMDB())
```

### Step 2: Map to Reactions

Now we can check our second table.

``` r
compoundsReactions <- read_tsv(file.path(updateDir, '2_reactions_and_compounds.tsv'))
glimpse(compoundsReactions)
```

    ## Observations: 15,237
    ## Variables: 4
    ## $ ID      <chr> "4.2.2.16-RXN", "3.2.1.154-RXN", "RXN1G-137", "TREHALA...
    ## $ Name    <chr> "[6)-&beta;-D-fructofuranosyl-(2&rarr;]<sub>(n)</sub> ...
    ## $ Matches <chr> "Levan, DFA-IV", "Levan, Fructofuranose, WATER", "CPD1...
    ## $ X4      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...

We can see there are some HTML markup and entities in there, but they
are restricted to the “Name” column, which we are not mapping against.
However, we should rename the columns and drop the extraneous 4th
column.

``` r
compoundsReactions %<>% dplyr::select(-X4)
compoundsReactions %<>% dplyr::rename('reaction' = 'ID', 'reactionName' = 'Name', 'compoundID' = 'Matches')
glimpse(compoundsReactions)
```

    ## Observations: 15,237
    ## Variables: 3
    ## $ reaction     <chr> "4.2.2.16-RXN", "3.2.1.154-RXN", "RXN1G-137", "TR...
    ## $ reactionName <chr> "[6)-&beta;-D-fructofuranosyl-(2&rarr;]<sub>(n)</...
    ## $ compoundID   <chr> "Levan, DFA-IV", "Levan, Fructofuranose, WATER", ...

Almost, but we need to meld the data.frame to have one compound per row.
I really like the ‘splitstackshape’ library for this.

``` r
library(splitstackshape)
compoundsReactions %<>% cSplit(splitCols = c('compoundID'), 
                              sep = ', ', direction = 'long', 
                              type.convert = FALSE)
glimpse(compoundsReactions)
```

    ## Observations: 66,252
    ## Variables: 3
    ## $ reaction     <chr> "4.2.2.16-RXN", "4.2.2.16-RXN", "3.2.1.154-RXN", ...
    ## $ reactionName <chr> "[6)-&beta;-D-fructofuranosyl-(2&rarr;]<sub>(n)</...
    ## $ compoundID   <chr> "Levan", "DFA-IV", "Levan", "Fructofuranose", "WA...

Now let’s give it a test

``` r
lactateReactions <- compoundsReactions %>% 
  dplyr::filter(compoundID %in% extract2(linkedLactate, 'Compound'))
glimpse(lactateReactions)
```

    ## Observations: 18
    ## Variables: 3
    ## $ reaction     <chr> "LACTALDDEHYDROG-RXN", "RXN-12165", "LACTATE-RACE...
    ## $ reactionName <chr> "(<i>S</i>)-lactaldehyde + NAD<sup>+</sup> + H<su...
    ## $ compoundID   <chr> "L-LACTATE", "L-LACTATE", "L-LACTATE", "L-LACTATE...

Good to go!

Step 3: Map reactions to genes
------------------------------

``` r
reactionsGenes <- read_tsv(file.path(updateDir, '3_reactions_and_genes.tsv'))
reactionsGenes %<>% dplyr::select(-X4)
reactionsGenes %<>% dplyr::rename('geneID' = 'ID', 'geneName' = 'Name', 'reaction' = 'Matches')
head(reactionsGenes)
```

    ## # A tibble: 6 x 3
    ##      geneID               geneName
    ##       <chr>                  <chr>
    ## 1   G-10132           &alpha;-SCS2
    ## 2   G-10320              &beta;-FS
    ## 3 AT4G25700         &beta;-OHase 1
    ## 4 AT5G52570         &beta;-OHase 2
    ## 5   G-18015             &beta;-pgm
    ## 6   G-15387 &beta;-primeverosidase
    ## # ... with 1 more variables: reaction <chr>

Again, we see there are some HTML markup and entities in there, but they
are restricted to the “geneName” column, which we are not mapping
against. We also need to split our reactions column.

``` r
reactionsGenes %<>% cSplit(splitCols = c('reaction'), 
                           sep = ', ', direction = 'long', 
                           type.convert = FALSE)
glimpse(reactionsGenes)
```

    ## Observations: 20,961
    ## Variables: 3
    ## $ geneID   <chr> "G-10132", "G-10320", "AT4G25700", "AT4G25700", "AT4G...
    ## $ geneName <chr> "&alpha;-SCS2", "&beta;-FS", "&beta;-OHase 1", "&beta...
    ## $ reaction <chr> "SUCCCOASYN-RXN", "RXN-8422", "RXN-8026", "RXN-8039",...

Now we can test out step 3. Let’s do a join here rather than a filter so
we retain our relational information. That is to say, how we go to this
step.

``` r
lactateGenes <- left_join(lactateReactions, reactionsGenes, by = 'reaction')
glimpse(lactateGenes)
```

    ## Observations: 31
    ## Variables: 5
    ## $ reaction     <chr> "LACTALDDEHYDROG-RXN", "LACTALDDEHYDROG-RXN", "LA...
    ## $ reactionName <chr> "(<i>S</i>)-lactaldehyde + NAD<sup>+</sup> + H<su...
    ## $ compoundID   <chr> "L-LACTATE", "L-LACTATE", "L-LACTATE", "L-LACTATE...
    ## $ geneID       <chr> "EG10035", "G-12540", "G-12538", "G-9416", "G-180...
    ## $ geneName     <chr> "aldA", "ladh", "LADH", "MJ1411", "rhaEW", "rhaEW...

### Step 4: Map to gene IDs

### Step 5: Map to pathways
