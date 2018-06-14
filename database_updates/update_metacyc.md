Generating MetaCyc SmartTables
================

This R Markdown document provides all of the steps necessary to update
MetaBridge with the most recent build of MetaCyc. As of this writing
(June 14th, 2018), we are on MetaCyc 22.0. I highly recommend working
through this document step by step and inspecting the output, rather
than running the whole thing in one shot.

## Online SmartTables

### Create Table 1: Compounds and IDs

1.  Go to www.metacyc.org and log in.
2.  Navigate to \[SmartTables\] -\> \[Special SmartTables\].
3.  Choose \[All compounds of MetaCyc\].
4.  If you see a warning message, make sure you create a writeable copy
    of the SmartTable.
5.  Delete all columns except for the \[Compound\] column by clicking on
    the column header (in navy blue), and then in the control panel on
    the right hand side, clicking \[Column \>\] -\> \[\> Delete Column\]
6.  Then click the \[Compound\] column header, and you should then see
    three dropdown menus above the table: \[Add Transform Column\],
    \[Add Property Column\], and \[Enrichments\].
7.  \[Add Property Column\] -\> \[Database Links\]. Choose:
      - \[CAS\]
      - \[HMDB\]
      - \[KEGG LIGAND\]
      - \[PubChem-compound\]
8.  On the right-hand pane, under \[Operations\], choose \[Export \>\]
    -\> \[\> to Spreadsheet File\], choose \[frame IDs\] as the “format
    type for values in spreadsheet”, and export the SmartTable.

<!-- end list -->

    /database_updates/1-compounds-ids.tsv

### Create Table 4: Genes and IDs

1.  Go to www.metacyc.org and log in.
2.  Navigate to \[SmartTables\] -\> \[Special SmartTables\].
3.  Choose \[All genes of MetaCyc\].
4.  If you see a warning message, make sure you create a writeable copy
    of the SmartTable.
5.  Delete all columns except for the \[Gene Name\] column by clicking
    on the column header (in navy blue), and then in the control panel
    on the right hand side, clicking \[Column \>\] -\> \[\> Delete
    Column\]
6.  Then click the \[Gene Name\] column header, and you should then see
    three dropdown menus above the table: \[Add Transform Column\],
    \[Add Property Column\], and \[Enrichments\].
7.  \[Add Property Column\] -\> \[Database Links\]. Choose:
      - \[Ensembl Human\]
      - \[GeneCards\] - Official Gene Symbol
8.  On the right-hand pane, under \[Operations\], choose \[Export \>\]
    -\> \[\> to Spreadsheet File\], choose \[frame IDs\] as the “format
    type for values in spreadsheet”, and export the SmartTable.

<!-- end list -->

    /database_updates/4-genes-ids.tsv

## SmartTables in pathway-tools

pathway-tools offers more consistent results, better user interface, and
is generally less buggy than the online SmartTables. However, they
should theoretically provide the same results, and you can test this. To
download and install pathway-tools, you will need to contact MetaCyc
support for an educational license. They will verify your request and
then email you a link with a username and password to log in and
download the software.

1.  Download and install pathway-tools (currently version 21.0)
2.  Run pathway-tools, and in the visual interface click ‘MetaCyc’

### Create Table 2: Reactions of Compounds

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -\> \[Containing All\] -\> \[Compounds\]
2.  Once list of all compounds appears (currently 15950 Total), choose
    \[SmartTables\] -\> \[Transform SmartTable\] -\> \[Reactions of
    Compounds\]
3.  Save the SmartTable: \[SmartTables\] -\> \[Export SmartTable\] -\>
    \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

<!-- end list -->

    /database_updates/2-compounds-reactions.tsv

### Create Table 3: Genes of Reactions

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -\> \[Containing All\] -\> \[Reactions\]
2.  Once the list fo all reactions appears (currently 15311 Reactions),
    choose \[SmartTables\] -\> \[Transform SmartTable\] -\> \[Genes of a
    Reaction\]
3.  Save the SmartTable: \[SmartTables\] -\> \[Export SmartTable\] -\>
    \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

<!-- end list -->

    /database_updates/3-reactions-genes.tsv

### Create Table 5: Pathways and Reactions

1.  From the \[SmartTables\] menu dropdown choose \[Create New
    SmartTable\] -\> \[Containing All\] -\> \[Pathways\]
2.  Once list of all compounds appears (currently 2903 Total), choose
    \[SmartTables\] -\> \[Transform SmartTable\] -\> \[Reactions of
    pathway\]
3.  Save the SmartTable: \[SmartTables\] -\> \[Export SmartTable\] -\>
    \[Tab-delimited table\], and select save directory & filename.
4.  When prompted, specify \[Identifiers\] as the export choice.

<!-- end list -->

    /database_updates/5-pathways-reactions.tsv

## Munge Tables

### Step 1: Cross-referencing Databases

Unfortunately, all of these tables require munging\! The have been
formatted for display with HTML entities and anchor tags.

> **IMPORTANT NOTE BEFORE WE GET STARTED:** **This script will OVERWRITE
> your current database files located in /data/. Make sure you check
> that everything is working properly before you commit these changes to
> version control and deploy the update.**

``` r
library(tidyverse)
library(stringr)
library(magrittr)
library(rprojroot)
library(splitstackshape)

rootDir <- rprojroot::find_rstudio_root_file()
updateDir <- file.path(rootDir, 'database_updates')
exampleDir <- file.path(rootDir, 'example_data')
dataDir <- file.path(rootDir, 'data')
cleanDir <- file.path(updateDir, 'cleaned_files')

compoundIDs <- read_tsv(file.path(updateDir, '1-compounds-ids.tsv'))

compoundIDs %<>% 
  # Create new columns that represent 'web' columns
  # mutate(webHMDB = HMDB) %>% mutate(webCAS = CAS) %>% 
  # mutate(webKEGG = KEGG) %>% mutate(webPubChem = PubChem) %>% 
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

Looks good for the first dataset. Let’s give it a test\!

``` r
# lactate <- read_csv(file.path(exampleDir, 'lactate.csv'))
exampleCompound <- read_csv(file.path(exampleDir, 'urea.csv'))

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

    ## # A tibble: 1 x 5
    ##   Compound  CAS     HMDB      KEGG   PubChem
    ##   <chr>     <chr>   <chr>     <chr>  <chr>  
    ## 1 L-LACTATE 79-33-4 HMDB00190 C00186 5460161

``` r
compoundIDs %>% 
  dplyr::filter(HMDB == exampleCompound %>% extract2('HMDB') %>% matchHMDB())
```

    ## # A tibble: 1 x 5
    ##   Compound CAS     HMDB      KEGG   PubChem
    ##   <chr>    <chr>   <chr>     <chr>  <chr>  
    ## 1 UREA     57-13-6 HMDB00294 C00086 1176

``` r
# Good, let's save that
linkedExampleCompound <- compoundIDs %>% 
  dplyr::filter(HMDB == exampleCompound %>% extract2('HMDB') %>% matchHMDB())
```

Looks like we’re good to go, let’s save it\!

``` r
metaCycDBLinks <- compoundIDs
save(metaCycDBLinks, file = file.path(dataDir, 'm01_metaCycDBLinks.RData'))
```

### Step 2: Map to Reactions

Now we can check our second
table.

``` r
compoundsReactions <- read_tsv(file.path(updateDir, '2-compounds-reactions.tsv'))
glimpse(compoundsReactions)
```

    ## Observations: 16,256
    ## Variables: 4
    ## $ ID      <chr> "RXN1G-137", "TREHALA-RXN", "RXN-4441", "ALPHAALPHA-TR...
    ## $ Name    <chr> "&alpha;, &alpha;'-trehalose 6-&alpha;-mycolate<sub>[c...
    ## $ Matches <chr> "CPD1G-1344, ADP, ATP, WATER, PROTON, Pi", "TREHALOSE,...
    ## $ X4      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...

We can see there are some HTML markup and entities in there, but they
are restricted to the “Name” column, which we are not mapping against.
However, we should rename the columns and drop the extraneous 4th
column.

``` r
compoundsReactions %<>% dplyr::select(-X4)
compoundsReactions %<>% dplyr::rename('reaction' = 'ID', 'reactionName' = 'Name', 'compound' = 'Matches')
glimpse(compoundsReactions)
```

    ## Observations: 16,256
    ## Variables: 3
    ## $ reaction     <chr> "RXN1G-137", "TREHALA-RXN", "RXN-4441", "ALPHAALP...
    ## $ reactionName <chr> "&alpha;, &alpha;'-trehalose 6-&alpha;-mycolate<s...
    ## $ compound     <chr> "CPD1G-1344, ADP, ATP, WATER, PROTON, Pi", "TREHA...

Almost, but we need to meld the data.frame to have one compound per row.
I really like the ‘splitstackshape’ library for this.

``` r
compoundsReactions %<>% cSplit(splitCols = c('compound'), 
                              sep = ', ', direction = 'long', 
                              type.convert = FALSE, stripWhite = FALSE)
glimpse(compoundsReactions)
```

    ## Observations: 70,563
    ## Variables: 3
    ## $ reaction     <chr> "RXN1G-137", "RXN1G-137", "RXN1G-137", "RXN1G-137...
    ## $ reactionName <chr> "&alpha;, &alpha;'-trehalose 6-&alpha;-mycolate<s...
    ## $ compound     <chr> "CPD1G-1344", "ADP", "ATP", "WATER", "PROTON", "P...

Save our results

``` r
metaCycCompoundsReactions <- compoundsReactions
save(metaCycCompoundsReactions, file = file.path(dataDir, 'm02_metaCycCompoundsReactions.RData'))
```

Now let’s give it a test

``` r
exampleReactions <- compoundsReactions %>% 
  dplyr::filter(compound %in% extract2(linkedExampleCompound, 'Compound'))
glimpse(exampleReactions)
```

    ## Observations: 21
    ## Variables: 3
    ## $ reaction     <chr> "UREIDOGLYCOLATE-LYASE-RXN", "DIGUANIDINOBUTANASE...
    ## $ reactionName <chr> "(<i>S</i>)-ureidoglycolate  &rarr;  urea + glyox...
    ## $ compound     <chr> "UREA", "UREA", "UREA", "UREA", "UREA", "UREA", "...

Good to
go\!

## Step 3: Map reactions to genes

``` r
reactionsGenes <- read_tsv(file.path(updateDir, '3-reactions-genes.tsv'))
reactionsGenes %<>% dplyr::select(-X4)
reactionsGenes %<>% dplyr::rename('geneID' = 'ID', 'geneName' = 'Name', 'reaction' = 'Matches')
head(reactionsGenes)
```

    ## # A tibble: 6 x 3
    ##   geneID    geneName               reaction                               
    ##   <chr>     <chr>                  <chr>                                  
    ## 1 G-10132   &alpha;-SCS2           SUCCCOASYN-RXN                         
    ## 2 G-10320   &beta;-FS              RXN-8422                               
    ## 3 AT4G25700 &beta;-OHase 1         RXN-8025, RXN-8026, RXN-8039           
    ## 4 AT5G52570 &beta;-OHase 2         RXN-8025, RXN-8026                     
    ## 5 G-18015   &beta;-pgm             BETA-PHOSPHOGLUCOMUTASE-RXN, RXN-16995…
    ## 6 G-15387   &beta;-primeverosidase RXN-13694, RXN-13696, RXN-13693, RXN-1…

Again, we see there are some HTML markup and entities in there, but they
are restricted to the “geneName” column, which we are not mapping
against. We also need to split our reactions column.

``` r
reactionsGenes %<>% cSplit(
  splitCols = c('reaction'), 
  sep = ', ', direction = 'long', 
  type.convert = FALSE, stripWhite = FALSE
)
glimpse(reactionsGenes)
```

    ## Observations: 22,603
    ## Variables: 3
    ## $ geneID   <chr> "G-10132", "G-10320", "AT4G25700", "AT4G25700", "AT4G...
    ## $ geneName <chr> "&alpha;-SCS2", "&beta;-FS", "&beta;-OHase 1", "&beta...
    ## $ reaction <chr> "SUCCCOASYN-RXN", "RXN-8422", "RXN-8025", "RXN-8026",...

``` r
metaCycReactionsGenes <- reactionsGenes
save(metaCycReactionsGenes, file = file.path(dataDir, 'm03_metaCycReactionsGenes.RData'))
```

Now we can test out step 3. Let’s do a join here rather than a filter so
we retain our relational information. That is to say, how we go to this
step.

``` r
exampleGenes <- left_join(exampleReactions, reactionsGenes, by = 'reaction')
glimpse(exampleGenes)
```

    ## Observations: 44
    ## Variables: 5
    ## $ reaction     <chr> "UREIDOGLYCOLATE-LYASE-RXN", "UREIDOGLYCOLATE-LYA...
    ## $ reactionName <chr> "(<i>S</i>)-ureidoglycolate  &rarr;  urea + glyox...
    ## $ compound     <chr> "UREA", "UREA", "UREA", "UREA", "UREA", "UREA", "...
    ## $ geneID       <chr> "G6275", "YIR032C", NA, NA, "G-9016", "G-9012", "...
    ## $ geneName     <chr> "allA", "DAL3", NA, NA, "gbh", "gbuA", "gbuA", "K...

### Step 4: Map to gene IDs

``` r
geneIDs <- read_tsv(file.path(updateDir, '4-genes-ids.tsv'))
glimpse(geneIDs)
```

    ## Observations: 12,142
    ## Variables: 3
    ## $ `Gene Name` <chr> "G-6024", "G185E-5667", "G-12048", "G7490", "AT4G2...
    ## $ Ensembl     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ GeneCards   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...

``` r
geneIDs %<>% 
  mutate(Ensembl = str_replace(Ensembl, "^<a href='.*'>", "")) %>% 
  mutate(Ensembl = str_replace(Ensembl, "<\\/a>$", "")) %>% 
  mutate(GeneCards = str_replace(GeneCards, "^<a href='.*'>", "")) %>% 
  mutate(GeneCards = str_replace(GeneCards, "<\\/a>$", "")) %>% 
  rename('geneID' = 'Gene Name', 'Symbol' = 'GeneCards')
glimpse(geneIDs)
```

    ## Observations: 12,142
    ## Variables: 3
    ## $ geneID  <chr> "G-6024", "G185E-5667", "G-12048", "G7490", "AT4G20150...
    ## $ Ensembl <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ Symbol  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...

``` r
metaCycGeneIDs <- geneIDs

save(metaCycGeneIDs, file = file.path(dataDir, 'm04_metaCycGeneIDs.RData'))
```

And join to the table

``` r
examplegeneIDs <- left_join(exampleGenes, geneIDs, by = 'geneID')
glimpse(examplegeneIDs)
```

    ## Observations: 44
    ## Variables: 7
    ## $ reaction     <chr> "UREIDOGLYCOLATE-LYASE-RXN", "UREIDOGLYCOLATE-LYA...
    ## $ reactionName <chr> "(<i>S</i>)-ureidoglycolate  &rarr;  urea + glyox...
    ## $ compound     <chr> "UREA", "UREA", "UREA", "UREA", "UREA", "UREA", "...
    ## $ geneID       <chr> "G6275", "YIR032C", NA, NA, "G-9016", "G-9012", "...
    ## $ geneName     <chr> "allA", "DAL3", NA, NA, "gbh", "gbuA", "gbuA", "K...
    ## $ Ensembl      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "...
    ## $ Symbol       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "...

### Step 5: Map to pathways

``` r
pathwaysReactions <- read_tsv(file.path(updateDir, '5-pathways-reactions.tsv'))
```

    ## Warning: Missing column names filled in: 'X4' [4]

    ## Parsed with column specification:
    ## cols(
    ##   ID = col_character(),
    ##   Name = col_character(),
    ##   Matches = col_character(),
    ##   X4 = col_character()
    ## )

First things first, let’s take a look at what we have:

``` r
glimpse(pathwaysReactions)
```

    ## Observations: 9,887
    ## Variables: 4
    ## $ ID      <chr> "RXN1G-137", "TREHALA-RXN", "RXN-4441", "ALPHAALPHA-TR...
    ## $ Name    <chr> "&alpha;, &alpha;'-trehalose 6-&alpha;-mycolate<sub>[c...
    ## $ Matches <chr> "PWYG-321, PWY-6113", "PWY-6981, PWY0-1182, PWY0-1466"...
    ## $ X4      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...

``` r
pathwaysReactions %<>% dplyr::select(-X4)
pathwaysReactions %<>% dplyr::rename('pathwayID' = 'ID', 'pathwayName' = 'Name', 'reaction' = 'Matches')
```

Again, we see there are some HTML markup and entities in the table, but
they are restricted to the “Name” column, which we are not mapping
against. However, we will need to split the reactions column.

``` r
pathwaysReactions %<>% cSplit(
  splitCols = c('reaction'), 
  sep = ', ', direction = 'long', 
  type.convert = FALSE, stripWhite = FALSE
)
glimpse(pathwaysReactions)
```

    ## Observations: 17,719
    ## Variables: 3
    ## $ pathwayID   <chr> "RXN1G-137", "RXN1G-137", "TREHALA-RXN", "TREHALA-...
    ## $ pathwayName <chr> "&alpha;, &alpha;'-trehalose 6-&alpha;-mycolate<su...
    ## $ reaction    <chr> "PWYG-321", "PWY-6113", "PWY-6981", "PWY0-1182", "...

Looks good, let’s save it\!

``` r
metaCycPathways <- pathwaysReactions
save(metaCycPathways, file = file.path(dataDir, 'm05_metaCycPathways.RData'))
```
