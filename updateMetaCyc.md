# Instructions For Generating MetaCyc SmartTable

## Create Table 1: Compounds and IDs
1. Go to www.metacyc.org and log in.
2. Navigate to [SmartTables] -> [Special SmartTables].
3. Choose [All compounds of MetaCyc].
4. If you see a warning message, make sure you create a writeable copy of the SmartTable. You should then see three options: [Add Transform Column], [Add Property Column], and [Enrichments]. 
5. [Add Property Column] -> [Database Links]. Choose: 
    + [CAS]
    + [HMDB]
    + [KEGG LIGAND]
    + [PubChem-compound]
6. On the right-hand pane, under [Operations], choose [Export >] -> [> to Spreadsheet File], choose [frame IDs] as the "format type for values in spreadsheet", and export the SmartTable. 

## Start MetaCyc pathway-tools
1. Download and install pathway-tools (currently version 21.0)
2. Run pathway-tools, and in the visual interface click 'MetaCyc'

## Create Table 2: Reactions of Compounds
1. From the [SmartTables] menu dropdown choose [Create New SmartTable] -> [Containing All] -> [Compounds]
2. Once list of all compounds appears (currently 15950 Total), choose [SmartTables] -> [Transform SmartTable] -> [Reactions of Compounds]
3. Save the SmartTable: [SmartTables] -> [Export SmartTable] -> [Tab-delimited table], and select save directory & filename. 
4. When prompted, specify [Identifiers] as the export choice. 

## Create Table 3: Genes of Reactions
1. From the [SmartTables] menu dropdown choose [Create New SmartTable] -> [Containing All] -> [Reactions]
2. Once the list fo all reactions appears (currently 15311 Reactions), choose [SmartTables] -> [Transform SmartTable] -> [Genes of a Reaction]
3. Save the SmartTable: [SmartTables] -> [Export SmartTable] -> [Tab-delimited table], and select save directory & filename. 
4. When prompted, specify [Identifiers] as the export choice. 

## Create Table 4: Compounds and IDs
1. Go to www.metacyc.org and log in.
2. Navigate to [SmartTables] -> [Special SmartTables].
3. Choose [All genes of MetaCyc].
4. If you see a warning message, make sure you create a writeable copy of the SmartTable. You should then see three options: [Add Transform Column], [Add Property Column], and [Enrichments]. 
5. [Add Property Column] -> [Database Links]. Choose: 
    + [Ensembl Human]
    + [GeneCards] - Official Gene Symbol
6. On the right-hand pane, under [Operations], choose [Export >] -> [> to Spreadsheet File], choose [frame IDs] as the "format type for values in spreadsheet", and export the SmartTable. 

## Create Table 5: Pathways and Reactions
1. From the [SmartTables] menu dropdown choose [Create New SmartTable] -> [Containing All] -> [Pathways]
2. Once list of all compounds appears (currently 2903 Total), choose [SmartTables] -> [Transform SmartTable] -> [Reactions of pathway]
3. Save the SmartTable: [SmartTables] -> [Export SmartTable] -> [Tab-delimited table], and select save directory & filename. 
4. When prompted, specify [Identifiers] as the export choice. 