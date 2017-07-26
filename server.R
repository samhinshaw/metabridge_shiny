
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output, session) {

  ################################################
  #                                              #
  #          Define reactive variables           #
  #                                              #
  ################################################
  
  # Reactive Object for Metabolite Data
  metaboliteObject <- reactiveVal()
  mappedMetabolites <- reactiveVal()
  preSelectedIDType <- reactiveVal()
  
  ################################################
  #                                              #
  #            Welcome Tab Handlers              #
  #                                              #
  ################################################
  
  # When clicking "Get Started", switch to Upload panel
  observeEvent(input$getStarted, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel")
  }, ignoreInit = TRUE)
  
  ################################################
  #                                              #
  #            Upload Tab Handlers               #
  #                                              #
  ################################################
  
  ## Inject example df when "Try Examples" selected
  observeEvent(input$tryExamples, {
    metaboliteObject(name_map)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  ## Read CSV when any of (fileInput, checkboxInput, radioButtons) states change
  observeEvent({
    input$metaboliteUpload
    # make sure this is reevaluated if the sep or header changes
    input$sep
    input$header
    }, {read_delim(file = input$metaboliteUpload$datapath, 
                   col_names = input$header, delim = input$sep) %>% 
        metaboliteObject()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## Once data is populated, render help text to user
  output$uploadSuccess <- renderUI({
    input$tryExamples # make sure the try examples button is a dependency
    if (is.null(metaboliteObject()))
      return(NULL)
    tags$p(
      class = "conditional-help",
      "Check below to see that your data has been uploaded properly.  ",
      "If so, pick a column and proceed to the 'Map' tab!"
    )
  })
  
  # Isolate rendering of dataTable
  uploadedDataTable <- eventReactive({
    input$tryExamples
    input$metaboliteUpload
    # make sure this is reevaluated if the sep or header changes
    input$sep
    input$header
  }, {
     # make sure the try examples button is a dependency
    if (is.null(metaboliteObject())) {
      return(NULL)
    } else {
      metaboliteObject()
    }
  })
  
  ## Once data is populated, render preview of data to user
  output$uploadedDataTable <- DT::renderDataTable({
    if (is.null(uploadedDataTable())) {
      return(NULL)
    } else {
      uploadedDataTable()
    }
  }, options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 15, 20),
    # autoWidth = TRUE,
    scrollX = '100%' # AMAZING! Crucial argument to make sure DT doesn't overflow
  ),
  rownames = FALSE,
  selection = 'none',
  style = 'bootstrap',
  class = 'table-bordered table-responsive'
  )
  
  columnPickerUI <- eventReactive({
    input$tryExamples
    input$metaboliteUpload
    input$sep
    input$header
  }, {
    if (!is.null(metaboliteObject())) {
      dataColumns <- names(metaboliteObject())
      tags$form(
        class = "well",
        ## For now, just allow one column. Later we can allow multiple to be chosen. 
        radioButtons("columnsPicked", "Choose Columns", dataColumns, 
                     # If an HMDB column exists in the data, default to that
                     selected = if_else(
                       condition = is_in('hmdb', tolower(dataColumns)),
                       true = 'HMDB',
                       false = NULL
                     )),
        uiOutput('idSelector')
      )
    }
  })
  
  ## When data is populated, show column picker panel for users to select
  output$columnPickerPanel <- renderUI({
    columnPickerUI()
  })
  
  # This has to be rendered separately from the column picker panel. Otherwise,
  # the entire column picker panel has to be re-rendered when the preselected ID
  # type gets updated, which resets the entire panel, which reverts to the
  # preselected column, effectively making it impossible to switch columns!
  output$idSelector <- renderUI({
    tags$div(
      selectInput("idType", "ID Type", width = "50%",
                  choices = c("HMDB", "KEGG", "PubChem", 'CAS'), 
                  selected = preSelectedIDType(), selectize = FALSE),
      actionButton("continueToMap", "Proceed")
    )
  })
  
  ## Get column positions of selected columns and add CSS class
  observeEvent(input$columnsPicked, {
    # Vector of column positions
    possibleColPositions <- seq_along(names(metaboliteObject()))
    # Match the columns picked to their integer positions in the DF
    selectedColPositions <- input$columnsPicked %>%
      match(names(metaboliteObject()))
    # unselected columns
    unselectedColPositions <- setdiff(possibleColPositions, selectedColPositions)
    # Add CSS classes to selected columns
    walk(.x = selectedColPositions,
         .f = ~ addCssClass(
           class = "info",
           selector = paste0("#uploadedDataTable table td:nth-child(", .x, ")")
         ))
    walk(.x = selectedColPositions,
         .f = ~ addCssClass(

           class = "info",
           selector = paste0("#uploadedDataTable table th:nth-child(", .x, ")")
         ))
    # Remove CSS classes from unselected columns
    walk(.x = unselectedColPositions,
         .f = ~ removeCssClass(
           class = "info",
           selector = paste0("#uploadedDataTable table td:nth-child(", .x, ")")
         ))
    walk(.x = unselectedColPositions,
         .f = ~ removeCssClass(
           class = "info",
           selector = paste0("#uploadedDataTable table th:nth-child(", .x, ")")
         ))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # If a column ID picked is (case insensitively) matched to an ID we already
  # have, preselect it.
  observeEvent(input$columnsPicked, {
    if (tolower(input$columnsPicked) %in% c("cas", "pubchem", "hmdb", "kegg")) {
      preSelectedIDType(input$columnsPicked)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## Switch to Map panel when "Proceed" is clicked on Upload tab
  observeEvent(input$continueToMap, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "mapPanel")
  }, ignoreInit = TRUE)
  
  ################################################
  #                                              #
  #                Map Tab Handlers              #
  #                                              #
  ################################################
  
  ## Here's where the heavy lifting takes place!!
  ## We now take the columns the user specified and map them to genes!
  ## Even better, now that we have a UI, we can choose 
  observeEvent(input$mapButton, {
    # Clear any pre-existing alerts
    removeUI(selector = "#mappingAlert")
    # Conduct the mapping
    mappingOutput <- mapGenerally(importDF = metaboliteObject(), col = input$columnsPicked, 
                                  db = input$dbChosen, idType = input$idType)
    # Assign the mapped data to our reactive value
    mappedMetabolites(mappingOutput$data)
    # Create new alert bubble with status message
    mappingAlert(status = mappingOutput$status, 
                 message = mappingOutput$message, 
                 suggest = mappingOutput$suggest)
  }, ignoreInit = TRUE)
  
  # Draw table in isolated environment
  mappedMetaboliteTable <- eventReactive(input$mapButton, {
    # Should never be null since we're not responding until map button is
    # clicked, but good to have just in case
    if (is.null(mappedMetabolites())) {
      return(NULL)
    } else {
      mappedMetabolites() %>% group_by_(input$idType, 'Compound') %>% summarize(
        "# of Genes (MetaCyc Gene ID)" = n_distinct(`MetaCyc Gene ID`),
        "# of Genes (Official Gene Symbol)" = n_distinct(`Official Gene Symbol`),
        "# of Genes (Ensembl Gene ID)" = n_distinct(`Ensembl Gene ID`)
      )
    }
  }, ignoreInit = TRUE)
  
  ## Once metabolites have been mapped, preview the results!
  output$mappedMetaboliteTable <- DT::renderDataTable({
    # Just really make sure we're not getting any errors thrown at the user
    if (is.null(mappedMetaboliteTable())) {
      return(NULL)
    } else {
      mappedMetaboliteTable()
    }
  }, options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 15, 20),
    # autoWidth = TRUE,
    scrollX = '100%' # AMAZING! Crucial argument to make sure DT doesn't overflow
    ),
  rownames= FALSE,
  style = 'bootstrap',
  class = 'table-bordered table-responsive compact', 
  escape = FALSE,
  selection = 'single'
  )
  
  # output$mappingTableTitle <- renderUI({
  #   if (!is.null(mappedMetabolites())){
  #     tags$h2("Your Input")
  #   }
  # })
  
  ## If MetaCyc database was used, mention that DT is horizontally scrollable
  
  # output$horizontalScrollMessage <- renderText({
  #   input$mapButton
  #   if (is.null(mappedMetabolites()))
  #     return(NULL)
  #   else if (isolate(input$dbChosen) == "MetaCyc") {
  #     return("Note: This table is horizontally-scrollable.")
  #   }
  # })
  
  ## Watch for the "try again" button that will be rendered if an error occurs in mapping
  observeEvent(input$remap, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  ## Once table exists, render save panel
  output$saveMappingPanel <- renderUI({
    if (!is.null(mappedMetabolites())) {
      tags$form(
        class = "well",
        radioButtons("saveType", "Download Results",
                     choices = c('Comma-Separated Values' = 'csv', 
                                 'Tab-Separated Values' = 'tsv'), 
                     selected = 'csv'),
        downloadButton('downloadMappingData', 'Download')
      )
    }
  })
  
  ## Export data
  output$downloadMappingData <- downloadHandler(
    filename = function() { paste0('mapped_metabolites_', input$dbChosen, ".", input$saveType) },
    content  = function(file) {
      write_delim(mappedMetabolites(), file, 
                  delim = switch(input$saveType, 
                                 "csv" = ",", 
                                 "tsv" = "\t"))
    }
  )
  ################################################
  #                                              #
  #                Viz Tab Handlers              #
  #                                              #
  ################################################
  
  # Set up reactive values for: 
  # - The selected compound of the clicked row
  # - The pathways that compound is involved in
  # - The genes (for the enzymes) that compound interacts with
  selectedCompound           <- reactiveVal()
  selectedCompoundName       <- reactiveVal()
  pathwaysOfSelectedCompound <- reactiveVal()
  genesOfSelectedCompound    <- reactiveVal()
  
  # Now, when the selected row changes...
  observeEvent(input$mappedMetaboliteTable_rows_selected, {
    
    ### Pull the selected row and extract its compound ID
    selectedMetab <- mappedMetabolites()[as.numeric(rownames(mappedMetabolites())) == 
                                         input$mappedMetaboliteTable_rows_selected,] %>% 
      extract2(input$columnsPicked)
    
    ### Assign that to its reactive value
    selectedCompound(selectedMetab)
    
    ### Pull the selected row and extract its compound Name
    selectedMetabName <- mappedMetabolites()[as.numeric(rownames(mappedMetabolites())) == 
                                           input$mappedMetaboliteTable_rows_selected,] %>% 
      extract2('Compound')
    
    ### Assign that to its reactive value
    selectedCompoundName(selectedMetabName)
    
    ### Quote necessary variables for dplyr
    
    # To be treated like a variable
    namedSelectedCols <- as.name(input$columnsPicked)
    # To be treated like a character string
    quotedSelectedMetab <- rlang::enquo(selectedMetab)
    
    ### Pull out the pathways that our compound is present in from the
    ### metabPathways object stored in `data/`
    pathwaysOfInterest <- metabPathways %>%
      filter(rlang::UQ(namedSelectedCols) == rlang::UQ(quotedSelectedMetab))
    
    ### Assign that to its reactive value
    pathwaysOfSelectedCompound(pathwaysOfInterest)
    
    ## Find all the genes that compound interacts with (from our initial mapping
    ## table)
    genesOfInterest <- mappedMetabolites() %>% 
      filter(rlang::UQ(namedSelectedCols) == rlang::UQ(quotedSelectedMetab)) %>% 
      magrittr::extract2("Official Gene Symbol")
    
    ### Assign that to its reactive value
    genesOfSelectedCompound(genesOfInterest)
  })
  
  output$pathwayPanel <- renderUI({
    tags$div(
      tags$h4(paste0('Pathways for Compound ', tools::toTitleCase(tolower(selectedCompoundName())))),
      selectInput(inputId = 'pathwaysPicked', label = 'Pathway', 
                  choices = pathwaysOfSelectedCompound()$namedPway,
                  selectize = FALSE)
    )
  })
  
  output$pathwayView <- renderImage({
    if(is.null(input$pathwaysPicked)) {
      return({
        list(src = 'material_loading.gif',
             contentType = 'image/png',
             width = 297,
             height = 297,
             alt = "loading...")
      })
    }
    # Setup named variables for standard eval
    pathwayNameIDcol <- as.name('namedPway')
    selectedPathway <- rlang::quo(input$pathwaysPicked)
    
    # Pull the pathway ID from the pathway name selected by the user
    selectedPathwayID <- pathwaysOfSelectedCompound() %>% 
      filter(rlang::UQ(pathwayNameIDcol) == input$pathwaysPicked) %>% 
      extract2('id')
    
    # Generate the PNG
    suppressWarnings({
      pathview(
        gene.data = genesOfSelectedCompound(),
        cpd.data = selectedCompound(),
        pathway.id = selectedPathwayID,
        gene.idtype = "SYMBOL",
        species = "hsa",
        kegg.dir = 'pathways'
      )
    })
    
    filename <- paste0('hsa', selectedPathwayID, ".pathview.png")
    
    # Return a list containing the filename
    list(src = filename,
         contentType = 'image/png',
         width = 700,
         height = 700,
         alt = paste0("Pathway map of KEGG Pathway ", input$pathwaysPicked)) %>% 
      return()
  }, deleteFile = TRUE)
  
  output$debugWindow <- renderTable(
    
    genesOfSelectedCompound()
    # Select table via DT API for row selection. UNFORTUNATELY, as far as I know
    # this must be done via row number. Fortunately, since DT is in charge of
    # all of the sorting/interaction, as well as providing the row index, there
    # should not be much disagreement
    # mappedMetabolites()[as.numeric(rownames(mappedMetabolites())) == 
    #                       input$mappedMetaboliteTable_rows_selected,]
  )
})
