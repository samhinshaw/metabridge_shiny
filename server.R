
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
  
  ## Once data is populated, render preview of data to user
  output$uploadedDataTable <- DT::renderDataTable({
    input$tryExamples # make sure the try examples button is a dependency
    if (is.null(metaboliteObject()))
      return(NULL)
    metaboliteObject()
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
  
  dataColumns <- eventReactive({
    input$tryExamples
    input$metaboliteUpload
    input$sep
    input$header
  }, {
    names(metaboliteObject())
  })
  
  ## When data is populated, show column picker panel for users to select
  output$columnPickerPanel <- renderUI({
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
  
  # This has to be rendered separately from the column picker panel. Otherwise,
  # the entire column picker panel has to be re-rendered when the preselected ID
  # type gets updated, which resets the entire panel, which reverts to the
  # preselected column, effectively making it impossible to switch columns!
  output$idSelector <- renderUI({
    tags$div(
      selectInput("idType", "ID Type", width = "50%",
                  choices = c("HMDB", "KEGG", "PubChem", 'CAS'), 
                  selected = preSelectedIDType(),selectize = FALSE),
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
  # 
  # # If a column ID picked is (case insensitively) matched to an ID we already
  # # have, preselect it.
  # observeEvent(input$columnsPicked, {
  #   if (tolower(input$columnsPicked) %in% c("cas", "pubchem", "hmdb", "kegg")) {
  #     preSelectedIDType(input$columnsPicked)
  #   }
  # }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
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
  
  ## Once metabolites have been mapped, preview the results!
  output$mappedMetaboliteTable <- DT::renderDataTable({
    if (is.null(mappedMetabolites()))
      return(NULL)
    mappedMetabolites()
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
  
  output$horizontalScrollMessage <- renderText({
    input$mapButton
    if (is.null(mappedMetabolites()))
      return(NULL)
    else if (isolate(input$dbChosen) == "MetaCyc") {
      return("Note: This table is horizontally-scrollable.")
    }
  })
  
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
  
  selectedCompound <- reactiveVal()
  pathwaysOfSelectedRows <- reactiveVal()
  genesOfSelectedCompound <- reactiveVal()
  
  observeEvent(input$mappedMetaboliteTable_rows_selected, {
    ## Find the pathways we're interested in
    selectedMetab <- mappedMetabolites()[as.numeric(rownames(mappedMetabolites())) == 
                                         input$mappedMetaboliteTable_rows_selected,] %>% 
      extract2(input$columnsPicked)
    
    selectedCompound(selectedMetab)
    
    pathwaysOfInterest <- metabPathways %>%
      filter_("KEGG %in% selectedMetab") %>% 
      extract2("pathways")
    
    pathwaysOfInterest %<>% str_replace('map', '')
    
    pathwaysOfSelectedRows(pathwaysOfInterest)
    
    ## Find all the genes relevant to that pathway
    genesOfInterest <- mappedMetabolites() %>% 
      filter_(paste0(input$columnsPicked, " %in% ", selectedMetab)) %>% 
      extract2("Official Gene Symbol")
    
    genesOfSelectedCompound(genesOfInterest)
  })
  
  output$myImage <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    # outfile <- tempfile(fileext='.png')
    
    # Generate the PNG
    pathview(
      gene.data = genesOfSelectedCompound(),
      cpd.data = selectedCompound(),
      pathway.id = pathwaysOfSelectedRows(),
      gene.idtype = "SYMBOL",
      species = "hsa",
      kegg.dir = file.path('pathways')
    )
    
    filenames <- paste0('hsa', pathwaysOfSelectedRows(), ".pathview.png")
    
    # Return a list containing the filename
    list(src = filenames,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$debugWindow <- renderText(
    
    pathwaysOfSelectedRows()
    # Select table via DT API for row selection. UNFORTUNATELY, as far as I know
    # this must be done via row number. Fortunately, since DT is in charge of
    # all of the sorting/interaction, as well as providing the row index, there
    # should not be much disagreement
    # mappedMetabolites()[as.numeric(rownames(mappedMetabolites())) == 
    #                       input$mappedMetaboliteTable_rows_selected,]
  )
})
