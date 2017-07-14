
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
  preSelectedIDType <- reactiveVal("HMDB")
  
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
  })
  
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
  output$uploadSuccess <- renderText({
    input$tryExamples # make sure the try examples button is a dependency
    if (is.null(metaboliteObject()))
      return(NULL)
    paste0("Please check below to see that your data has been uploaded properly.  ",
           "If so, pick a column and continue to the 'Map' tab!")
  })
  
  ## Once data is populated, render preview of data to user
  output$uploadedDataTable <- renderDataTable({
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
  rownames= FALSE,
  style = 'bootstrap',
  class = 'table-bordered table-responsive'
  )
  
  ## When data is populated, show column picker panel for users to select
  output$columnPickerPanel <- renderUI({
    if (!is.null(metaboliteObject())) {
      dataColumns <- names(metaboliteObject())
      tags$form(
        class = "well",
        ## For now, just allow one column. Later we can allow multiple to be chosen. 
        radioButtons("columnsPicked", "Choose Columns", dataColumns),
        selectInput("idType", "ID Type", width = "50%",
                    choices = c("CAS", "HMDB", "KEGG", "PubChem"), 
                    selected = preSelectedIDType(), selectize = FALSE),
        actionButton("continueToMap", "Proceed")
      )
    }
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
         .f = ~ addCssClass(class = "info", 
                            selector = paste0("#uploadedDataTable table td:nth-child(", .x, ")")))
    walk(.x = selectedColPositions, 
         .f = ~ addCssClass(class = "info", 
                            selector = paste0("#uploadedDataTable table th:nth-child(", .x, ")")))
    # Remove CSS classes from unselected columns
    walk(.x = unselectedColPositions, 
         .f = ~ removeCssClass(class = "info", 
                               selector = paste0("#uploadedDataTable table td:nth-child(", .x, ")")))
    walk(.x = unselectedColPositions, 
         .f = ~ removeCssClass(class = "info", 
                               selector = paste0("#uploadedDataTable table th:nth-child(", .x, ")")))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # If a column ID picked is (case insensitively) matched to an ID we already
  # have, preselect it.
  # observeEvent(input$columnsPicked, {
  #   if (tolower(input$columnsPicked) %in% c("cas", "pubchem", "hmdb", "kegg")) {
  #     preSelectedIDType(input$columnsPicked)
  #   }
  # })
  
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
    mappingOutput <- mapGenerally(importDF = metaboliteObject(), col = input$columnsPicked, 
                                  db = input$dbChosen, idType = input$idType)
    mappedMetabolites(mappingOutput)
  }, ignoreInit = TRUE)
  
  ## Once metabolites have been mapped, preview the results!
  output$mappedMetaboliteTable <- DT::renderDataTable({
    if (is.null(mappedMetabolites()))
      return(NULL)
    mappedMetabolites()
      # head(n = 20) # DT will take care of this for us. 
  }, options = list(
    pageLength = 5,
    lengthMenu = c(5, 10, 15, 20),
    # autoWidth = TRUE,
    scrollX = '100%' # AMAZING! Crucial argument to make sure DT doesn't overflow
    ),
  rownames= FALSE,
  style = 'bootstrap',
  class = 'table-bordered table-responsive compact'
  )
  
  ## If MetaCyc database was used, mention that DT is horizontally scrollable
  
  output$horizontalScrollMessage <- renderText({
    input$mapButton
    if (is.null(mappedMetabolites()))
      return(NULL)
    else if (isolate(input$dbChosen) == "MetaCyc") {
      return("Note: This table is horizontally-scrollable.")
    }
  })
  
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
  
})
