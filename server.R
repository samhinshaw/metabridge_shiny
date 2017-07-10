
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output) {

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
  output$uploadedDataTable <- renderTable({
    input$tryExamples # make sure the try examples button is a dependency
    if (is.null(metaboliteObject()))
      return(NULL)
    metaboliteObject()
  })
  
  ## When data is populated, show column picker panel for users to select
  output$columnPickerPanel <- renderUI({
    if (!is.null(metaboliteObject())) {
      dataColumns <- names(metaboliteObject())
      tags$form(
        class = "well",
        ## For now, just allow one column. Later we can allow multiple to be chosen. 
        radioButtons("columnsPicked", "Choose Columns", dataColumns),
        selectInput("idType", "ID Type", 
                    choices = c("CAS", "HMDB", "KEGG", "PubChem"), 
                    selected = preSelectedIDType(), selectize = FALSE)
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
         .f = ~ addCssClass(class = "success", selector = paste0("#uploadedDataTable table td:nth-child(", .x, ")")))
    walk(.x = selectedColPositions, 
         .f = ~ addCssClass(class = "success", selector = paste0("#uploadedDataTable table th:nth-child(", .x, ")")))
    # Remove CSS classes from unselected columns
    walk(.x = unselectedColPositions, 
         .f = ~ removeCssClass(class = "success", selector = paste0("#uploadedDataTable table td:nth-child(", .x, ")")))
    walk(.x = unselectedColPositions, 
         .f = ~ removeCssClass(class = "success", selector = paste0("#uploadedDataTable table th:nth-child(", .x, ")")))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # If a column ID picked is (case insensitively) matched to an ID we already
  # have, preselect it.
  # observeEvent(input$columnsPicked, {
  #   if (tolower(input$columnsPicked) %in% c("cas", "pubchem", "hmdb", "kegg")) {
  #     preSelectedIDType(input$columnsPicked)
  #   }
  # })
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
  output$mappedMetaboliteTable <- renderTable({
    if (is.null(mappedMetabolites()))
      return(NULL)
    head(mappedMetabolites(), n = 20)
  })
})
