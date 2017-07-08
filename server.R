
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output) {
  # hideElement(id = "columnPickerPanel")
  
  ################################################
  #                                              #
  #          Define reactive variables           #
  #                                              #
  ################################################
  
  # Reactive Object for Metabolite Data
  metaboliteObject <- reactiveVal()
  columnsOfInterest <- reactiveVal()
  
  ################################################
  #                                              #
  #            Import Tab Handlers               #
  #                                              #
  ################################################
  
  ## Inject example df when "Try Examples" selected
  observeEvent(input$tryExamples, {
    metaboliteObject(name_map)
  })
  
  ## Read CSV when (fileInput, checkboxInput, radioButtons) state changes
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
    paste0("Check below to see that your data has been uploaded properly.  ",
           "If so, continue to the 'Plot' tab!")
  })
  
  ## Once data is populated, render preview of data to user
  output$uploadedDataTable <- renderTable({
    input$tryExamples # make sure the try examples button is a dependency
    if (is.null(metaboliteObject()))
      return(NULL)
    metaboliteObject()
  })
  
  ## Now we can unhide the column picker panel
  # observeEvent(
  #   {metaboliteObject()}, {
  #       toggleElement(id = "columnPickerPanel", condition = !is.null(metaboliteObject()))
  #     }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  ## When data is populated, show columns for users to select
  output$columnPicker <- renderUI({
    if (!is.null(metaboliteObject())) {
      dataColumns <- names(metaboliteObject())
      checkboxGroupInput("columnsPicked", "Choose Columns", dataColumns)
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
  
  ################################################
  #                                              #
  #               Plot Tab Handlers              #
  #                                              #
  ################################################
  
  # Placeholder output for Output Tab
  output$databases <- renderTable({
    head(name_map)
  })
  
  # observeEvent(input$tryExamples, {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'Thank you for clicking')
  # })
  
  # Placeholder output for Plot Tab
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
})
