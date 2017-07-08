
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
  
  ########
  # Setup Object for State Plot Interactions
  ########
  metaboliteObject <- reactiveVal()
  
  ################################################
  #                                              #
  #              Handle Interaction              #
  #                                              #
  ################################################
  
  ## Inject example df when "Try Examples" selected
  observeEvent(input$tryExamples, {
    # print(name_map)
    metaboliteObject(name_map)
  })
  
  ## Placeholder CSV handling
  observeEvent({
    input$metaboliteUpload
    # make sure this is reevaluated if the sep or header changes
    input$sep
    input$header
    }, {read_delim(file = input$metaboliteUpload$datapath, 
                   col_names = input$header, delim = input$sep) %>% 
        metaboliteObject()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$uploadSuccess <- renderText({
    input$tryExamples # make sure the try examples button is a dependency
    if (is.null(metaboliteObject()))
      return(NULL)
    paste0("Check below to see that your data has been uploaded properly.  ",
           "If so, continue to the 'Plot' tab!")
  })
  
  output$uploadedDataTable <- renderTable({
    input$tryExamples # make sure the try examples button is a dependency
    if (is.null(metaboliteObject()))
      return(NULL)
    metaboliteObject()
  })
  
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
