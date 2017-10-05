

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
  mappingObject <- reactiveVal()
  preSelectedIDType <- reactiveVal()
  databaseChosen <- reactiveVal()
  selectedMetab <- reactiveVal()
  
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
  observeEvent(input$tryExamples,
               {
                 metaboliteObject(name_map)
               },
               ignoreInit = TRUE,
               ignoreNULL = TRUE)
  
  ## Read CSV when any of (fileInput, checkboxInput, radioButtons) states change
  observeEvent({
    input$metaboliteUpload
    # make sure this is reevaluated if the sep or header changes
    input$sep
    input$header
  }, {
    read_delim(
      file = input$metaboliteUpload$datapath,
      col_names = input$header,
      delim = input$sep
    ) %>%
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
    scrollX = '100%',
    # AMAZING! Crucial argument to make sure DT doesn't overflow
    # vertical scrolling options
    scrollY = "450px",
    scrollCollapse = TRUE,
    paging = FALSE
  ),
  rownames = FALSE,
  selection = 'none',
  style = 'bootstrap',
  class = 'table-bordered table-responsive')
  
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
        "Select the column that contains the metabolites you wish to map and the ID type. ",
        br(),
        br(),
        ## For now, just allow one column. Later we can allow multiple to be chosen.
        radioButtons(
          "columnsPicked",
          "Select Column",
          dataColumns,
          # If an HMDB column exists in the data, default to that
          selected = if_else(
            condition = is_in('hmdb', tolower(dataColumns)),
            true = 'HMDB',
            false = NULL
          )
        ),
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
      selectInput(
        "idType",
        "ID Type",
        width = "50%",
        choices = c("HMDB", "KEGG", "PubChem", 'CAS'),
        selected = preSelectedIDType(),
        selectize = FALSE
      ),
      actionButton(
        inputId = "continueToMap",
        label = "Proceed",
        class = "btn btn-default",
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        `data-original-title` = "Proceed to the next tab"
      )
    )
  })
  
  ## Get column positions of selected columns and add CSS class
  observeEvent({
    input$columnsPicked
    input$metaboliteUpload
    # metaboliteObject()
  }, {
    # Vector of column positions
    possibleColPositions <- seq_along(names(metaboliteObject()))
    # Match the columns picked to their integer positions in the DF
    selectedColPositions <- input$columnsPicked %>%
      match(names(metaboliteObject()))
    # unselected columns
    unselectedColPositions <-
      setdiff(possibleColPositions, selectedColPositions)
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
  observeEvent(input$columnsPicked,
               {
                 if (tolower(input$columnsPicked) %in% c("cas", "pubchem", "hmdb", "kegg")) {
                   preSelectedIDType(input$columnsPicked)
                 }
               },
               ignoreNULL = TRUE,
               ignoreInit = TRUE)
  
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
    # change the dbChosen reactive Value
    databaseChosen(input$dbChosen)
    # Clear any pre-existing alerts
    removeUI(selector = "#mappingAlert")
    # Conduct the mapping
    mappingOutput <-
      mapGenerally(
        importDF = metaboliteObject(),
        col = input$columnsPicked,
        db = input$dbChosen,
        idType = input$idType
      )
    # Assign the mapped data to our reactive value
    mappedMetabolites(mappingOutput$data)
    # and assign the full object so we can access the status reports later
    mappingObject(mappingOutput)
    # Create new alert bubble with status message
    mappingAlert(
      status  = mappingOutput$status,
      message = mappingOutput$message,
      suggest = mappingOutput$suggest
    )
  }, ignoreInit = TRUE)
  
  ###############################################################
  #                                                             #
  # Interlude: Change ID Type to KEGG if KEGG DB is being used. #
  # Otherwise, continue to use input ID type                    #
  #                                                             #
  ###############################################################
  
  # idTypeOfInterest <- reactiveVal()
  #
  # observeEvent(input$mapButton, {
  #   if (input$dbChosen == 'KEGG') {
  #     idTypeOfInterest('KEGG')
  #   } else {
  #     idTypeOfInterest(input$idType)
  #   }
  # })
  
  ############################# End ##############################
  
  # Show a summary table of the mapped metabolites (just # of genes)
  mappingSummaryTable <- eventReactive(input$mapButton, {
    if (input$dbChosen == 'KEGG') {
      generateSummaryTable(mappingObject(), input$idType, input$dbChosen)
    } else if (input$dbChosen == 'MetaCyc') {
      generateSummaryTable(mappingObject(), input$idType, input$dbChosen)
    }
  })
  
  ## Once metabolites have been mapped, render the results!
  output$mappingSummaryTable <- DT::renderDataTable({
    mappingSummaryTable()
  },
  rownames = FALSE,
  style = 'bootstrap',
  class = 'table-bordered table-responsive compact',
  escape = FALSE,
  selection = 'single')
  
  output$mappingSummaryPanel <- renderUI({
    if (is.null(mappingObject())) {
      return(NULL)
    } else if (mappingObject()$status == 'error' |
               mappingObject()$status == 'empty') {
      return (NULL)
    } else {
      tags$div(
        tags$h3('Mapping Summary', class = "tab-header"),
        if (databaseChosen() == "KEGG") {
          tags$p(
            "If you wish to visualize your results, please select a metabolite from ",
            "this table and switch to the 'Visualize' tab."
          )
        },
        DT::dataTableOutput('mappingSummaryTable')
      )
    }
  })
  
  # When a new metabolite is selected, set it to the selected metabolite!
  observeEvent(input$mappingSummaryTable_rows_selected, {
    selectedMetab(input$mappingSummaryTable_rows_selected)
  })
  
  # But when the map button is selected, nullify and previously selected metabolites
  observeEvent(input$mapButton, {
    selectedMetab(NULL)
  })
  
  
  # Now, show the filtered (unsummarized) table, based on what the user clicked on.
  mappedMetaboliteTable <- eventReactive({
    input$mappingSummaryTable_rows_selected
    input$mapButton
  }, {
    if (mappingObject()$status == 'error' |
        mappingObject()$status == 'empty') {
      mappingObject()$data
      
    } else if (input$dbChosen == 'KEGG') {
      generateKEGGMetabTable(mappingObject(),
                             mappingSummaryTable(),
                             selectedMetab(),
                             input$idType)
      
    } else if (input$dbChosen == 'MetaCyc') {
      generateMetaCycMetabTable(mappingObject(),
                                mappingSummaryTable(),
                                selectedMetab(),
                                input$idType)
      
    }
  })
  
  ## Once metabolites have been mapped, render the results!
  output$mappedMetaboliteTable <- DT::renderDataTable({
    # Just really make sure we're not getting any errors thrown at the user
    if (mappingObject()$status == 'success' &
        input$dbChosen == 'KEGG') {
      mappedMetaboliteTable() %>% dplyr::select(-starts_with("bare"))
    } else {
      mappedMetaboliteTable()
    }
  },
  rownames = FALSE,
  style = 'bootstrap',
  class = 'table-bordered table-responsive compact',
  escape = FALSE,
  selection = 'single')
  
  output$fullMappingResultsPanel <- renderUI({
    tags$div(if (is.null(mappingObject())) {
      return(NULL)
    } else if (mappingObject()$status == 'error' |
               mappingObject()$status == 'empty') {
      tags$h3('Intermediate Results')
    } else {
      tags$h3('Per-Metabolite Mapping Results')
    },
    DT::dataTableOutput('mappedMetaboliteTable'))
  })
  
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
  observeEvent(input$remap,
               {
                 updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel")
               },
               ignoreInit = TRUE,
               ignoreNULL = TRUE)
  
  ## Once table exists, render save panel
  output$saveMappingPanel <- renderUI({
    if (!is.null(mappedMetabolites())) {
      tags$form(
        class = "well",
        tags$p("Download a copy of your full mapping results. "),
        radioButtons(
          "saveType",
          "Download Results",
          choices = c(
            'Comma-Separated Values' = 'csv',
            'Tab-Separated Values' = 'tsv'
          ),
          selected = 'csv'
        ),
        downloadButton(
          'downloadMappingData',
          'Download',
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          `data-original-title` = "Download your full mapping results."
        )
      )
    }
  })
  
  # Navigate to Viz page when KEGG was chosen
  
  output$continueToViz <- renderUI({
    # Do not render panel if no db has been mapped against yet (because
    # databaseChosen does not get input$dbChosen until "mapButton" is clicked)
    if (is.null(databaseChosen())) {
      return(NULL)
      # once mapped, render the panel
    } else {
      tags$form(class = "well",
                tags$p("Visualize Results"),
                br(),
                # If we mapped against KEGG, show visualize button
                if (databaseChosen() == "KEGG" &
                    !is.null(selectedMetab())) {
                  actionButton(inputId = "visualizeButton",
                               label = "Visualize",
                               class = "btn btn-default")
                  # But if we mapped against MetaCyc, we don't have visualizations for
                  # this yet, so disable the viz button
                } else {
                  actionButton(inputId = "visualizeButton",
                               label = "Visualize",
                               class = "btn btn-default disabled")
                })
    }
  })
  
  # Also disable viz tab in navbar when viz mapping is not possible
  observeEvent(input$mapButton, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
    }
  })
  
  observeEvent(input$mappingSummaryTable_rows_selected, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
    }
  })
  
  # When clicking "Visualize", switch to Visualize panel
  observeEvent(input$visualizeButton, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "vizPanel")
  }, ignoreInit = TRUE)
  
  ## Export data
  output$downloadMappingData <- downloadHandler(
    filename = function() {
      paste0(
        ifelse(
          test = is.null(input$metaboliteUpload),
          yes = 'example_dataset',
          no = tools::file_path_sans_ext(input$metaboliteUpload$name)
        ),
        '_mapped_',
        input$dbChosen,
        ".",
        input$saveType
      )
    },
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
  selectedRowAttrs <- reactiveValues(
    'selectedCompound' = NULL,
    'selectedCompoundName' = NULL,
    'pathwaysOfSelectedCompound' = NULL,
    'genesOfSelectedCompound' = NULL
  )
  
  # Now, when the selected row changes...
  observeEvent(input$mappingSummaryTable_rows_selected, {
    summaryTable <- mappingSummaryTable()
    fullTable <- mappedMetaboliteTable()
    
    # Map!
    pathwayMappingAttrs <- generalPathwayMapping(
      summaryTable = summaryTable,
      fullTable = fullTable,
      idType = input$idType,
      db = input$dbChosen,
      selectedRow = selectedMetab()
    )
    
    ### Assign results to their reactive values
    selectedRowAttrs$selectedCompound <-
      pathwayMappingAttrs$selectedCompound
    selectedRowAttrs$selectedCompoundName <-
      pathwayMappingAttrs$selectedCompoundName
    selectedRowAttrs$genesOfSelectedCompound <-
      pathwayMappingAttrs$genesOfSelectedCompound
    selectedRowAttrs$pathwaysOfSelectedCompound <-
      pathwayMappingAttrs$pathwaysOfSelectedCompound
    
    
  })
  
  output$pathwayPanel <- renderUI({
    ## Check for results before rendering!
    
    if (nrow(selectedRowAttrs$pathwaysOfSelectedCompound) == 0) {
      tags$div(tags$h4(paste0(
        'Pathways for Compound ', tools::toTitleCase(tolower(
          selectedRowAttrs$selectedCompoundName
        ))
      )),
      tags$p("No pathways found for this compound."))
    } else if (input$dbChosen == 'KEGG') {
      tags$div(
        tags$h4(paste0(
          'Pathways for Compound ', tools::toTitleCase(tolower(
            selectedRowAttrs$selectedCompoundName
          ))
        )),
        selectInput(
          inputId = 'pathwaysPicked',
          label = 'Pathway',
          choices = selectedRowAttrs$pathwaysOfSelectedCompound$namedPway,
          selectize = FALSE
        ),
        tags$p("Note: each pathway may take some time to process.")
      )
    } else if (input$dbChosen == 'MetaCyc') {
      tags$div(
        tags$h4(paste0(
          'Pathways for Compound ', tools::toTitleCase(tolower(
            selectedRowAttrs$selectedCompoundName
          ))
        )),
        selectInput(
          inputId = 'pathwaysPicked',
          label = 'Pathway',
          choices = selectedRowAttrs$pathwaysOfSelectedCompound$pathwayName,
          selectize = FALSE
        )
      )
    }
  })
  
  output$pathwayView <- renderImage({
    if (is.null(input$pathwaysPicked)) {
      return({
        list(
          src = './material_loading.gif',
          contentType = 'image/gif',
          width = 297,
          height = 297,
          alt = "loading..."
        )
      })
    }
    # Setup named variables for standard eval
    pathwayNameIDcol <- as.name('namedPway')
    selectedPathway <- rlang::quo(input$pathwaysPicked)
    
    # Pull the pathway ID from the pathway name selected by the user
    selectedPathwayID <-
      selectedRowAttrs$pathwaysOfSelectedCompound %>%
      filter(rlang::UQ(pathwayNameIDcol) == input$pathwaysPicked) %>%
      extract2('id')
    
    # Generate the PNG
    suppressWarnings({
      pathview(
        gene.data = selectedRowAttrs$genesOfSelectedCompound,
        cpd.data = selectedRowAttrs$selectedCompound,
        pathway.id = selectedPathwayID,
        gene.idtype = "SYMBOL",
        species = "hsa",
        kegg.dir = 'pathways'
      )
    })
    
    filename <- paste0('hsa', selectedPathwayID, ".pathview.png")
    
    # Return a list containing the filename
    list(
      src = filename,
      contentType = 'image/png',
      width = 700,
      height = 700,
      alt = paste0("Pathway map of KEGG Pathway ", input$pathwaysPicked)
    ) %>%
      return()
  }, deleteFile = TRUE)
  
  output$debugWindow <- renderTable(genesOfSelectedCompound()
                                    # Select table via DT API for row selection. UNFORTUNATELY, as far as I know
                                    # this must be done via row number. Fortunately, since DT is in charge of
                                    # all of the sorting/interaction, as well as providing the row index, there
                                    # should not be much disagreement
                                    # mappedMetabolites()[as.numeric(rownames(mappedMetabolites())) ==
                                    #                       input$mappingSummaryTable_rows_selected,])
})
  