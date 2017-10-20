

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output, session) {

  # Wait for sessionInitialized to load packages. This does not have to be defined
  # in your UI, as the input will be passed via Shiny.onInputChange()
  observeEvent(input$sessionInitialized, {
    library(shinyjs)
    library(tools)
    library(tidyverse)
    library(stringr) # moved to observeEvent in server.R
    library(magrittr) # moved to first invocation in mapGenerally.R
    library(rlang) # moved to first invocation in mapGenerally.R
    library(DT) # moved to generateTables.R
    library(pathview) # moved to visualizePathways.R
    library(shinycssloaders)
    # After packages loaded, run button transform to signal ready states
    runjs('handlers.initGetStarted();')
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # 'Disable' the Viz tab on load
  # runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")

  ################################################
  #                                              #
  #          Define reactive variables           #
  #                                              #
  ################################################
  
  # Reactive Values for Metabolite Data. These are isolated into individual
  # reactive values so we can depend on them for reactive changes
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
    input$sep
    input$header
  }, {
    if(!is.null(input$metaboliteUpload)) {
      read_delim(
        file = input$metaboliteUpload$datapath,
        col_names = input$header,
        delim = input$sep
      ) %>%
        # and save to the reactiveVal
        metaboliteObject()
    }
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
    if (is.null(metaboliteObject())) {
      # Return null if nothing so that we don't pass an error
      return(NULL)
    } else {
      # Render the (reactive value) uploadedDataTable
      metaboliteObject()
    }
  # DataTables options
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

  output$uploadedTablePanel <- renderUI({
    tags$div(
      class = "col-sm-9",
      uiOutput('uploadSuccess'), 
      dataTableOutput('uploadedDataTable')# %>% withSpinner(type = 8, color = '#303E4E')
    )
  })
  
  # Render the UI for the column picker panel
  columnPickerUI <- eventReactive({
    # Change on button click (upload/examples)
    input$tryExamples
    input$metaboliteUpload
    # OR on header change
    input$sep
    input$header
  }, {
      # Only render if NOT NULL
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
        # Dynamically render the idType selector panel here (see below) This is
        # intentionally separate to that we do not have a feedback loop that
        # triggers re-rendering. Otherwise, as soon as you change this value,
        # the entire panel re-renders, switching it back to its default. 
        uiOutput('idSelector')
      )
    }
  })
  
  # When data is populated, show column picker panel for users to select. This
  # is separate from the actual code to render so that we can only depend on
  # specific events triggering re-renders.
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
      # Include button to proceed. 
      actionButton(
        inputId = "continueToMap",
        label = "Proceed",
        class = "btn-med css-tooltip",
        title = "Proceed to mapping your metabolites"
        # `data-toggle` = "tooltip",
        # `data-placement` = "right",
        # `data-original-title` = "Proceed to the next tab"
      )
    )
  })
  
  ## Get column positions of selected columns and add CSS class to make the selection obvious!
  observeEvent({
    # Re-run when column picked changes
    input$columnsPicked
    # OR when the dataframe changes
    input$tryExamples
    input$metaboliteUpload
    # OR on header change
    input$sep
    input$header
  }, {
    # Vector of column positions
    possibleColPositions <- seq_along(names(metaboliteObject()))
    # Match the columns picked to their integer positions in the DF
    selectedColPositions <- input$columnsPicked %>%
      match(names(metaboliteObject()))
    # Get the unselected columns
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
  
  # If the selected ID type is a column name in the DF, preselect that
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

  # when the map button is clicked, update the dbChosen. 
  observeEvent(input$mapButton, {
    # change the dbChosen reactive Value
    databaseChosen(input$dbChosen)
    # Clear any pre-existing alerts
    removeUI(selector = "#mappingAlert")
    # Conduct the mapping from our mapGenerally() function defined in mapGenerally.R
    mappingOutput <- mapGenerally(
        importDF = metaboliteObject(),
        col = input$columnsPicked,
        db = databaseChosen(),
        idType = input$idType
      )
    # Assign the mapped data to our reactive value
    mappedMetabolites(mappingOutput$data)
    # and assign the full object so we can access the status reports later
    mappingObject(mappingOutput)
    # Create new alert bubble with status message
    # Add optional tweet link? Or email link with error message?
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
  #   if (databaseChosen() == 'KEGG') {
  #     idTypeOfInterest('KEGG')
  #   } else {
  #     idTypeOfInterest(input$idType)
  #   }
  # })
  
  ############################# End ##############################
  
  # THREE STEP RENDER PROCESS
  # 1. Generate Table from `generateTables.R::generateSummaryTable()`, depending
  #    only on the mapButton click. 
  # 2. Render the generated table with DR::renderDataTable(). This is separate
  #    from #1 because we need to assign the reactive table object to its own
  #    output Object. 
  # 3. Render the entire UI surrounding the table and insert the rendered DT. 

  ## STEP ONE
  # ~~~~~~~~~~
  # Show a summary table of the mapped metabolites (just # of genes)
  # This calls generateSummaryTable() from generateTables.R
  # We should make this optional!
  # Only render when 'map' clicked
  mappingSummaryTable <- eventReactive(input$mapButton, {
    if (databaseChosen()== 'KEGG') {
      generateSummaryTable(mappingObject(), input$idType, databaseChosen())
    } else if (databaseChosen() == 'MetaCyc') {
      generateSummaryTable(mappingObject(), input$idType, databaseChosen())
    }
  })
  
  ## STEP TWO
  # ~~~~~~~~~~
  # Once metabolites have been mapped, render the results!
  output$mappingSummaryTable <- DT::renderDataTable(
    { mappingSummaryTable() },
    rownames = FALSE,
    style = 'bootstrap',
    class = 'table-bordered table-responsive compact',
    escape = FALSE,
    selection = 'single'
  )
  
  ## STEP THREE
  # ~~~~~~~~~~
  # Render the panel separately so we have reactive control over all the UI
  # elements surrounding the DataTable, not just the dataTable
  output$mappingSummaryPanel <- renderUI({
    if (is.null(mappingObject())) {
      return(NULL)
    } else if (mappingObject()$status == 'error' |
               mappingObject()$status == 'empty') {
      return(NULL)
      # Only render if we had non-null, non-error, non-empty results
    } else {
      tags$div(
        tags$h3('Mapping Summary', class = "tab-header"),
        if (databaseChosen() == "KEGG") {
          tags$p(
            "If you wish to visualize your results, please select a metabolite from ",
            "this table and switch to the 'Visualize' tab."
          )
        },
        # Insert the datatable here that we rendered above. 
        DT::dataTableOutput('mappingSummaryTable')
      )
    }
  })
  
  # When a new metabolite is selected, set it to the selected metabolite reactive value!
  # Why? So we can reset it given other certain conditions (see the next function)
  observeEvent(input$mappingSummaryTable_rows_selected, {
    selectedMetab(input$mappingSummaryTable_rows_selected)
  })
  
  # But when the map button is selected, nullify any previously selected metabolite
  observeEvent(input$mapButton, {
    selectedMetab(NULL)
  })

  
  # THREE STEP RENDER PROCESS
  # 1. Generate Table from `generateTables.R::generateSummaryTable()`, depending
  #    only on the mapButton click. 
  # 2. Render the generated table with DR::renderDataTable(). This is separate
  #    from #1 because we need to assign the reactive table object to its own
  #    output Object. 
  # 3. Render the entire UI surrounding the table and insert the rendered DT. 
  
  ## STEP ONE 
  ## Generate table
  ## ~~~~~~~~~~
  # Now, show the filtered (unsummarized) table, based on what metabolite user clicked on.
  mappedMetaboliteTable <- eventReactive({
    # When we select a new metabolite in the summary table...
    input$mappingSummaryTable_rows_selected
    # or when we click the map button... (this is important because we need to be
    # able to update in case there are errors we need to display)
    input$mapButton
  }, {
    # Pull the $data object from the tryCatch output if there was an error. This
    # should default to the previous successful step. 
    if (mappingObject()$status == 'error' |
        mappingObject()$status == 'empty') {
        mappingObject()$data
      
      # Otherwise, generate our table depending on the chosen database! As with
      # `generateSummaryTable()`, these functions come from `generateTables.R`
    } else if (databaseChosen() == 'KEGG') {
      generateKEGGMetabTable(mappingObject(),
                             mappingSummaryTable(),
                             selectedMetab(),
                             input$idType)
      
    } else if (databaseChosen() == 'MetaCyc') {
      generateMetaCycMetabTable(mappingObject(),
                                mappingSummaryTable(),
                                selectedMetab(),
                                input$idType)
      
    }
  })
  
  ## STEP TWO
  ## Render generated table
  ## ~~~~~~~~~~
  # Once metabolites have been mapped, render the results!
  output$mappedMetaboliteTable <- DT::renderDataTable({
    # Just really make sure we're not getting any errors thrown at the user
    if (mappingObject()$status == 'success' &
        databaseChosen() == 'KEGG') {
      # Exclude the 'bare' columns, which are redundant to the HTML-ified columns. 
      mappedMetaboliteTable() %>% dplyr::select(-starts_with("bare"))
      # Only render if we had non-null, non-error, non-empty results
    } else {
      mappedMetaboliteTable()
    }
  },
  rownames = FALSE,
  style = 'bootstrap',
  class = 'table-bordered table-responsive compact',
  escape = FALSE,
  selection = 'single')
  
  ## STEP THREE
  ## Render entire UI output, including the rendered table
  # ~~~~~~~~~~
  output$fullMappingResultsPanel <- renderUI({
    tags$div(if (is.null(mappingObject())) {
      return(NULL)
      # If we had an error, change the header to reflect that these are intermediate results
    } else if (mappingObject()$status == 'error' |
               mappingObject()$status == 'empty') {
      tags$h3('Intermediate Results')
    # Only render if we had non-null, non-error, non-empty results
    } else {
      tags$h3('Per-Metabolite Mapping Results')
    },
    # Rendered table from STEP TWO goes here!
    DT::dataTableOutput('mappedMetaboliteTable'))
  })
  
  ## Watch for the "try again" button that will be rendered if an error occurs in mapping
  observeEvent(input$remap,
    { updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel") },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
  )
  
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
        # With a tooltip
        downloadButton(
          'downloadMappingData',
          'Download',
          class = "btn-med css-tooltip",
          title = "Download your full mapping results"
          # `data-toggle` = "tooltip",
          # `data-placement` = "right",
          # `data-original-title` = "Download your full mapping results."
        )
      )
    }
  })
  
  # Navigate to Viz page when KEGG was chosen

  ##############################################
  #                                            #
  #      UPDATE TO REMOVE DISABLED BUTTON      #
  #       It's simply bad user experience      #
  #                                            #
  ##############################################
  
  output$continueToViz <- renderUI({
    # Do not render panel if no db has been mapped against yet (because
    # databaseChosen does not get input$dbChosen until "mapButton" is clicked)
    if (is.null(databaseChosen())) {
      return(NULL)
      # once mapped, render the panel
    } else if (databaseChosen() == "MetaCyc") {
      return(NULL)
    } else {
      tags$form(class = "well",
                tags$label("Visualize Results"),
                tags$p("If you mapped against KEGG, you have the option to visualize your results with pathview."),
                br(),
                # If we mapped against KEGG, show visualize button
                if (databaseChosen() == "KEGG" &
                    !is.null(selectedMetab())) {
                  actionButton(inputId = "visualizeButton",
                               label = "Visualize",
                               class = "btn btn-med css-tooltip", 
                               title = "Visualize your results with pathview")
                  # But if we mapped against MetaCyc, we don't have visualizations for
                  # this yet, so disable the viz button
                } else {
                  actionButton(inputId = "visualizeButton",
                               label = "Visualize",
                               class = "btn btn-med css-tooltip disabled", 
                               title = "Select a metabolite from the summary table")
                })
    }
  })
  
  # Client-side JS to enable/disable viz tab!
  # Also disable viz tab in navbar when viz mapping is not possible
  # Make sure that we have a tooltip explaining why the viz tab is disabled as well
  observeEvent(input$mapButton, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").removeClass('panel-tooltip');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").addClass('panel-tooltip');")
    }
  })
  
  # Client-side JS to enable/disable viz tab!
  observeEvent(input$mappingSummaryTable_rows_selected, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").removeClass('panel-tooltip');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").addClass('panel-tooltip');")
    }
  })
  
  # When clicking "Visualize", switch to Visualize panel
  observeEvent(input$visualizeButton, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "vizPanel")
  }, ignoreInit = TRUE)
  
  ## Export data
  output$downloadMappingData <- downloadHandler(
    # Name file: `originalFilename_mapped_dbChosen.savetype`
    filename = function() {
      paste0(
        ifelse(
          test = is.null(input$metaboliteUpload),
          yes = 'example_dataset',
          no = tools::file_path_sans_ext(input$metaboliteUpload$name)
        ),
        '_mapped_',
        databaseChosen(),
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
      db = databaseChosen(),
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
  
  # Render the pathway panel once
  output$pathwayPanel <- renderUI({
    ## Check for results before rendering!
    if (nrow(selectedRowAttrs$pathwaysOfSelectedCompound) == 0) {
      tags$div(tags$h4(paste0(
        'Pathways for Compound ', tools::toTitleCase(tolower(
          selectedRowAttrs$selectedCompoundName
        ))
      )),
      tags$p("No pathways found for this compound."))
    } else if (databaseChosen() == 'KEGG') {
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
    } else if (databaseChosen() == 'MetaCyc') {
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
  
  ## have conditional renderUI here instead, so we can add our loading image with JS
  ## Different div ID depending on the image
  ## Perhaps can style this div to be full screen size

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
    
  source(file.path('functions', 'visualizePathways.R'))
  filename <- visualizePathview(
    pathway = selectedPathwayID, 
    genes = selectedRowAttrs$genesOfSelectedCompound, 
    cpd = selectedRowAttrs$selectedCompound
  )
    
    # Return a list containing the filename
    return(list(
      src = filename,
      contentType = 'image/png',
      width = 700,
      height = 700,
      alt = paste0("Pathway map of KEGG Pathway ", input$pathwaysPicked)
    ))
  }, deleteFile = TRUE)

  ## Render entire UI for vizPanel

  output$vizPanelUI <- renderUI({
    if (is.null(databaseChosen())) {
      tags$div(
        tags$h2('Pathway View', class = "tab-header"),
        # Nothing to map alert
        tags$div(
          class = "alert alert-dismissible alert-danger",
          tags$button(
            HTML("&times;"),
            type = "button",
            class = "close",
            `data-dismiss` = "alert"
          ),
          "There is nothing selected to map!"
        )
      )
    } else if (databaseChosen() == "MetaCyc"){
      tags$div(
        tags$h2('Pathway View', class = "tab-header"),
        # Nothing to map alert
        tags$div(
          class = "alert alert-dismissible alert-danger",
          tags$button(
            HTML("&times;"),
            type = "button",
            class = "close",
            `data-dismiss` = "alert"
          ),
          "You must map via KEGG to visualize your results with pathview!"
        )
      )
    } else if (is.null(selectedMetab())){
      tags$div(
        tags$h2('Pathway View', class = "tab-header"),
        # Nothing to map alert
        tags$div(
          class = "alert alert-dismissible alert-danger",
          tags$button(
            HTML("&times;"),
            type = "button",
            class = "close",
            `data-dismiss` = "alert"
          ),
          "You must select a metabolite to visualize your results with pathview!"
        )
      )
    } else {
      tags$div(
        # Manual Sidebar
        tags$div(class = "col-sm-3 manual-sidebar",
                # Allow user to pick which pathway that the selected 
                # metabolite participates in to view
                 tags$form(class = "well",
                           uiOutput('pathwayPanel'))),
        # Pathway visualization
        tags$div(
          class = "col-sm-9",
          tags$h2('Pathway View', class = "tab-header"),
          imageOutput('pathwayView') %>% withSpinner(type = 8, color = '#303E4E')
        )
      )
    }
  })
  
  output$debugWindow <- renderTable(genesOfSelectedCompound()
    # Select table via DT API for row selection. UNFORTUNATELY, as far as I know
    # this must be done via row number. Fortunately, since DT is in charge of
    # all of the sorting/interaction, as well as providing the row index, there
    # should not be much disagreement
    # mappedMetabolites()[as.numeric(rownames(mappedMetabolites())) ==
    #                       input$mappingSummaryTable_rows_selected,])
  )
})
  