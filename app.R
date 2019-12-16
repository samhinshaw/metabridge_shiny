
# Load the first couple libraries -----------------------------------------

# Most libraries and functions are loaded through a call to `deferred.R` at the
# beginning of the `server()` function (#429).
library(shiny)
library(shinyjs)


# Useful colours which match the flatly theme:
# Dark blue     #2c3e50
# Turquoise     #18bc9c
# Light blue    #3498db
# DT blue       #0075b0
# Grey          #ecf0f1
# White         #fff


# TODO Need to run these lines each time app is published so packages from
# Bioconductor can be found by Shiny.
# library(BiocManager)
# options(repos = BiocManager::repositories())


# Define UI part of the app -----------------------------------------------

# Workaround to ensure logo in top left corner (tab bar) is found/rendered when
# app is published.
addResourcePath(prefix = "pics", directoryPath = "./www")

ui <- fluidPage(

  # Head linking to Flatly bootstrap theme and my personal tweaks.
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/user.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/tippy.css"),

    # Favicon options
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "/apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
    tags$link(rel = "manifest", href = "/manifest.json"),
    tags$link(rel = "mask-icon", href = "/safari-pinned-tab.svg", color = "#303e4e"),
    tags$meta(name = "theme-color", content = "#303e4e"),

    HTML(
      "<!-- Global site tag (gtag.js) - Google Analytics -->
      <script async src='https://www.googletagmanager.com/gtag/js?id=UA-123892284-1'></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());

        gtag('config', 'UA-123892284-1');
      </script>
      "
    )
  ),

  ### Begin the tab bar layout
  navbarPage(
    title = htmltools::HTML("<img src='pics/logo_white.svg' alt='' height='28'"), # <sup class='tiny'>BETA</sup>
    id = "navbarLayout",
    position = "fixed-top",
    windowTitle = "MetaBridge",
    collapsible = TRUE,

    # Make sure we enable ShinyJS. We also add the `tags$style()` call to add
    # space between the navbar and body content; otherwise the navbar would
    # overlap the elements below it (caused by fixed navbar).
    header = tagList(
      useShinyjs(),
      tags$style(type = "text/css", "body {padding-top: 80px;}")
    ),

    ### Welcome tab/landing page
    tabPanel(
      title = "MetaBridge",
      value = "welcomePanel",

      # Main panel that will contain text and links
      tags$div(
        id = "welcomeHero",
        class = "jumbotron",

        h1("Welcome"),

        tags$hr(),

        tags$div(
          class = "logoWrapper",

          tags$p(
            "Welcome to MetaBridge v1.2, a user-friendly web tool for ",
            "network-based integrative analysis of metabolomics data. Here you ",
            "can upload a set of metabolites and identify the directly ",
            "interacting enzymes for network integration."
          ),
          tags$p(
            "To start, you'll want a set of metabolites as",
            "HMDB, KEGG, PubChem, or CAS IDs. We recommend",
            tags$a("MetaboAnalyst", href = "http://www.metaboanalyst.ca"),
            "for metabolomics data processing, as well as ID conversion ",
            "if you have only compound names."
          ),
          tags$p(
            "With the output of MetaBridge, you can create a ",
            "protein-protein interaction network representative ",
            "of your metabolomics data. We recommend",
            tags$a("NetworkAnalyst", href = "http://www.networkanalyst.ca"),
            "for generation of these networks and for network-based integration ",
            "with protein-protein interaction networks created from other omics types."
          ),

          tags$p(
            "MetaBridge was developed at the ",
            tags$a("Hancock Lab", href = "http://cmdr.ubc.ca/bobh/"),
            "using data from MetaCyc (Version 23.0) and KEGG (Release 92)."
          ),

          tags$p(
            "Click the button below to Get Started! If you'd like to learn more ",
            "about how MetaBridge can be used, check the Tutorial. For more ",
            "information, refer to the About page."
          ),

          tags$br(),

          div(
            # Buttons linking to various tabs of the app. To see how these
            # buttons are hidden, refer to `www/js/client.js`.
            # First the button which shows the app loading, then links to the
            # Upload panel.
            actionButton(
              inputId = "getStarted",
              label = "Initializing App...",
              class = "btn-primary btn-lg disabled", # btn-tooltip
              `data-position` = "bottom",
              icon("circle-o-notch", class = "fa fa-spin", lib = "font-awesome")
            ),

            # Horizontal spacer
            HTML("&nbsp;&nbsp;&nbsp;"),

            # Linking to the Tutorials page
            actionButton(
              inputId = "tutorial",
              label = "Tutorial",
              class = "btn-success btn-lg btn-tooltip btn-hidden", # btn-tooltip
              `data-position` = "bottom",
              style = "width: 155px",
              title = "Learn how to use MetaBridge for integrative analysis."
            ),

            # Horizontal spacer
            HTML("&nbsp;&nbsp;&nbsp;"),

            # Button linking straight to the About page
            actionButton(
              inputId = "about",
              label = "About",
              class = "btn-primary btn-lg btn-tooltip btn-hidden", # btn-tooltip
              style = "color: #fff; background-color: #3498db; border-color: #3498db; width: 155px",
              `data-position` = "bottom",
              title = "Learn more about MetaBridge."
            )
          )
        )
      ),
      # Separate div to include the lab logo below the main section. Also made
      # into a clickable link!
      tags$div(
        style = "padding-top: 5vw; padding-bottom: 10px; padding-right: 10px",
        tags$footer(htmltools::HTML(
          "<a href='http://cmdr.ubc.ca/bobh/'> <img src = 'pics/hancock-lab-logo.svg'> </a>"
        ))
      )
    ),


    ### Upload panel
    tabPanel(
      "Upload",
      value = "uploadPanel",

      # Sidebar
      tags$div(
        class = "col-sm-3 manual-sidebar",

        # Separate form 'wells' within the sidebar (custom CSS class)
        tags$form(
          class = "well",
          tags$p(
            "Upload a plain-text spreadsheet (CSV or TSV) containing ",
            "your metabolites of interest in a single column, or try ",
            "out our example dataset."
          ),

          # Upload handling. Note that the "Browse..." button is customized in
          # `www/css/user.css`
          fileInput(
            inputId = "metaboliteUpload",
            label = "Upload Metabolites",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              "text/tab-separated-values"
            )
          ),

          # Header in file?
          checkboxInput(
            inputId = "header",
            label   = "Header",
            value   = TRUE
          ),

          # TSV or CSV?
          radioButtons(
            inputId = "sep",
            label = "Separator",
            choices = c(Comma = ",", Tab = "\t", Semicolon = ";"),
            selected = ","
          ),

          # OR, try our example!
          actionLink(
            inputId = "tryExamples",
            class = "btn btn-link btn-med btn-tooltip",
            `data-position` = "right",
            label = tags$b("Try Examples"),
            style = "font-size:110%",
            title = "Try an example dataset from MetaboAnalyst"
          )
        ),

        # Show the columns of the uploaded file
        uiOutput("columnPickerPanel")
      ),

      # Display the file that was uploaded
      uiOutput("uploadedTablePanel")
    ),


    ### Mapping Panel
    tabPanel(
      title = "Map",
      value = "mapPanel",

      # Manual Sidebar
      tags$div(
        class = "col-sm-3 manual-sidebar",
        id = "mapPanelSidebar",

        tags$form(
          class = "well",

          tags$p(
            "Choose a database to map with. MetaCyc has higher quality ",
            "annotations, but KEGG may yield more hits. If you map via KEGG, ",
            "you also have the option to visualize your results."
          ),

          # Choose database for mapping.
          radioButtons(
            "dbChosen",
            "Choose Database",
            choices = c("MetaCyc", "KEGG"),
            selected = "MetaCyc"
          ),

          # Map!
          actionButton(
            "mapButton",
            tags$b("Map"),
            class = "btn-primary btn-tooltip",
            `data-position` = "right",
            title = "Map your metabolites to the selected database"
          )
        ),

        # Let the user download their results
        uiOutput("saveMappingPanel"),

        # Show panel for continuing to visualization results
        uiOutput("continueToViz")
      ),

      # Display mapping results
      tags$div(
        class = "col-sm-9",

        # Show summary table.
        uiOutput("mappingSummaryPanel"),

        # Show FULL results for a selected metabolite.
        uiOutput("fullMappingResultsPanel")
      )
    ),


    ### Visualize With Pathview
    tabPanel(
      title = "Pathview",
      value = "vizPanel",
      id = "visualizationPanel",
      class = "viz-panel",
      uiOutput("vizPanelUI")
    ),

    # Finally, the 'More' Panel, with about, help, etc.
    navbarMenu(
      # Overall title which contains links to the specific tabs
      "Help",

      ### Tutorial Page
      tabPanel(
        title = "Tutorial",
        value = "tutorialPanel",

        tags$div(
          class = "jumbotron",

          tags$h1("Tutorial"),

          tags$hr(),

          tags$div(
            class = "logoWrapper",

            tags$h2("Network-Based Integrative Analysis with MetaBridge"),

            tags$p(
              "Below you will find a sample workflow for integrating your ",
              "metabolomics data with transcriptomics or proteomics data via ",
              "network methodologies. You can also view this tutorial on ",
              HTML(paste0(
                "<a href='https://github.com/travis-m-blimkie/MetaBridgeShiny/",
                "blob/master/tutorial/tutorial.md' target='_blank'>GitHub</a>."
              ))
            ),

            tags$ol(
              tags$li(tags$a(
                "Metabolite Preprocessing",
                href = "#metabolite-preprocessing"
              )),
              tags$li(tags$a(
                "MetaBridge Mapping",
                href = "#metabridge-mapping"
              )),
              tags$li(tags$a(
                "NetworkAnalyst",
                href = "#networkanalyst"
              ))
            )
          )
        ),

        div(
          class = "col-lg-10 tutorial",
          includeMarkdown("tutorial/tutorial.md")
        )
      ),

      ### About page
      tabPanel(
        value = "aboutPanel",
        title = "About",

        tags$div(
          class = "jumbotron",

          tags$h1("About"),

          tags$hr(),

          tags$div(
            class = "logoWrapper",

            tags$p(
              "MetaBridge was designed by Samuel Hinshaw and Travis Blimkie at the ",
              tags$a(href = "http://cmdr.ubc.ca/bobh/",
                     "Centre for Microbial Diseases and Immunity Research"),
              " at The University of British Columbia, and published in",
              tags$em("Bioinformatics"),
              " (doi: ",
              tags$a(
                href = "https://doi.org/10.1093/bioinformatics/bty331",
                "10.1093/bioinformatics/bty331",
                .noWS = "after"
              ),
              "). Please cite this paper when using MetaBridge in your analyses."
            ),

            tags$p(
              "For help, you can post an issue at the ",
              tags$a(href = "https://github.com/hancockinformatics/MetaBridgeShiny", "Github page."),
            ),

            tags$p("MetaBridge uses the following databases and R packages:"),

            tags$p(
              tags$dl(

                # MetaCyc
                tags$dt(
                  tags$a(href = "https://metacyc.org/", "MetaCyc v23"),
                  tags$dd("Curated database for human metabolomic data.")
                ),

                # KEGG
                tags$dt(
                  tags$a(href = "https://www.genome.jp/kegg/", "KEGG Release 92"),
                  tags$dd("Large database containing multiple data types.")
                ),

                # Shiny
                tags$dt(
                  tags$a(href = "https://shiny.rstudio.com/", "Shiny"),
                  tags$dd("Web application framework for R.")
                ),

                # ShinyCSSLoaders
                tags$dt(
                  tags$a(href = "https://github.com/andrewsali/shinycssloaders", "shinycssloaders"),
                  tags$dd("Animated loaders for shiny outputs.")
                ),

                # ShinyJS
                tags$dt(
                  tags$a(href = "https://deanattali.com/shinyjs/", "shinyjs"),
                  tags$dd("Improve the user experience of your Shiny apps in seconds.")
                ),

                # Tidyverse
                tags$dt(
                  tags$a(href = "https://www.tidyverse.org/", "tidyverse"),
                  tags$dd("A collection of R packages designed for data science.")
                ),

                # Pathview
                tags$dt(
                  tags$a(href = "https://doi.org/10.1093/bioinformatics/btt285", "Pathview"),
                  tags$dd("Pathway-based data integration and visualization.")
                )
              )
            )
          )
        )
      )
    )
  ),
  tags$script(src = "js/tippy.min.js"),
  tags$script(src = "js/client.js")
)


###########################################################################


# Define the server code --------------------------------------------------

server <- function(input, output, session) {

  # Wait for sessionInitialized to load packages. This does not have to be
  # defined in your UI, as the input will be passed via Shiny.onInputChange()
  observeEvent(input$sessionInitialized, {
    source("deferred.R")
    # After packages loaded, run button transform to signal ready states.
    runjs("handlers.initGetStarted();")
  }, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE)


  ################################################
  ##        Define reactive variables           ##
  ################################################

  # Reactive Values for Metabolite Data. These are isolated into individual
  # reactive values so we can depend on them for reactive changes.
  metaboliteObject <- reactiveVal()
  mappedMetabolites <- reactiveVal()
  mappingObject <- reactiveVal()
  mappingSummary <- reactiveValues(table = NULL, dbChosen = NULL)
  mappedMetaboliteTable <- reactiveVal()
  preSelectedIDType <- reactiveVal()
  databaseChosen <- reactiveVal()
  selectedMetab <- reactiveVal()
  idTypeChosen <- reactiveVal()
  columnPicked <- reactiveVal()
  hmdbCol <- reactiveVal()


  ################################################
  ##          Welcome Tab Handlers              ##
  ################################################

  # When clicking "Get Started", switch to `Upload` panel
  observeEvent(input$getStarted, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel")
  }, ignoreInit = TRUE)

  # When clicking "Tutorial", switch to `Tutorial` panel
  observeEvent(input$tutorial, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "tutorialPanel")
  }, ignoreInit = TRUE)

  # When clicking "About", switch to the `About` panel
  observeEvent(input$about, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "aboutPanel")
  }, ignoreInit = TRUE)


  ################################################
  ##          Upload Tab Handlers               ##
  ################################################

  # Inject example data frame when "Try Examples" is clicked
  observeEvent(input$tryExamples, {
    # Input examples...
    metaboliteObject(examples)
    # ...and wipe mapping objects
    mappingObject(NULL)
    mappedMetabolites(NULL)
    mappingObject(NULL)
    mappingSummary$table <- NULL
    mappingSummary$dbChosen <- NULL
    mappedMetaboliteTable(NULL)
    databaseChosen(NULL)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Read file when any of (fileInput, checkboxInput, radioButtons) states change.
  observeEvent({
    input$metaboliteUpload
    input$sep
    input$header
  }, {
    if (!is.null(input$metaboliteUpload)) {
      read_delim(
        file = input$metaboliteUpload$datapath,
        col_names = input$header,
        delim = input$sep
      ) %>% metaboliteObject() # Save to the reactiveVal...
      # ...then wipe mapping objects for a fresh start.
      mappingObject(NULL)
      mappedMetabolites(NULL)
      mappingObject(NULL)
      mappingSummary$table <- NULL
      mappingSummary$dbChosen <- NULL
      mappedMetaboliteTable(NULL)
      databaseChosen(NULL)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)


  # Once data is populated, render help text to the user,...
  output$uploadSuccess <- renderUI({
    input$tryExamples # ...making sure the "Try Examples" button is a dependency
    if (is.null(metaboliteObject())) {
      return(NULL)
    }
    tagList(
      tags$h4(
        class = "conditional-help",
        "Check below to see that your data has been uploaded properly.  ",
        "If so, click a column and ID type and proceed to the 'Map' tab!"
      ),
      tags$br()
    )
  })

  # Once the data is populated, render a preview of the data to the user.
  output$uploadedDataTable <- DT::renderDataTable({
    input$tryExamples
    input$sep
    input$header
    input$metaboliteUpload
    if (is.null(metaboliteObject())) {
      # Return `NULL` if nothing present so that we don't pass an error.
      return(NULL)
    } else {
      # Render the `uploadedDataTable()`.
      metaboliteObject()
    }
  },
  # DataTable options
  options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 15, 20),
    scrollX = "100%",
    # AMAZING! Crucial argument to make sure DT doesn't overflow vertical
    # scrolling options.
    scrollY = "456px",
    scrollCollapse = TRUE,
    paging = FALSE
  ),
  rownames = FALSE,
  selection = list(
    mode = "single", target = "column",
    selected = 0
  ),
  style = "bootstrap",
  class = "table-bordered table-responsive"
  )

  output$uploadedTablePanel <- renderUI({
    tags$div(
      class = "col-sm-9",
      uiOutput("uploadSuccess"),
      DT::dataTableOutput("uploadedDataTable")
    )
  })

  observeEvent({
    input$uploadedDataTable_columns_selected
    metaboliteObject()
  }, {
    # Wait 500ms after panel render and re-activate tooltips.
    runjs("setTimeout(() => { handlers.activateTooltips(['.panel-tooltip', '.btn-tooltip']); }, 100)")
  })

  # This has to be rendered separately from the column picker panel. Otherwise,
  # the entire column picker panel has to be re-rendered when the preselected ID
  # type gets updated, which resets the entire panel, which reverts to the
  # preselected column, effectively making it impossible to switch columns!
  output$idSelector <- renderUI({
    tags$div(

      tags$p(HTML(
        "Select the ID type you would like to use in the mapping. We recommend ",
        "using <b>HMDB</b> or <b>KEGG</b>, as these will yield the best results.",
        "Ensure the ID selected here matches the highlighted column."
      )),

      selectInput(
        inputId   = "idType",
        label     = "ID Type",
        width     = "50%",
        choices   = c("HMDB", "KEGG", "PubChem", "CAS", "MetaCyc Object ID" = "Compound"),
        selected  = preSelectedIDType(),
        selectize = FALSE
      ),
      # Include button to proceed

      actionButton(
        inputId = "continueToMap",
        label   = tags$b("Proceed"),
        class   = "btn-med btn-tooltip",
        style   = "color: #fff; background-color: #2c3e50; border-color: #2c3e50;",
        title   = "Proceed to mapping your metabolites"
        # `data-position` = "right"
      )
    )
  })

  # Render the UI for the column picker panel
  columnPickerUI <- eventReactive({
    # Change on button click (uploaded file or example data)...
    input$tryExamples
    input$metaboliteUpload
    # ...OR on header change.
    input$sep
    input$header
  }, {
    # Only render if NOT `NULL`.
    if (!is.null(metaboliteObject())) {
      tags$form(
        class = "well",
        # Dynamically render the `idType()` selector panel here (see below). This
        # is intentionally separate so that we do not have a feedback loop that
        # triggers re-rendering. Otherwise, as soon as you change this value,
        # the entire panel re-renders, switching it back to its default.
        uiOutput("idSelector")
      )
    }
  })

  observeEvent(input$uploadedDataTable_columns_selected, {
    # DataTables indexes by 0, so we add one...
    columnIndex <- input$uploadedDataTable_columns_selected + 1
    # ...then pick the column name!
    columnName <- colnames(metaboliteObject())[columnIndex]
    columnPicked(columnName)
  })

  # When data is populated, show column picker panel for users to select. This
  # is separate from the actual code to render so that we can only depend on
  # specific events triggering re-renders.
  output$columnPickerPanel <- renderUI({
    columnPickerUI()
  })

  # If the selected ID type is a column name in the data frame, preselect that
  # column for use in mapping.
  observeEvent(columnPicked(), {
    if (tolower(columnPicked()) %in% c("cas", "pubchem", "hmdb", "kegg")) {
      preSelectedIDType(columnPicked())
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Switch to `Map` panel when "Proceed" is clicked on the `Upload` tab
  observeEvent(input$continueToMap, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "mapPanel")
  }, ignoreInit = TRUE)


  ################################################
  ##                Map Tab Handlers            ##
  ################################################

  # Store ID type chosen as a reactive variable which only changes when the
  # "Map" button is clicked
  observeEvent(input$mapButton, {
    idTypeChosen(input$idType)
  })

  # Here's where the heavy lifting takes place! We now take the column the user
  # specified and map the IDs to genes!

  # When the map button is clicked, update the `dbChosen()`.
  observeEvent(input$mapButton, {
    databaseChosen(input$dbChosen)

    # Clear any pre-existing alerts
    removeUI(selector = "#mappingAlert")

    # Conduct the mapping with our `mapGenerally()` function defined in
    # "functions/mapGenerally.R".
    mappingOutput <- mapGenerally(
      importDF = metaboliteObject(),
      col = columnPicked(),
      db = databaseChosen(),
      idType = idTypeChosen()
    )

    # Assign just the mapped data to our reactive value...
    mappedMetabolites(mappingOutput$data)

    # ...and assign the full object (data plus status, errors, etc.) so we can
    # access the status reports later.
    mappingObject(mappingOutput)

    # Create new alert bubble with the status message.
    mappingAlert(
      status = mappingOutput$status,
      message = mappingOutput$message,
      suggest = mappingOutput$suggest
    )
  }, ignoreInit = TRUE)


  ############################# End ##############################


  # THREE STEP RENDER PROCESS, PART 1 - MAPPING SUMMARY TABLE
  # 1. Generate Table from `generateTables.R::generateSummaryTable()`, depending
  #    only on the mapButton click.
  # 2. Render the generated table with DT::renderDataTable(). This is separate
  #    from #1 because we need to assign the reactive table object to its own
  #    output object.
  # 3. Render the entire UI surrounding the table and insert the rendered DT.

  # STEP ONE
  # ~~~~~~~~~~
  # Show a summary table of the mapped metabolites (just number of genes, etc.)
  # This calls `generateSummaryTable()` from "functions/generateTables.R" and
  # only renders when the "Map" button is clicked
  observeEvent(input$mapButton, {
    results <- generateSummaryTable(
      mappingObject(),
      idTypeChosen(),
      databaseChosen()
    )
    mappingSummary$table <- results$table
    mappingSummary$dbChosen <- results$dbChosen
  })

  # STEP TWO
  # ~~~~~~~~~~
  # Once metabolites have been mapped, render the results.
  output$mappingSummaryTable <- DT::renderDataTable({
    mappingSummary$table %>% hyperlinkTable(databaseChosen())
  },
  rownames = FALSE,
  style = "bootstrap",
  class = "table-bordered table-responsive compact",
  escape = FALSE,
  selection = "single"
  )

  # STEP THREE
  # ~~~~~~~~~~
  # Render the panel separately so we have reactive control over all the UI
  # elements surrounding the table, not just the table itself.
  output$mappingSummaryPanel <- renderUI({
    # Make sure this depends on the summary table (and thus updates every time
    # the summary table does).
    mappingSummary$table
    # Now proceed
    if (is.null(mappingObject())) {
      return(NULL)
    } else if (mappingObject()$status == "error" | mappingObject()$status == "empty") {
      return(NULL)
      # Only render if we had non-null, non-error, non-empty results.
    } else {
      return(
        tagList(
          tags$h3(paste0("Mapping Summary - ", databaseChosen()), class = "tab-header"),
          # Insert the datatable here that we rendered above.
          DT::dataTableOutput("mappingSummaryTable")
        )
      )
    }
  })

  # When a new metabolite is selected, set it to the selected metabolite
  # reactive value! Why? So we can reset it given other certain conditions (see
  # the next function).
  observeEvent(input$mappingSummaryTable_rows_selected, {
    selectedMetab(input$mappingSummaryTable_rows_selected)
  })

  # But when the map button is selected, nullify any previously selected
  # metabolite.
  observeEvent(input$mapButton, {
    selectedMetab(NULL)
  })


  ############################# End ##############################


  # THREE STEP RENDER PROCESS, PART 2 - METABOLITE SPECIFIC TABLE
  # 1. Generate Table from `generateTables.R::generateSummaryTable()`, depending
  #    only on the mapButton click.
  # 2. Render the generated table with DT::renderDataTable(). This is separate
  #    from #1 because we need to assign the reactive table object to its own
  #    output Object.
  # 3. Render the entire UI surrounding the table and insert the rendered DT.

  # STEP ONE
  # Generate table
  # ~~~~~~~~~~
  # Now, show the filtered (unsummarized) table, based on what metabolite user
  # clicked on.
  observeEvent({
    # Update when we select a new metabolite in the summary table...
    selectedMetab()
    # ...or when we click the map button (this is important because we need to
    # be able to update in case there are errors we need to display).
    input$mapButton
  }, {
    # Pull the `$data` object from the `tryCatch()` output if there was an
    # error. This should default to the previous successful step.
    if (mappingObject()$status == "error" | mappingObject()$status == "empty") {
      mappingObject()$data %>% mappedMetaboliteTable()

      # Otherwise, generate our table depending on the chosen database! As with
      # `generateSummaryTable()`, these functions come from "generateTables.R"

    } else if (databaseChosen() == "KEGG") {
      if (mappingSummary$dbChosen != "KEGG") {
        cat("DATABASE WAS NOT KEGG, NULL RETURNING...")
        # If our summary table was somehow not updated yet, exit.
        return(NULL)
      } else {
        generateKEGGMetabTable(
          mappingObject(),
          mappingSummary$table,
          selectedMetab(),
          idTypeChosen()
        ) %>% mappedMetaboliteTable()
        # Otherwise proceed with generated the metabolite table.
      }

    } else if (databaseChosen() == "MetaCyc") {
      # If our summary table was somehow not updated yet, exit.
      if (mappingSummary$dbChosen != "MetaCyc") {
        cat("DATABASE WAS NOT METACYC, NULL RETURNING...")
        return(NULL)
        # Otherwise proceed with generated the metabolite table.
      } else {
        generateMetaCycMetabTable(
          mappingObject(),
          mappingSummary$table,
          selectedMetab(),
          idTypeChosen()
        ) %>% mappedMetaboliteTable()
      }
    }
  })

  # STEP TWO
  # Render generated table
  # ~~~~~~~~~~
  # Once metabolites have been mapped, render the results!
  output$mappedMetaboliteTable <- DT::renderDataTable({
    if (is.null(mappingObject()) | is.null(selectedMetab())) {
      return(data.frame())

    } else if (mappingObject()$status == "success") {
      # Only render if we had non-null, non-error, non-empty results.
      mappedMetaboliteTable() %>% hyperlinkTable(databaseChosen())
    }
  },
  rownames = FALSE,
  style = "bootstrap",
  class = "table-bordered table-responsive compact",
  escape = FALSE,
  selection = "single"
  )

  # STEP THREE
  # Render entire UI output, including the rendered table.
  # ~~~~~~~~~~
  output$fullMappingResultsPanel <- renderUI({
    tags$div(
      if (is.null(mappingObject())) {
        return(NULL)
        # If we had an error, change the header to reflect that these are
        # intermediate results.

      } else if (
        mappingObject()$status == "error" | mappingObject()$status == "empty"
      ) {
        tags$h3("Intermediate Results")
        # Only render if we had non-null, non-error, non-empty results.

      } else {
        tagList(
          tags$hr(),
          tags$h3("Per-Metabolite Mapping Results")
        )
      },
      # Rendered table from STEP TWO goes here!
      DT::dataTableOutput("mappedMetaboliteTable")
    )
  })


  ############################# End ##############################


  # Watch for the "Try Again" button that will be rendered if an error occurs in
  # the mapping.
  observeEvent(input$remap, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "uploadPanel")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Once table exists, render the save panel...
  output$saveMappingPanel <- renderUI({
    if (!is.null(mappedMetabolites())) {

      tags$form(
        class = "well",
        tags$p("Download your full mapping results below."),
        radioButtons(
          inputId = "saveType",
          label   = "Save results as:",
          choices = c("Comma-Delimited" = "csv",
                      "Tab-Delimited"   = "tsv"),
          selected = "csv"
        ),

        # ...with a tooltip.
        downloadButton(
          "downloadMappingData",
          tags$b("Download"),
          style = "color: #fff; background-color: #3498db; border-color: #3498db",
          class = "btn-med btn-tooltip btn-right",
          title = "Download your full mapping results",
        ),

        # These breaks are only needed because we right-align the download
        # button. Without them, the button will not stay within the bounds of
        # the form/well UI object.
        tags$br(),
        tags$br()
      )
    }
  })


  # Navigate to the "Visualize" page when KEGG was the chosen database.
  output$continueToViz <- renderUI({
    # Do not render panel if no database has been mapped against yet, because
    # `databaseChosen()` does not get `input$dbChosen` until the "Map" button
    # is clicked.
    if (is.null(databaseChosen())) {
      return(NULL)

      # Once mapped, render the panel.
    } else if (databaseChosen() == "MetaCyc") {
      return(NULL)
    } else {
      tags$form(
        class = "well",
        tags$label("Visualize Results"),
        tags$p(
          "If you mapped against KEGG, you have the option",
          "to visualize your results with pathview."
        ),
        br(),

        # If we mapped against KEGG, show "Visualize" button.
        if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
          actionButton(
            inputId = "visualizeButton",
            label = tags$b("Visualize"),
            class = "btn btn-med btn-tooltip",
            style = "color: #fff; background-color: #2c3e50; border-color: #2c3e50;",
            title = "Visualize your results with pathview"
          )
        # But if we mapped against MetaCyc disable the "Visualize" button.
        } else {
          actionButton(
            inputId = "visualizeButton",
            label = tags$b("Visualize"),
            class = "btn btn-med btn-tooltip disabled",
            title = "Select a metabolite from the summary table"
          )
        }
      )
    }
  })

  # Client-side JS to enable/disable "Visualize" tab! Also disables the
  # "Visualize" tab in the navbar when visualization is not possible. Make sure
  # that we have a tooltip explaining why the "Visualization" tab is disabled. A
  # lot of this refers to code in `www/js/client.js`.
  observeEvent(input$mapButton, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").removeClass('panel-tooltip');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").addClass('panel-tooltip');")
    }
  })

  # Client-side JS to enable/disable "Visualization" tab!
  observeEvent(input$mappingSummaryTable_rows_selected, {
    if (databaseChosen() == "KEGG" & !is.null(selectedMetab())) {
      runjs("$(\"a[data-value='vizPanel']\").parent().removeClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").removeClass('panel-tooltip');")
    } else {
      runjs("$(\"a[data-value='vizPanel']\").parent().addClass('disabled');")
      runjs("$(\"a[data-value='vizPanel']\").addClass('panel-tooltip');")
    }
  })

  # When clicking "Visualize", switch to the "Visualize" panel.
  observeEvent(input$visualizeButton, {
    updateNavbarPage(session, inputId = "navbarLayout", selected = "vizPanel")
  }, ignoreInit = TRUE)


  # Cleaning the mapped MetaCyc (not KEGG) data before download, to remove HTML
  # tags from reactions. Specifically, we are removing any HTML tags, using
  # plain text arrows, and switching Greek letters to English versions.
  cleanMappedMetabolites <- reactive({
    req(mappedMetabolites())

    if (databaseChosen() == "MetaCyc") {
      mappedMetabolites() %>% cleanReactions(.)
    } else {
      mappedMetabolites()
    }
  })

  # Export the data.
  output$downloadMappingData <- downloadHandler(
    # Name file format: `originalFilename_mapped_dbChosen.savetype`.
    filename = function() {
      paste0(
        ifelse(
          test = is.null(input$metaboliteUpload),
          yes = "example_dataset",
          no = tools::file_path_sans_ext(input$metaboliteUpload$name)
        ),
        "_mapped_",
        databaseChosen(),
        ".",
        input$saveType
      )
    },
    content = function(file) {
      write_delim(
        cleanMappedMetabolites(),
        file,
        delim = switch(
          input$saveType,
          "csv" = ",",
          "tsv" = "\t"
        )
      )
    }
  )


  ################################################
  ##              Viz Tab Handlers              ##
  ################################################

  # Set up reactive values for:
  # - The selected compound of the clicked row
  # - The pathways that compound is involved in
  # - The genes (for the enzymes) that compound interacts with
  selectedRowAttrs <- reactiveValues(
    "selectedCompound" = NULL,
    "selectedCompoundName" = NULL,
    "pathwaysOfSelectedCompound" = NULL,
    "genesOfSelectedCompound" = NULL
  )

  # Now, when the selected row changes...
  observeEvent(input$mappingSummaryTable_rows_selected, {

    # ...map!
    pathwayMappingAttrs <- generalPathwayMapping(
      summaryTable = mappingSummary$table,

      # The fullTable provided used to be the table that was rendered just for
      # the selected metabolites. This means that the only genes were those
      # identified for the selected metabolites. For now, I have fixed this by
      # including all genes in the mapping. HOWEVER, in the future it could be
      # interesting to create a toggle that would let the user specify which
      # they would prefer. In that case, we would have to make sure that
      # mappedMetaboliteTable() was updated before the pathway mapping function
      # was called.

      # fullTable = mappedMetaboliteTable(),
      fullTable = mappingObject()$data,
      idType = idTypeChosen(),
      db = databaseChosen(),
      selectedRow = selectedMetab()
    )

    # Assign results to their reactive values
    selectedRowAttrs$selectedCompound <-
      pathwayMappingAttrs$selectedCompound

    selectedRowAttrs$selectedCompoundName <-
      pathwayMappingAttrs$selectedCompoundName

    selectedRowAttrs$genesOfSelectedCompound <-
      pathwayMappingAttrs$genesOfSelectedCompound

    selectedRowAttrs$pathwaysOfSelectedCompound <-
      pathwayMappingAttrs$pathwaysOfSelectedCompound
  })

  # Render the pathway panel once.
  output$pathwayPanel <- renderUI({
    # Check for results before rendering!
    if (nrow(selectedRowAttrs$pathwaysOfSelectedCompound) == 0) {
      tags$div(
        tags$h4(
          paste0(
            "Pathways for ",
            selectedRowAttrs$selectedCompoundName
          )
        ),
        tags$p("No pathways found for this compound.")
      )
    } else if (databaseChosen() == "KEGG") {
      tags$div(
        tags$h4(paste0(
          "Pathways for ",
          stringr::str_to_title(selectedRowAttrs$selectedCompoundName)
        )),
        selectInput(
          inputId = "pathwaysPicked",
          label = "",
          choices = selectedRowAttrs$pathwaysOfSelectedCompound$namedPway,
          selectize = FALSE
        ),
        tags$p("Each pathway may take some time to process."),
        tags$p(
          "For each pathway, only the compound selected ",
          "is shown, but ALL mapped genes are shown."
        )
      )
    } else if (databaseChosen() == "MetaCyc") {
      tags$div(
        tags$h4(paste0(
          "Pathways for ",
          selectedRowAttrs$selectedCompoundName
        )),
        selectInput(
          inputId = "pathwaysPicked",
          label = "",
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
          src = "./logo_background.svg",
          contentType = "image/svg",
          width = 512,
          height = 512,
          alt = "pathway placeholder"
        )
      })
    }

    # Setup named variables for standard evaluation.
    pathwayNameIDcol <- as.name("namedPway")
    selectedPathway <- quo(input$pathwaysPicked)

    # Pull the pathway ID from the pathway name selected by the user.
    selectedPathwayID <-
      selectedRowAttrs$pathwaysOfSelectedCompound %>%
      filter(!!(pathwayNameIDcol) == input$pathwaysPicked) %>%
      extract2("id")

    filename <- visualizePathview(
      pathway = selectedPathwayID,
      genes = selectedRowAttrs$genesOfSelectedCompound,
      cpd = selectedRowAttrs$selectedCompound
    )

    # Return a list containing the filename. Render image at 1000px and then
    # constrain image to `div` in CSS.
    return(list(
      src = filename,
      contentType = "image/png",
      width = 1000,
      # height = imageHeight(),
      alt = paste0("Pathway map of KEGG Pathway ", input$pathwaysPicked)
    ))
  }, deleteFile = TRUE)


  # Render entire UI for `vizPanel`.
  output$vizPanelUI <- renderUI({
    if (is.null(databaseChosen())) {
      tags$div(
        tags$h2("Pathway View", class = "tab-header"),
        # "Nothing to map" alert
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
    } else if (databaseChosen() == "MetaCyc") {
      tags$div(
        tags$h2("Pathway View", class = "tab-header"),
        # "Nothing to map" alert
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
    } else if (is.null(selectedMetab())) {
      tags$div(
        tags$h2("Pathway View", class = "tab-header"),
        # "Nothing to map" alert
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
        tags$div(
          class = "col-sm-3 manual-sidebar",
          # Allow user to pick which pathway that the selected metabolite
          # participates in to view.
          tags$form(
            class = "well",
            uiOutput("pathwayPanel")
          )
        ),

        # Pathway visualization
        tags$div(
          class = "col-sm-9",
          tags$h2("Pathway View", class = "tab-header"),
          imageOutput("pathwayView") %>% withSpinner(type = 8, color = "#303E4E")
        )
      )
    }
  })
}


# Finally, run the app! ---------------------------------------------------

shinyApp(ui, server)
