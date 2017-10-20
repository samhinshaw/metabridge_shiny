

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(fluidPage(
  # Head linking to Flatly bootstrap theme & my personal tweaks
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "user.css")
  ),
  # Body
  navbarPage(
    # Navbar Brand
    title = HTML("MetaBridge <sup class='tiny'>BETA</sup>"),
    id = "navbarLayout",
    # Make sure we use ShinyJS
    header = tagList(useShinyjs()),
    windowTitle = "MetaBridge",
    collapsible = TRUE,
    # begin the tabPanel Layout!
    tabPanel(
      # Our welcome tab
      "Welcome",
      value = "welcomePanel",
      # Dev mode alert
      # tags$div(
      #   class = "alert alert-dismissible alert-danger",
      #   tags$button(
      #     HTML("&times;"),
      #     type = "button",
      #     class = "close",
      #     `data-dismiss` = "alert"
      #   ),
      #   "Warning: In development environment!"
      # ),
      # Welcome hero
      tags$div(
        class = "jumbotron",
        h1("Welcome"),
        br(),
        tags$p(
          "Welcome to MetaBridge, a web tool for network-based integrative ",
          "analysis of metabolomics data. Here you can upload a set of metabolites ",
          "and identify the directly interacting enzymes for network integration. "
        ),
        tags$p(
          "To start, you'll want a set of metabolites as",
          "HMDB, KEGG, PubChem, or CAS IDs. We recommend ",
          tags$a("MetaboAnalyst", href = "http://www.metaboanalyst.ca"),
          " for metabolomics data processing and ID conversion. "
        ),
        br(),
        actionButton(
          inputId = "getStarted", 
          label = "Loading R Packages...",
          class = "btn-primary btn-lg disabled", # css-tooltip
          title = "Let's Go!",
          icon("circle-o-notch", class = "fa fa-spin", lib = "font-awesome")
        )
      )
    ),
    # Upload panel!
    tabPanel(
      "Upload",
      value = "uploadPanel",
      # Sidebar
      tags$div(
        class = "col-sm-3 manual-sidebar",
        # Separate form 'wells' within the sidebar
        tags$form(
          class = "well",
          tags$p(
            "Upload a plain-text spreadsheet (CSV or TSV) containing ",
            "your metabolites of interest in a single column, or try ",
            "out our example dataset."
          ),
          # Upload handling
          fileInput(
            inputId = 'metaboliteUpload',
            label = 'Upload Metabolites',
            # width = '50%',
            accept = c(
              'text/csv',
              'text/comma-separated-values,text/plain',
              '.csv',
              'text/tab-separated-values'
            )
          ),
          # Header in file?
          checkboxInput(
            inputId = 'header',
            label = 'Header',
            value = TRUE
          ),
          # TSV or CSV?
          radioButtons(
            inputId = 'sep',
            label = 'Separator',
            choices = c(
              Comma = ',',
              Tab = '\t',
              Semicolon = ';'
            ),
            selected = ','
          ),
          # OR, try our examples!
          actionLink(
            inputId = "tryExamples",
            class = "btn btn-link btn-med css-tooltip",
            label = "Try Examples",
            title = "Try an example dataset from MetaboAnalyst", 
            # `data-toggle` = "tooltip",
            # `data-placement` = "right",
            # `data-original-title` = "Try an example dataset from MetaboAnalyst"
          )
        ),
        # Show the columns of the uploaded file
        uiOutput('columnPickerPanel')
      ),
      # Display the file that was uploaded
      uiOutput('uploadedTablePanel')
    ),
    ## Mapping Panel
    tabPanel(
      title = "Map",
      value = "mapPanel",
      # h2("Map Metabolites to Interacting Enzymes"),
      # Manual Sidebar
      tags$div(
        class = "col-sm-3 manual-sidebar",
        id = 'mapPanelSidebar',
        tags$form(
          class = "well",
          tags$p(
            "Choose a database to map with. MetaCyc has higher quality annotations, ",
            "but KEGG may yield more hits. If you map via KEGG, you also have the ",
            "option to visualize your results."
          ),
          ## For now just allow one database. Later we can allow multiple mappings in one go
          radioButtons(
            "dbChosen",
            "Choose Database",
            choices = c("MetaCyc", "KEGG"),
            selected = "MetaCyc"
          ),
          # Map!
          actionButton(
            "mapButton",
            "Map",
            class = "btn-med css-tooltip",
            title = "Map your metabolites to the selected database"
            # `data-toggle` = "tooltip",
            # `data-placement` = "right",
            # `data-original-title` = "Map your metabolites against the selected database"
          )
          # Maybe show tickbox to allow user to see full results rather than just the summary?
        ),
        # Let user download results
        uiOutput('saveMappingPanel'),
        # Show panel for continuing to visualize results
        uiOutput('continueToViz')
      ),
      ## DISPLAY MAPPING RESULTS
      tags$div(
        class = "col-sm-9",
        # Show summary table (server-rendered)
        uiOutput('mappingSummaryPanel'),
        # Show FULL results for a selected metabolite (server-rendered)
        uiOutput('fullMappingResultsPanel')
      )
    ),
    # Visualize the results!
    tabPanel(
      title = "Pathview",
      value = "vizPanel",
      id = "visualizationPanel",
      uiOutput('vizPanelUI')
    ),
    # Finally, the 'More' Panel, with about, help, etcetera
    navbarMenu(
      "More",
      # "Info",
      tabPanel(
        "About",
        tags$div(
          class = "jumbotron",
          tags$h1("About"),
          "MetaBridge was designed by Samuel Hinshaw at the Centre for Microbial Diseases and Immunity Research at The University of British Columbia",
          tags$h2("Sources"),
          tags$ul(
            tags$li(
              "Luo, W. and Brouwer C., Pathview: an R/Bioconductor package for pathway-based data integration and visualization. Bioinformatics, 2013, 29(14): 1830-1831, doi: 10.1093/bioinformatics/btt285"
            ),
            tags$li(
              "Hadley Wickham (NA). tidyverse: Easily Install and Load the 'Tidyverse'. http://tidyverse.tidyverse.org, https://github.com/tidyverse/tidyverse."
            ),
            tags$li(
              "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (NA). shiny: Web Application Framework for R. R package version 1.0.3.9001. http://shiny.rstudio.com"
            ),
            tags$li(
              "Yihui Xie (2016). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.2.12. http://rstudio.github.io/DT"
            ),
            tags$li(
              "Dean Attali (2017). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 0.9.1. https://CRAN.R-project.org/package=shinyjs"
            ),
            tags$li(
              "Stefan Milton Bache and Hadley Wickham (2014). magrittr: A Forward-Pipe Operator for R. R package version 1.5. https://CRAN.R-project.org/package=magrittr"
            ),
            tags$li(
              "Hadley Wickham, Jim Hester and Romain Francois (2017). readr: Read Rectangular Text Data. R package version 1.1.1. https://CRAN.R-project.org/package=readr"
            )
          )
        )
      ),
      # "----",
      # "Section header",
      tabPanel("Help",
               tags$div(
                 class = "jumbotron",
                 h1("Help"),
                 tags$p("For assistance, you can reach me on twitter, @samhinshaw."),
                 HTML(
                   '<a href="https://twitter.com/intent/tweet?screen_name=samhinshaw" \
                   class="twitter-mention-button" data-show-count="false">
                   Tweet to @samhinshaw</a>
                   <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>'
                 )
                 ))
                 )
                 ),
  tags$script(src = "client.js"),
  HTML(
    "<!-- Global Site Tag (gtag.js) - Google Analytics -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=UA-106038065-1'></script>
    <script>
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments)};
    gtag('js', new Date());
    
    gtag('config', 'UA-106038065-1');
    </script>"
  )
  ))
