
# Welcome to the MetaBridge Shiny app! This is the UI code defining how
# everything looks. Note that many elements will be missing UI code here, as
# they are generated in calls to renderUI() in `server.R` in order to allow for
# more control over when/how elements are rendered.


shinyUI(fluidPage(
  # Head linking to Flatly bootstrap theme and my personal tweaks
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
  # Body
  navbarPage(
    # Navbar Brand
    title = HTML("<img src ='/logo_white.svg' alt='M' height='28'"), #  MetaBridge <sup class='tiny'>BETA</sup>
    id = "navbarLayout",
    # Make sure we use ShinyJS
    header = tagList(useShinyjs()),
    windowTitle = "MetaBridge",
    collapsible = TRUE,
    # Begin the tabPanel Layout!
    tabPanel(
      # Our welcome tab
      title = "MetaBridge",
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
        id = "welcomeHero",
        class = "jumbotron",
        h1("Welcome"),
        # br(),
        tags$div(
          class = "logoWrapper",
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
          tags$p(
            "With the output of MetaBridge, you can create a ",
            "protein-protein interaction network representative ",
            "of your metabolomics data. We recommend ",
            tags$a("NetworkAnalyst", href = "http://www.networkanalyst.ca"),
            "for generation of these networks and for network-based integration ",
            "with protein-protein interaction networks created from other ",
            "omics types."
          ),
          br(),
          div(

            # To see how these buttons are hidden, refer to `www/js/client.js`
            actionButton(
              inputId = "getStarted",
              label = "Initializing App...",
              class = "btn-primary btn-lg disabled", # btn-tooltip
              `data-position` = "bottom",
              icon("circle-o-notch", class = "fa fa-spin", lib = "font-awesome")
            ),

            HTML("&nbsp;&nbsp;&nbsp;"),

            actionButton(
              inputId = "tutorial",
              label = "Tutorial",
              class = "btn-success btn-lg btn-tooltip btn-hidden", # btn-tooltip
              `data-position` = "bottom",
              style = "width: 155px",
              title = "Learn how to use MetaBridge for integrative analysis."
            ),

            HTML("&nbsp;&nbsp;&nbsp;"),

            actionButton(
              inputId = "about",
              label = "About",
              class = "btn-primary btn-lg btn-tooltip btn-hidden", # btn-tooltip
              style = "color: #2c3e50; background-color: #fff; border-color: #2c3e50; width: 155px",
              `data-position` = "bottom",
              title = "Visit the About page."
            )
          )
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
            inputId = "metaboliteUpload",
            label = "Upload Metabolites",
            # width = '50%',
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
            label = "Header",
            value = TRUE
          ),
          # TSV or CSV?
          radioButtons(
            inputId = "sep",
            label = "Separator",
            choices = c(
              Comma = ",",
              Tab = "\t",
              Semicolon = ";"
            ),
            selected = ","
          ),
          # OR, try our examples!
          actionLink(
            inputId = "tryExamples",
            class = "btn btn-link btn-med btn-tooltip",
            `data-position` = "right",
            label = "Try Examples",
            title = "Try an example dataset from MetaboAnalyst"
          )
        ),
        # Show the columns of the uploaded file
        uiOutput("columnPickerPanel")
      ),
      # Display the file that was uploaded
      uiOutput("uploadedTablePanel")
    ),

    # Mapping Panel
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
            "Choose a database to map with. MetaCyc has higher quality annotations, ",
            "but KEGG may yield more hits. If you map via KEGG, you also have the ",
            "option to visualize your results."
          ),
          # For now just allow one database. Later we can allow multiple
          # mappings at once.
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
            class = "btn-med btn-tooltip",
            `data-position` = "right",
            title = "Map your metabolites to the selected database"
          )
        # Maybe show tickbox to allow user to see full results rather than just
        # the summary? Currently not implemented, see `server.R:631`
        ),
        # Let the user download their results
        uiOutput("saveMappingPanel"),
        # Show panel for continuing to visualization results
        uiOutput("continueToViz")
      ),

      # DISPLAY MAPPING RESULTS
      tags$div(
        class = "col-sm-9",
        # Show summary table (server-rendered)
        uiOutput("mappingSummaryPanel"),
        # Show FULL results for a selected metabolite (server-rendered)
        uiOutput("fullMappingResultsPanel")
      )
    ),
    # Visualize the results!
    tabPanel(
      title = "Pathview",
      value = "vizPanel",
      id = "visualizationPanel",
      class = "viz-panel",
      uiOutput("vizPanelUI")
    ),
    # Finally, the 'More' Panel, with about, help, etc.
    navbarMenu(
      "Help",
      tabPanel(
        title = "Tutorial",
        value = "tutorialPanel",
        tags$div(
          class = "jumbotron",
          tags$h1("Tutorial"),
          tags$div(
            class = "logoWrapper",
            tags$h2("Network-Based Integrative Analysis with MetaBridge"),
            tags$p(
              "Below you will find a sample workflow for integrating your ",
              "metabolomics data with transcriptomics or proteomics data via ",
              "network methodologies. You can also view this tutorial on ",
              HTML(paste0(
                "<a href='https://github.com/samhinshaw/metabridge_shiny/blob/master/tutorial/tutorial.md' ",
                "target='_blank'>GitHub</a>."
                ))
            ),
            tags$ol(
              tags$li(
                tags$a("Metabolite Preprocessing", href = "#metabolite-preprocessing")
              ),
              tags$li(
                tags$a("MetaBridge Mapping", href = "#metabridge-mapping")
              ),
              tags$li(
                tags$a("NetworkAnalyst", href = "#networkanalyst")
              )
            )
          )
        ),
        div(
          class = "col-lg-10 tutorial",
          # class = 'tutorial',
          includeMarkdown("tutorial/tutorial.md")
        )
      ),
      tabPanel(
        value = "aboutPanel",
        title = "About",
        tags$div(
          class = "jumbotron",
          tags$h1("About"),
          tags$div(
            class = "logoWrapper",
            tags$p(
              "MetaBridge was designed by Samuel Hinshaw and Travis Blimkie at the ",
              tags$a(href = "http://cmdr.ubc.ca/bobh/", "Centre for Microbial Diseases and Immunity Research"),
              " at The University of British Columbia.",
              "MetaBridge was published in",
              tags$em("Bioinformatics"),
              " (doi: ",
              tags$a(href = "https://doi.org/10.1093/bioinformatics/bty331", "10.1093/bioinformatics/bty331"),
              "). Please cite this paper when using MetaBridge in your analyses. ",
              "A protocol was also published in ", tags$em("Current Protocols in Bioinformatics"),
              " (doi: ", tags$a(href = "", "doi_number"), ")."
            ),

            tags$p(
              "For help, you can post an issue at the ",
              tags$a(href = "https://github.com/travis-m-blimkie/metabridge_shiny", "Github page.")
            ),
            tags$p(
              "MetaBridge uses the following databases and R packages:"
            ),
            tags$p(
              tags$dl(

                # MetaCyc
                tags$dt(
                  tags$a(href = "https://metacyc.org/", "MetaCyc v23")
                ),
                tags$dd("Curated database for human metabolomic data"),

                # KEGG
                tags$dt(
                  tags$a(href = "https://www.genome.jp/kegg/", "KEGG Release 92")
                ),
                tags$dd("Large database containing multiple data types"),

                # Pathview
                tags$dt(
                  tags$a(href = "https://doi.org/10.1093/bioinformatics/btt285", "Pathview")
                ),
                tags$dd("Pathway-based data integration and visualization"),

                # Shiny
                tags$dt(
                  tags$a(href = "https://shiny.rstudio.com/", "Shiny")
                  ),
                tags$dd("Web Application Framework for R"),

                # ShinyCSSLoaders
                tags$dt(
                  tags$a(href = "https://github.com/andrewsali/shinycssloaders", "shinycssloaders")
                ),
                tags$dd("Animated loaders for shiny outputs"),

                # ShinyJS
                tags$dt(
                  tags$a(href = "https://deanattali.com/shinyjs/", "shinyjs")
                ),
                tags$dd("Easily Improve the User Experience of Your Shiny Apps in Seconds"),

                # Tidyverse
                tags$dt(
                  tags$a(href = "https://www.tidyverse.org/", "The Tidyverse"),
                  tags$dd("An opinionated collection of R packages designed for data science. ")
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
))
