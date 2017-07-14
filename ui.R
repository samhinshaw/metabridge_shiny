
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "user.css")
  ),
  navbarPage(
    title = "MetaBridge", id = "navbarLayout",
    header = tagList(
      useShinyjs()
    ),
    windowTitle = "MetaBridge",
    collapsible = TRUE,
    tabPanel("Welcome", value = "welcomePanel",
             tags$div(
               class = "jumbotron",
               h1("Welcome"),
               tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
               actionButton("getStarted", "Get Started", 
                            class = "btn btn-primary btn-lg")
             )
    ),
    tabPanel("Upload", value = "uploadPanel",
             # h2("Upload Metabolites"),
             tags$div(class = "col-sm-3 manual-sidebar",
                      tags$form(class = "well",
                                fileInput(inputId = 'metaboliteUpload', label = 'Upload Metabolites',
                                          # width = '50%', 
                                          accept = c('text/csv', 
                                                     'text/comma-separated-values,text/plain', 
                                                     '.csv', 
                                                     'text/tab-separated-values')),
                                checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                                radioButtons(inputId = 'sep', label = 'Separator',
                                             choices = c(Comma = ',', Tab = '\t', Semicolon = ';'),
                                             selected = ','),
                                actionLink(inputId = "tryExamples",
                                           class = "btn btn-link",
                                           label = "Try Examples")
                      ),
                      uiOutput('columnPickerPanel')
             ),
             tags$div(class = "col-sm-9",
                      uiOutput('uploadSuccess'),
                      dataTableOutput('uploadedDataTable')
             )
    ),
    tabPanel("Map", value = "mapPanel",
             # h2("Map Metabolites to Interacting Enzymes"),
             # Manual Sidebar
             tags$div(
               class = "col-sm-2 manual-sidebar",
               tags$form(
                 class = "well",
                 ## For now just allow one database. Later we can allow multiple
                 radioButtons("dbChosen", "Choose Database", 
                              choices = c("MetaCyc", "KEGG"), 
                              selected = "MetaCyc"), 
                 actionButton("mapButton", "Map")
               ),
               uiOutput('saveMappingPanel')
             ),
             tags$div(
               class = "col-sm-10",
               DT::dataTableOutput('mappedMetaboliteTable'), 
               textOutput('horizontalScrollMessage')
             )
    ),
    tabPanel("Visualize"),
    # tabPanel("NetworkAnalyst"),
    # Simple alternative to 'float: right'
    navbarMenu("More",
               # "Info",
               tabPanel(
                 "About", 
                 tags$div(
                   class = "jumbotron",
                   h1("About"),
                   "MetaBridge was designed by Samuel Hinshaw at the Centre for 
                   Microbial Diseases and Immunity Research at The University of British Columbia" 
                 )
                 ),
               # "----",
               # "Section header",
               tabPanel(
                 "Help", 
                 tags$div(
                   class = "jumbotron",
                   h1("Help"),
                   "For assistance, you can reach me on twitter, @samhinshaw.",
                   br(),br(),
                   HTML('<a href="https://twitter.com/intent/tweet?screen_name=samhinshaw" \
                        class="twitter-mention-button" data-show-count="false">
                        Tweet to @samhinshaw</a>
                        <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>')
                   )
                   )
                   )
                 )
  # tags$script(src = "user.js")
))
