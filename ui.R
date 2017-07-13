
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "user.css")
  ),
  navbarPage(
    "MetaBridge", id = "navbarLayout",
    collapsible = TRUE,
    tabPanel("Upload", value = "uploadPanel",
             # h2("Upload Metabolites"),
             tags$div(class = "col-sm-4 manual-sidebar",
                      tags$form(class = "well",
                        fileInput(inputId = 'metaboliteUpload', label = 'Upload Metabolites',
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
             tags$div(class = "col-sm-8",
               textOutput('uploadSuccess'),
               dataTableOutput('uploadedDataTable')
             )
             ),
    tabPanel("Map", value = "mapPanel",
      # h2("Map Metabolites to Interacting Enzymes"),
      # Manual Sidebar
      tags$div(
        class = "col-sm-4 manual-sidebar",
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
        class = "col-sm-8",
        DT::dataTableOutput('mappedMetaboliteTable'), 
        textOutput('horizontalScrollMessage')
      )
    ),
    tabPanel("Visualize"),
    # tabPanel("NetworkAnalyst"),
    
    # Simple alternative to 'float: right'
    navbarMenu("More",
      tabPanel(
        "About", 
        tags$div(
          class = "jumbotron",
          h1("About"),
          "MetaBridge was designed by Samuel Hinshaw at the Centre for Microbial Diseases and Immunity Research at The University of British Columbia" 
        )
      ),
      tabPanel(
        "Help", 
        tags$div(
          class = "jumbotron",
          h1("Help"),
          "For assistance, you can reach me on twitter, @samhinshaw.",
          # HTML('<a href="https://twitter.com/intent/tweet?screen_name=samhinshaw" class="twitter-mention-button" data-show-count="false">Tweet to @samhinshaw</a><script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>'),
          br(),br(),
          tags$button(
            class = "btn btn-info btn-lg",
            "Tweet"
          )
        )
      )
    )
  )
  # tags$script(src = "user.js")
))
