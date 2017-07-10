
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
    # tags$link(type = "text/javascript", href = "user.js")
  ),
  navbarPage(
    "MetaBridge",
    collapsible = TRUE,
    tabPanel("Upload", 
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
               tableOutput('uploadedDataTable')
             )
             ),
    tabPanel("Map", 
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
        )
      ),
      tags$div(
        class = "col-sm-8",
        tableOutput('mappedMetaboliteTable')
      )
    ), 
    tabPanel("Output",
             tableOutput('databases')),
    tabPanel("NetworkAnalyst"), 
    tabPanel("About", class = "navbar-right"), 
    tabPanel("Help", class = "navbar-right")
  )
))
