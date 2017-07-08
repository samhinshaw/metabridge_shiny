
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
    "MetaBridge",
    tabPanel("Import", 
             sidebarLayout(
               sidebarPanel(
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
               mainPanel(
                 tableOutput('contents'),
                 verbatimTextOutput('diagnostics')
               )
             )),
    tabPanel("Plot", 
      # Application title
      # h2("Old Faithful Geyser Data"),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30), 
          submitButton(text = "Submit", 
                       icon = NULL, 
                       width = NULL)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
      )
    ), 
    tabPanel("Output",
             tableOutput('databases')),
    tabPanel("NetworkAnalyst")
  )
))
