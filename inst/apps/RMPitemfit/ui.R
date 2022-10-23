pageWithSidebar(
  headerPanel('Item Statistics for polytomous items'),
  sidebarPanel(
    helpText("Parameter etimation method for:"),
    fluidRow(
      column(4,
             radioButtons(inputId = "method",
                          label = "Items",
                          choices = list(#"Simulation" = "sim",
                            "PCML" = "PCML",
                            "JML" = "JML",
                            "CML" = "CML",
                            "MML" = "MML"))
      ),
      column(2,
             radioButtons(inputId = "methodpp",
                          label = "Persons",
                          choices = list(#"Simulation" = "sim",
                            "WML" = "WML",
                            "MLE" = "MLE"))
      )
    ),
    tags$head(tags$style(".progress-bar{background-color:#8B8682;}",
                         type = "text/css", "a{color: #8B8682;}",
                         HTML('#go{background-color:#8B8682}'))),
    # Input: Select a file ----
    fileInput('beta', 'Choose CSV File with Item Parameters',
              accept=c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')),
    # Input: Checkbox if file has header ----
    checkboxInput("headerBeta", "Header", TRUE),
    fileInput('theta', 'Choose CSV File with Person Parameters',
              accept=c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')),
    # Input: Checkbox if file has header ----
    checkboxInput("headerTheta", "Header", TRUE),
    # Input: Control number of simulations ----
    numericInput('B', 'Number of simulations', 50, min = 0, max = 1000, step = 50),
    #conditionalPanel(
    #  condition = "input.method == 'JML'",
    #  numericInput(inputId = "adj", label = "Adjustment for extremes in JML", 
    #               value = 0.3, min = 0, max = 1, step = "any")
    #),
    actionButton("go", "Go")
  ),
  mainPanel(
    #conditionalPanel(
    #  condition = "input.stats == 'Positive/Negative Count'",
    #  plotOutput("sidebarplot")
    #  ),
    #dataTableOutput("table1"),
    tabsetPanel(type = "tabs", id = "inTabset",
                tabPanel(
                  title = "Input", value = "Input",
                  fluidRow(
                    column(width = 4,
                           h3("Item parameters"),
                           DT::dataTableOutput('tbl1')),
                    column(width = 4,
                           h3("Person parameters"),
                           DT::dataTableOutput('tbl2'))
                  )),
                tabPanel("Outfit", plotOutput("plot2")),
                tabPanel("Infit", plotOutput("plot3")),
                tabPanel("FitResidual", plotOutput("plot4")))
  )
)