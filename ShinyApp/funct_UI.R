
function.header <- function() {
  dashboardHeader(title = "Naive Bayes")
}

function.sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Initialisation", tabName = "initialisation"),
      menuItem("Target Config",tabName = "targetconfig"),
      menuItem("Data Quality Config", tabName = "general", startExpanded = TRUE,
               menuSubItem("Missing Values", tabName = "dataqualityconfigMissingValues"),
               menuSubItem("Consistency", tabName = "dataqualityconfigConsisting"),
               menuSubItem("Fixing", tabName = "dataqualityconfigFixing")
               ),
      menuItem("Costs Config", tabName = "costsconfig"),
      menuItem("Results", tabName = "results"),
      menuItem("Website", icon = icon("send",lib='glyphicon'), 
               href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
    )
  )
}

function.body <- function(){
  dashboardBody(
    tabItems(
      
      #__________________________________________________ Initialisation _______________________________________________________________________________________#
      
      tabItem(
        tabName = "initialisation",
        sidebarLayout(
          sidebarPanel(
            h1("Initialisation"),
            tabsetPanel(
              id = "tabsetInitialisation",
              tabPanel(
                "Load your file",
                value = "load",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfile"),
                      uiOutput("parametersbox"),
                      fluidRow(
                        column(6, uiOutput("uploadbutton")),
                        column(6, uiOutput("demobutton"))
                      ),
                      tags$br()
                  )
                ),
                uiOutput("fromLoadToNextTab")
              ),
              tabPanel(
                "Define NAs",
                value = "defineNas",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      status = "primary",
                      title = "Define NAs",
                      solidHeader = TRUE,
                      uiOutput("checkBoxInterogation"),
                      uiOutput("checkBoxEmpty"),
                      uiOutput("checkBoxNa"),
                      uiOutput("confirmNAs")
                  ),
                  uiOutput("fromInitToNextButton")
                )
              )
            )
          ),
          mainPanel(
            dataTableOutput("tabLoadedInitialisation")
          )
        )
      ),
      
      
      #____________________________________________________ Target Config _________________________________________________________________________________________#
      
      tabItem(
        tabName = "targetconfig",
        sidebarLayout(
          sidebarPanel(
            h1("Target Config"),
            
            tabsetPanel(
              id = "tabSetTarget",
              
              tabPanel(
                "Target",
                value = "column",
                tags$br(),
                fluidRow(
                  box(
                    width = 12,
                    uiOutput("selectcolumn"),
                    tags$hr(),
                    uiOutput("foldselection"),
                    uiOutput("nextButton")
                  )
                )
              ),
              
              tabPanel(
                "Remove other targets",
                value = "removecolumn",
                tags$br(),
                box(width = 12,
                    uiOutput("checkBox"),
                    uiOutput("ValidCheckBox")
                ),
                uiOutput("fromTargetToNextButton")
              )
              
            )
          ),
          mainPanel(
            dataTableOutput("tabLoadedTargetConfig")
          )
        )
      ),
      
      
      #__________________________________________________ DataQuality Config _______________________________________________________________________________________#
      
      
      #°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° Missing Values °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#
      
      tabItem(
        tabName = "dataqualityconfigMissingValues",
        sidebarLayout(
          sidebarPanel(
            h1("Data Quality Config - Missing Values"),
            tags$hr(),
            tabsetPanel(
              id = "tabsetdataqualityconfig",
              
              tabPanel(
                "Revome columns",
                value = "removecolumnsMV",
                tags$br(),
                box(width = 12,
                    h4("Do you want to remove columns with too many missing values ?"),
                    uiOutput("pourcentageSelection"),
                    uiOutput("removecolumnbutton")
                ),
                uiOutput("nextPanelrows")
              ),
              tabPanel(
                "Remove rows",
                value = "removerows",
                tags$br(),
                box(width = 12,
                    h4("Then, do you want to remove each row where there is at least one missing value ?"),
                    uiOutput("numberRowsWillRemove"),
                    uiOutput("removeNAsbutton")
                ),
                uiOutput("fromMissingValuesToNextButton")
              )
            )
          ),
          mainPanel(
            tabsetPanel(
              id = "tabset",
              tabPanel(
                "Bar Chart",
                value = "barchart",
                h3("Pourcentage of missing values in each column"),
                plotlyOutput("NAsBarChart")
              ),
              tabPanel(
                "DataBase",
                value = "database",
                dataTableOutput("tabLoadedDQconfig")
              )
            )
          )
        )
      ),
      
      
      #°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° Consisting °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#
      
      tabItem(
        tabName = "dataqualityconfigConsisting",
        sidebarLayout(
          sidebarPanel(
            h1("Data Quality Config - Consistency"),
            
            tabsetPanel(
              id = "tabsetConsistency",
              tabPanel(
                "Types config",
                value = "typesconfig",
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfileTypes"),
                      uiOutput("parametersboxTypes"),
                      uiOutput("typesButton")
                  )
                )
              ),
              tabPanel(
                "Ranges config",
                value = "rangesconfig",
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfileRanges"),
                      uiOutput("parametersboxRanges"),
                      uiOutput("rangesButton")
                  )
                )
              ),
              tabPanel(
                "Remove rows",
                value = "removeConsistency",
                fluidRow(
                  box(width = 12,
                      h4("Do you want to remove each row where there is an inconsistent value ?"),
                      uiOutput("numberRowsInconsistentWillRemove"),
                      uiOutput("removeInconsistentbutton")
                  )
                )
              )
            )
          ),
          mainPanel(
            dataTableOutput("typesFile"),
            dataTableOutput("rangesFile"),
            dataTableOutput("tabLoadedDQconfigConsistency"),
            dataTableOutput("tabmatrix")
          )
        )
      ),
      
      
      #°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° Fixing °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°#
      
      tabItem(
        tabName = "dataqualityconfigFixing",
        h1("Work in progress"),
        uiOutput("fromFixingToNextButton")
      ),
      
      
      #____________________________________________________ Costs Config _________________________________________________________________________________________#
      
      tabItem(
        tabName = "costsconfig",
        sidebarLayout(
          sidebarPanel(
            h1("Costs Config"),
            tags$br(),
            tabsetPanel(
              id = "tabsetcosts",
              tabPanel(
                "Prediction",
                value = "prediction",
                fluidRow(
                  box(width = 12,
                      helpText("Editable table : Choose costs and validate"),
                      rHandsontableOutput("costsTab"),
                      tags$br(),
                      uiOutput("validate"),
                      uiOutput("downloadButton")
                  ),
                  tags$hr(),
                  uiOutput("fromPredictionTabToNext")
                )
              ),
              tabPanel(
                "Fixing",
                value = "fixing",
                fluidRow(
                  box(width = 12,
                      uiOutput("costFixingSelection")
                    )
                ),
                uiOutput("fromCostsToNextButton")
              )
            )
          ),
          mainPanel(
            dataTableOutput("tabLoadedCostsConfig")
          )
        )
      ),
      
      #_______________________________________________________ Results ___________________________________________________________________________________________#
      
      tabItem(
        tabName = "results",
        
          fluidRow(
            
            column(6,
                   h1("Results - Initial"),
                   tags$hr(),
                   uiOutput("accuracyvalueSaved"),
                   tags$hr(),
                   uiOutput("boxBarChartSaved"),
                   tags$hr(),
                   uiOutput("costResultsValueSaved"),
                   tags$hr()
                   ,
                   uiOutput("infodataSaved"),
                   dataTableOutput("tabLoadedResultsSaved"),
                   dataTableOutput("matrixBooleanInit")
            ),
            
            column(6,
                   h1("Results - According Data Quality Config"),
                   tags$hr(),
                   uiOutput("accuracyvalue"),
                   tags$hr(),
                   uiOutput("boxBarChar"),
                   tags$hr(),
                   uiOutput("costresultsvalue"),
                   tags$hr()
                   ,
                   uiOutput("infodata"),
                   dataTableOutput("tabLoadedResults")
            )
            
          )
        )
      )
      
  )
}