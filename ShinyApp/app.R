library(shiny)
require(shinydashboard)
library(e1071) # Naive Bayes
library(mlr)
library(caret) 
library(dplyr)
library(plotly) # Plots
library(rhandsontable) # Edit table

source("funct_UI.R")
source("funct_reactivevalues.R")
source("funct_initStep.R")
source("funct_removeMissingValues.R")
source("funct_CVNaiveBayes.R")
source("funct_results.R")
source("funct_other.R")


ui <- dashboardPage(title = 'Data Quality test - Week 5', function.header(), function.sidebar(), function.body(), skin='yellow')


server <- function(input, output, session) {
    
    v <- function_reactiveValues()
    
    #__________________________________________________ Initialisation _____________________________________________________________________________________________________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    
    output$uploadbutton <- renderUI({
        actionButton("uploadbutton","Upload")
    })
    observeEvent(input$uploadbutton,{
        infile <- input$fileCSV
        if (is.null(infile)) return (NULL)
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile(infile$datapath, input$header , input$sep , input$quote)
    })
    
    
    
    output$demobutton <- renderUI({
        actionButton("demobutton","Upload a Demo")
    })
    observeEvent(input$demobutton,{
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile("risk_factors_cervical_cancer_Copie.csv", input$header , input$sep , input$quote)
    })
    
    
    
    output$fromInitToNextButton <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromInitToNextButton","Next step")
    })
    observeEvent(input$fromInitToNextButton,{
        v$dataframe_targetconfig <- v$dataframe_initialisation
        updateTabItems(session,"sidebarmenu", "targetconfig")
    })
    
    output$fromLoadToNextTab <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromLoadToNextTab", "Next")
    })
    observeEvent(input$fromLoadToNextTab, {
        updateTabsetPanel(session, "tabsetInitialisation", "defineNas")
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    output$selectionfile <- renderUI(
        function.fileInput()
    )
    
    
    output$parametersbox <- function_parametersBox()
    
    output$checkBoxInterogation <- renderUI({
        checkboxInput("interrogation", "?")
    })
    
    output$checkBoxEmpty <- renderUI({
        checkboxInput("empty", "\" \"")
    })
    
    output$checkBoxNa <- renderUI({
        checkboxInput("na", "NA")
    })
    
    output$confirmNAs <- renderUI({
        actionButton("confirmNAs", "OK")
    })
    observeEvent(input$confirmNAs, {
        v$dataframe_initialisation <- v$dataframe_initialisationBis
        if (input$interrogation){
            for (col in names(v$dataframe_initialisation )) {
                v$dataframe_initialisation [which(v$dataframe_initialisation [,col] == "?"), col] <- NA
            }
        }
        if (input$empty){
            for (col in names(v$dataframe_initialisation )) {
                v$dataframe_initialisation [which(v$dataframe_initialisation [,col] == ""), col] <- NA
            }
        }
        if (input$na){
            for (col in names(v$dataframe_initialisation )) {
                v$dataframe_initialisation [which(v$dataframe_initialisation [,col] == "NA"), col] <- NA
            }
        }
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$tabLoadedInitialisation <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    #____________________________________________________ Target Config __________________________________________________________________________________________________________________________________________#
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$selectcolumn <- renderUI(
        function.selectionColumn(v$dataframe_initialisation)
    )
    observeEvent(input$selectcolumn,{
        v$columnSelected <- input$selectcolumn
    })
    
    
    output$foldselection <- renderUI({
        sliderInput("foldselection","Number of fold for Cross Validation (Naive Bayes)", 1,50,10)
    })
    
    output$checkBox <- renderUI({ 
        
        v$dataframe_withoutcolselected <- v$dataframe_targetconfig[,!names(v$dataframe_targetconfig)%in%v$columnSelected]
        newList <- rev(names(v$dataframe_withoutcolselected))
        checkboxGroupInput("targets",label = "Select target(s)", choices = newList)
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$nextButton <- renderUI({
        actionButton("nextButton","Next")
    })
    observeEvent(input$nextButton,{
        updateTabsetPanel(session, "tabSetTarget", "removecolumn")
    })
    
    
    output$ValidCheckBox <- renderUI({
        actionButton("OK","Remove")
    })
    observeEvent(input$OK,{
        if (!is.null(input$targets)){
            
            list <- data.frame(Column = input$targets)
            v$dataframe_targetconfig <- v$dataframe_targetconfig[,!names(v$dataframe_targetconfig)%in%list$Column]
            
        }
    })
    
    
    output$fromTargetToNextButton <- renderUI({
        
        actionButton("fromTargetToNextButton","Next Step")
    })
    observeEvent(input$fromTargetToNextButton,{
        v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfigBis <- v$dataframe_targetconfig
        updateTabItems(session,"sidebarmenu", "dataqualityconfig")
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    output$tabLoadedTargetConfig <- renderDataTable(
        v$dataframe_targetconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    
    #__________________________________________________ DataQuality Config _________________________________________________________________________________________________________________________________________#
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$nextPanelrows <- renderUI({
        actionButton("nextPanelrows","Next")
    })
    observeEvent(input$nextPanelrows,{
        updateTabsetPanel(session, "tabsetdataqualityconfig", selected = "removerows")
    })
    
    
    
    output$removecolumnbutton <- renderUI({
        if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
        actionButton("removecolumnbutton","Remove")
    })
    observeEvent(input$removecolumnbutton,{
        v$dataframe_dataqualityconfig <- function.removeColumns(v$resNAsBarChart, v$dataframe_dataqualityconfigBis, input$pourcentageSelection, v$columnSelected)
    })
    
    
    
    output$removeNAsbutton <- renderUI({
        if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
        actionButton("removeNAsbutton","Remove")
    })
    observeEvent(input$removeNAsbutton,{
        v$dataframe_dataqualityconfig <- function.removeRows(v$dataframe_dataqualityconfig, "")
        updateTabsetPanel(session, "tabset", selected = "database")
    })
    
    output$numberRowsWillRemove <- renderUI({
        nb <- function.removeRows(v$dataframe_dataqualityconfig, "number")
        paste("(Number of rows will be removed : ", nb,"/",nrow(v$dataframe_dataqualityconfig),")")
    })
    
    
    
    output$fromDataQualityToNextButton <- renderUI({
        if (is.null(v$dataframe_dataqualityconfig)) return (NULL)
        actionButton("fromDataQualityToNextButton","Next Step")
    })
    observeEvent(input$fromDataQualityToNextButton,{
        v$dataframe_costsconfig <- function.as_factor(v$dataframe_dataqualityconfig)
        v$tabCosts <- function.tabNaiveBayes(v$dataframe_costsconfig, v$columnSelected)
        updateTabItems(session,"sidebarmenu", "costsconfig")
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    output$pourcentageSelection <- renderUI(
        sliderInput("pourcentageSelection","Pourcentage of missing values max", 0,100,15)
    )
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$tabLoadedDQconfig <- renderDataTable(
        v$dataframe_dataqualityconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    output$NAsBarChart <- renderPlotly({
        v$resNAsBarChart <-res <- function.barChartMissingValues(v$dataframe_dataqualityconfig)
        res <- sort(res, decreasing = TRUE)
        col_names <- names(res)
        
        plot_ly(x = factor(col_names, levels = col_names), 
                y = res, 
                type = "bar",
                color = res > input$pourcentageSelection, colors = c("#132B43", "#56B1F7")
        ) %>% 
            layout(xaxis = list(title = "Column's name"),
                   yaxis = list(title = "Pourcentage of missing values"))
        
        
    })
    
    
    
    #____________________________________________________ Costs Config __________________________________________________________________________________________________________________________________________#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$tabLoadedCostsConfig <- renderDataTable(
        v$dataframe_costsconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    output$costsTab <- renderRHandsontable({
        rhandsontable(v$tabCosts)
    })
    
    
    output$downloadData <- function.downloadFile(v$tabCosts)
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$downloadButton <- renderUI({
        if (v$validate == FALSE) return(NULL)
        downloadButton('downloadData', 'Download Costs Tab')
    })
    
    output$validate <- renderUI(
        actionButton("validate","Validate"),
    )
    observeEvent(input$validate,{
        v$tabCosts <- function.saveDataInFile(input$costsTab, "MyData.csv")
        v$validate <- TRUE
    })
    
    
    output$fromCostsToNextButton <- renderUI({
        if (is.null(v$dataframe_costsconfig) || v$validate == FALSE) return (NULL)
        actionButton("fromCostsToNextButton","Results")
    })
    observeEvent(input$fromCostsToNextButton,{
        
        #As factor to run naive Bayes
        v$dataframe_results <- v$dataframe_costsconfig
        v$dataframe_targetconfig <- function.as_factor(v$dataframe_targetconfig)
        
        
        # Naive Bayes INITIAL 
        resultats <- function.CVNaiveBayes(v$dataframe_targetconfig,input$selectcolumn,v$tabCosts,input$foldselection)
        v$resultDataSaved = sum(resultats$restab$cost * v$tabCosts$cost) * 5 
        v$accuracySaved <<- mean(resultats$moy)
        v$accuracyTabSaved <<- resultats$moy
        
        
        # Naive Bayes according DQ config #
        resultats <- function.CVNaiveBayes(v$dataframe_results,input$selectcolumn,v$tabCosts,input$foldselection)
        v$resultData = sum(resultats$restab$cost * v$tabCosts$cost) * 5 
        v$accuracy <<- mean(resultats$moy)
        v$accuracyTab <<- resultats$moy
        
        
        updateTabItems(session,"sidebarmenu", "results")
    })
    
    
    #_______________________________________________________ Compare Results INITIAL / DQ config ____________________________________________________________________________________________#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results initial ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$accuracyvalueSaved <- renderValueBox(
        function.accuracyBoxWithConfInterval(v$accuracyTabSaved, v$accuracySaved)
    )
    
    
    
    output$accuracyCVBarSaved <- renderPlotly (
        function.accuracyCVBarChart(v$accuracyTabSaved, v$accuracySaved, input$foldselection)
    )
    
    
    
    output$boxBarChartSaved <- renderUI(
        function.BarChartBox(v$accuracySaved, "accuracyCVBarSaved")
    )
    
    
    
    output$costResultsValueSaved <- renderValueBox(
        function.costsResultsVaue(v$resultDataSaved)
    )
    
    output$infodataSaved <- renderUI({
        comp <- function.nbMissingValues(v$dataframe_targetconfig)
        fluidRow(
            h4("Initial table : ", ncol(v$dataframe_targetconfig), " x ", nrow(v$dataframe_targetconfig), "  (columns x rows)"),
            h4("Missing Values : ", comp)
        )
    })
    
    
    output$tabLoadedResultsSaved <- renderDataTable(
        v$dataframe_targetconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results with DATA QUALITY config ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$accuracyvalue <- renderValueBox(
        function.accuracyBoxWithConfInterval(v$accuracyTab, v$accuracy)
    )
    
    
    
    output$accuracyCVbar <- renderPlotly (
        function.accuracyCVBarChart(v$accuracyTab, v$accuracy, input$foldselection)
    )
    output$boxBarChar <- renderUI(
        function.BarChartBox(v$accuracy, "accuracyCVbar")
    )
    
    
    
    output$costresultsvalue <- renderValueBox(
        function.costsResultsVaue(v$resultData)
    )
    
    
    output$infodata <- renderUI({
        comp <- function.nbMissingValues(v$dataframe_results)
        fluidRow(
            h4("New table : ", ncol(v$dataframe_results), " x ", nrow(v$dataframe_results), "  (columns x rows)"),
            h4("Missing Values : ", comp)
        )
    })
    
    
    output$tabLoadedResults <- renderDataTable(
        v$dataframe_results,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
