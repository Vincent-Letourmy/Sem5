source("funct_CVNaiveBayes.R")

function.fileInput <- function(){
  fileInput("fileCSV", "CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))
}

function.loadFile <- function(file, header, sep, quote){
  df <- read.csv(file,
           header = header, 
           sep = sep,
           quote = quote)
  return(df)
}

function.selectionColumn <- function(df){
  if (is.null(df)) {
    return (h4("Please upload a file and then select a column"))
  }
  items=rev(names(df))
  names(items)=items
  selectInput("selectcolumn", "Choose a column (try with \"Smokes\")",items)
}

function_parametersBox <- function(){
  
  renderUI({
    box(width = 12,
        title = "Parameters (CSV)",
        status = "primary",
        solidHeader = TRUE,
        column(6,
               checkboxInput("header", "Header", TRUE),
               radioButtons("sep", "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ',')
        ),
        column(6,
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = "")
        )
    )
  })
  
}

function.matrixBoolean <- function(df){
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- matrix (rep(0, n1*n2), n1, n2)
  a <- data.frame(a)
  names(a) <- names(df)
  
  for (col in names(df)) {
    ligne <- 1
    for (val in df[,col]) {
      if (is.na(val) || val == "" || val == "?"){
        a[ligne,col] <- 1
      }
      ligne <- ligne + 1
    }
  }
  return(a)
}
















