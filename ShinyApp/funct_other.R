
function.downloadFile <- function(tabCosts) {
  downloadHandler(
    
    filename = function() {
      paste("MydataDownload", "csv", sep = ".")
    },
    
    content = function(file) {
      write.table(tabCosts, file, sep = ",",
                  row.names = FALSE)
    }
  )
}


function.saveDataInFile <- function(costsTab, file){
  write.csv(hot_to_r(costsTab), file,row.names = FALSE)
  return(as.data.frame(read.csv(file)))
}



function.matrixBooleanConsistency <- function(df){
  
  types <- read.csv("TypesData.csv", header = TRUE, sep = ";")
  ranges <- read.csv("RangesData.csv", header = TRUE, sep = ";")
  
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- matrix (rep(0, n1*n2), n1, n2)
  a <- data.frame(a)
  names(a) <- names(df)
  
  for (col in names(df)) {
    
    df[,col] <- as.character(df[,col])
    
    if (types[,col] == "numeric") {
      df[,col] <- as.numeric(df[,col])
    }
    
    else if (types[,col] == "logical") {
      df[,col] <- as.logical(df[,col])
    }
    
    
    for (ligne in row.names(df)){
      if (types[,col] == "numeric") {
        if (! is.na(df[ligne,col])){
          if (df[ligne,col] <= ranges[1,col] || df[ligne,col] >= ranges[2,col]) {
            a[ligne,col] <- 1
          }
        }
        else a[ligne,col] <- 1
        
      }
      else if (types[,col] == "string") {
        if (! is.na(df[ligne,col])){
          if (df[ligne,col] %in% levels(ranges[,col]) && df[ligne,col] != ""){
          }
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
      else if (types[,col] == "logical") {
        if (!is.na(df[ligne,col])){
          if (df[ligne,col] == TRUE || df[ligne,col] == FALSE){
          }
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
    }
  }
  
  return(a)
  
}


function.fileInputTypes <- function(){
  fileInput("fileCSVTypes", "CSV File Types",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))
}

function.fileInputRanges <- function(){
  fileInput("fileCSVRanges", "CSV File Ranges",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))
}

function_parametersBoxTypes <- function(){
  
  renderUI({
    box(width = 12,
        title = "Parameters Types (CSV)",
        status = "primary",
        solidHeader = TRUE,
        column(6,
               checkboxInput("headerTypes", "Header", TRUE),
               radioButtons("sepTypes", "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ',')
        ),
        column(6,
               radioButtons("quoteTypes", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = "")
        )
    )
  })
  
}

function_parametersBoxRanges <- function(){
  
  renderUI({
    box(width = 12,
        title = "Parameters Ranges (CSV)",
        status = "primary",
        solidHeader = TRUE,
        column(6,
               checkboxInput("headerRanges", "Header", TRUE),
               radioButtons("sepRanges", "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ',')
        ),
        column(6,
               radioButtons("quoteRanges", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = "")
        )
    )
  })
  
}


function.removeConsistency <- function(df, a){
  rem <- 0
  for (row in row.names(a)) {
    if (!1 %in% a[row,]) rem[row] = row
  }
  rem <- rem[-1]
  return(df[rem,])
}
















