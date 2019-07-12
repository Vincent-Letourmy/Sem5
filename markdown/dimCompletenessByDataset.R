dimCompletenessByDataset <- function(repository){
  N = dim(repository)[1]
  NAmatrix <- !is.na(repository)
  sumNAmatrix <-apply(NAmatrix,2,sum)
  completenessByColumn <-round(sumNAmatrix/N*100,2)
  completenessByDataset <- mean(completenessByColumn)
  return(completenessByDataset)
}

