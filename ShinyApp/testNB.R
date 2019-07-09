library(mlr)
library(caret)
library(dplyr)


source("ShinyApp/funct_CVNaiveBayes.R")

df <- read.csv("risk_factors_cervical_cancer_Original.csv", header = TRUE, sep = ",")

df <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")

for (col in names(df)) {
  print(class(df[,col]))
  if(is.logical(df[,col])) df[,col] <- as.factor(df[,col])
  print(class(df[,col]))
}
colName <- "Biopsy"
#colNA <- is.na(col)
#df <- df[!colNA,]

task = makeClassifTask(data = df, target = colName)
selected_model = makeLearner("classif.naiveBayes")
NB_mlr = mlr::train(selected_model, task)

predictions_mlr = as.data.frame(predict(NB_mlr, newdata = df[,!names(df) %in% c(colName)]))
tab <- table(predictions_mlr[,1],df[,c(colName)])
tab

res <- function.CVNaiveBayes(df, "Biopsy",tab,10)
res[1]
library(e1071)

df <- read.csv("risk_factors_cervical_cancer_Original.csv", header = TRUE, sep = ",")

Naive_Bayes_Model=e1071::naiveBayes(df[,"Biopsy"] ~., data = df)
NB_Predictions=predict(Naive_Bayes_Model,df)
NB_Predictions
df[,"Biopsy"]
table(NB_Predictions,df[,"Biopsy"])

function.tabNaiveBayesBis(df,"Biopsy")





col = "Biopsy"
df$Biopsy <- as.factor(df$Biopsy)

training.samples <- df[,col] %>% 
  caret::createDataPartition(p = 0.8, list = FALSE)
train.data <- df[training.samples, ]
test.data <- df[-training.samples, ]

Naive_Bayes_Model=naiveBayes(train.data[,col] ~., data = train.data)
NB_Predictions=predict(Naive_Bayes_Model,test.data)
NB_Predictions
table(NB_Predictions,test.data[,col])

task <<- makeClassifTask(data = train.data, target = col)
selected_model <<- makeLearner("classif.naiveBayes")

NB_mlr <<- mlr::train(selected_model, task)

predictions_mlr <<- as.data.frame(predict(NB_mlr, newdata = test.data[,!names(df_noNAs) %in% c(col)]))
resultNaiveBayes <<- table(predictions_mlr[,1],test.data[,col])
as.data.frame(resultNaiveBayes)











