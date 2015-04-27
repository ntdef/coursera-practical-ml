#' ---
#' title: "Predicting Exercise Performance"
#' author: "N. Troy de Freitas"
#' date: "April 25th, 2015"
#' 
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

#+ setup, include=FALSE
library(knitr)
notify <- function(msg="Operation complete") {
  
  in.osx <- (Sys.info()['sysname'] == "Darwin")
  in.rstudio <- (Sys.getenv("RSTUDIO") == "1")
  in.rgui <- (Sys.getenv("R_GUI_APP_REVISION") != "")
  
  if (in.rstudio) { # hack to see if running in RStudio
    title <- "RStudio"
    sender <- activate <- "org.rstudio.RStudio"
  }
  
  if (in.rgui) { # running in R GUI app?
    title <- "R GUI"
    sender <- activate <- "org.R-project.R"
  }
  
  # if running in RStudio or R GUI app use NotificationCenter otherwise use message()
  if ((in.rstudio | in.rgui) & in.osx) {
    system(sprintf("/usr/bin/terminal-notifier -title '%s' -message '%s' -sender %s -activate %s",
                   title, msg, sender, activate ),
           ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
  } else {
    message(msg)      
  }
  
}

#' ## Predicting Exercise Quality with Random Forest
#' 
#' The goal of this report is to show how Random Forest can be applied to accurately predict weightlifiting 
#' quality in participants from the Weightlifting Exercise Dataset. To build my model, I created a testing
#' and training partition of the data and trained a Random Forest and a Naive Bayes on the training data.
#' I chose Random Forest because of its winning track record in Kaggle competitions and used Naive Bayes
#' as a benchmark because of its simplicity and assumption of independenc of the features, which seemed
#' valid for this dataset where all of the components are indpendent vectors.
#' I used Bootstraping to cross-validate my model for both the Naive Bayes and
#' the Random Forest model. I expect my out of sample error for the Random Forest model to be bounded 
#' between 0.85 and 0.95, just under
#' the restulst of my training model
#' The results showed that Random Forest strongly outperformed the Naive Bayes model, although Random Forest
#' took much longer to train.
#' 
#' What follows is the code used in my study.
#' 
library(caret)
library(ggplot2)
library(Hmisc)
set.seed(1337)

#+ echo=FALSE
data <- read.csv("~/Documents/data-science/coursera/pml-training.csv")

#' For this data set, I chose to only use the raw features to avoid multi-collinearity in my features
#' since variance, average, max, etc are all functions of the data.
#+ echo=FALSE
features <- names(data)[!grepl("^var|new|timestamp|amplitude|avg|kurtosis|skewness|max|min|stddev", names(data))]
subset <- data[,features]

#' Here, I split the data into a 60% training and 40% testing.
#+ echo=FALSE
train.idx <- createDataPartition(subset$classe, p=0.6, list=F)
training <- subset[train.idx,]
testing  <- subset[-train.idx,]

#' Here, I train my model with both Random Forest and Naive Bayes, pre-processing first by centering
#' and scaling and deriving the principal components.
#+ eacho=TRUE
randomForest <- train(classe ~ ., data=training, method="rf", preProcess = c("center", "scale"))
naiveBayes   <- train(classe ~ ., data=training, method="nb", preProcess = c("center", "scale"))

#' Once I derived my model, I predict results on the test set.
#+ echo=TRUE, cahche=TRUE
pred.randomForest <- predict(randomForest, testing$classe)
pred.naiveBayes   <- predict(naiveBayes, testing$classe)
summary(randomForest)
summary(naiveBayes)

#' I defined a function to evaludate my prediciton accuracy.
#+ echo=TRUE
accuracy  <- function(prediction) {
  return(sum(diag(prop.table(table(prediction, testing$classe)))))
}

accuracy(pred.randomForest)
accuracy(pred.naiveBayes)

install.packages("doParallel")
