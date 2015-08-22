library(caret)
library(dplyr)

set.seed(1433)

setwd("~/../datascience/MachineLearningActivityQuality")

## download data
dir.create("data", showWarnings = FALSE)
filepath.train <- "data/pml-training.csv"
fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
if (!file.exists(filepath.train)) 
    download.file(fileURL, filepath.train)
filepath.test <- "data/pml-testing.csv"
fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if (!file.exists(filepath.test)) 
    download.file(fileURL, filepath.test)

## load data
rawdat.train <- read.table(filepath.train, header=TRUE, sep=",", quote="\"", 
                           na.strings=c("#DIV/0!","NA"),
                           fill=TRUE,
                           comment.char="",
                           stringsAsFactors=FALSE)
rawdat.test <- read.csv(filepath.test)

## Data Cleaning
keepvars <- colSums(is.na(rawdat.train)) < .5*nrow(rawdat.train)
training <- rawdat.train[,keepvars]

## subset training into local test and train data
inTrain <- createDataPartition(training$classe, p=.6, list=FALSE)
training <- training[inTrain,]
quizing <- training[-inTrain,]
