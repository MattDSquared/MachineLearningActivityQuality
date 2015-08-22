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
keepvars <- colSums(is.na(rawdat.train)) < .1*nrow(rawdat.train)
training <- rawdat.train[,keepvars]

training <- mutate(training, 
                   timestamp = training$raw_timestamp_part_1 + 
                       training$raw_timestamp_part_2/1e6, 
                   classe = factor(classe),
                   new_window = factor(new_window), 
                   user_name = factor(user_name))

dummies <- dummyVars(classe ~ user_name + new_window, data=training)

training <- cbind(training, predict(dummies, newdata=training))

training <- select(training, 
                   -user_name, 
                   -raw_timestamp_part_1, 
                   -raw_timestamp_part_2,
                   -cvtd_timestamp,
                   -new_window)
       
## subset training into local test and train data
inTrain <- createDataPartition(training$classe, p=.1, list=FALSE)
training <- training[inTrain,]
quizing <- training[-inTrain,]

## data exploration
gg <- qplot(timestamp, num_window, 
            data=filter(training,timestamp<1322600000))
print(gg)

gg <- qplot(timestamp, pitch_belt, data=filter(training,timestamp<1322600000))
gg <- gg + geom_point(aes(timestamp, roll_belt), colour="blue")
gg <- gg + geom_point(aes(timestamp, yaw_belt), colour="green")
print(gg)

gg <- qplot(timestamp, accel_belt_x, data=filter(training,timestamp<1322600000))
gg <- gg + geom_point(aes(timestamp, accel_belt_y), colour="blue")
gg <- gg + geom_point(aes(timestamp, accel_belt_z), colour="green")
print(gg)

train.pca <- prcomp(select(training, -classe), center=TRUE, scale=TRUE)
print(summary(train.pca))

## Train Basic Model
model <- train(training$classe ~ ., method="rpart", data=training)
print(confusionMatrix(quizing$classe, predict(model, quizing)))

## PCA Preprocessing
pre.pca <- preProcess(select(training, -classe), method="pca", thresh=.95)
train.pca <- predict(pre.pca, select(training, -classe))
quiz.pca <- predict(pre.pca, select(quizing,-classe))

## Train PCA Model
model <- train(training$classe ~ ., method="rpart", data=train.pca)
print(confusionMatrix(quizing$classe, predict(model, quiz.pca)))
