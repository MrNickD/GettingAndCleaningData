## Load necessary libraries
library(dplyr)
library(tidyr)

## Read in training, test, features, subjects, and both activity data sets
train <- read.table("X_train.txt")
trainY <- read.table("y_train.txt")
test <- read.table("X_test.txt")
testY <- read.table("y_test.txt")
features <- read.table("features.txt")
train_subject <- read.table("subject_train.txt")
test_subject <- read.table("subject_test.txt")

## Rename activity observations to descriptive activity names
testY <- transmute(testY, activity = factor(V1, labels = 
                  c("walking", "walking_upstairs", "walking_downstairs", 
                    "sitting", "standing", "laying")))
trainY <- transmute(trainY, activity = factor(V1, labels = 
                  c("walking", "walking_upstairs", "walking_downstairs", 
                    "sitting", "standing", "laying")))

## Rename 'subject' label
names(test_subject) <- "subject"
names(train_subject) <- "subject"

## Bind activities and subjects to data sets
test <- cbind(testY, test_subject, test)
train <- cbind(trainY, train_subject, train)
rm(list = c("testY", "test_subject", "trainY", "train_subject"))

## Merge 'test' and 'train' data sets
mergedData <- merge(test, train, all = TRUE)
rm(list = c("test", "train"))

## Rename variable names to value of 'feature' table
names(mergedData) <- c("activity", "subject", as.character(features$V2))

## Create logival vector to isolate mean and std. dev of features
mean_std_strip <- c(TRUE, TRUE, grepl("std\\()|mean\\()",features$V2))
rm("features")

## Reduce data set to only mean and std. dev values
newData <- mergedData[,mean_std_strip]
rm(list = c("mergedData", "mean_std_strip"))

## Rename 'features' using more descriptive language
names(newData) <- gsub("^f", "frequency.", names(newData))
names(newData) <- gsub("^t", "time.", names(newData))
names(newData) <- gsub("-", ".", names(newData))
names(newData) <- gsub("mean\\()", "mean", names(newData))
names(newData) <- gsub("std\\()", "standardDeviation", names(newData))

## Group data by both 'subject' and 'activity'
newData <- newData %>% group_by(activity, subject)
      
## Create new tidy data set
## Calculate mean grouped by activity and subject
my_tidy_data <- newData %>% summarise_each(funs(mean))

## print out the tidy data to a .csv file
write.table(my_tidy_data, file = "output.txt", row.names = FALSE)