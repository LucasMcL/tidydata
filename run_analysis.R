library(plyr)
library(dplyr)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data.zip")
unzip(zipfile = "data.zip")

#Read in the label data for training and test groups
#Subj = subject, Act = activity
trainSubj <- read.table("./UCI HAR Dataset/train/subject_train.txt")
trainAct <- read.table("./UCI HAR Dataset/train/y_train.txt")
testSubj <- read.table("./UCI HAR Dataset/test/subject_test.txt")
testAct <- read.table("./UCI HAR Dataset/test/y_test.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
actLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Read in the unformatted data
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
test <- read.table("./UCI HAR Dataset/test/X_test.txt")

#Add SubjectID columns to both datasets and merge by SubjectID
train <- cbind(trainSubj, train); names(train)[1] <- "SubjectID"
test <- cbind(testSubj, test); names(test)[1] <- "SubjectID"
data <- merge(test, train, all = TRUE, sort = FALSE)
names(data)[2:562] <- as.character(features[,2])

#Create df with correct length and order containing activity values and labels
ActivityNum <- c(as.numeric(testAct[,1]), as.numeric(trainAct[,1]))
ActivityDF <- as.data.frame(ActivityNum)
ActivityDF <- mutate(ActivityDF, Activity = factor(ActivityDF[,1], labels = c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")))
data <- cbind(ActivityDF[,2], data); names(data)[1] <- "Activity"
data <- data[c(2,1,3:ncol(data))]

#Use grep function to find which columns have "mean()" or "std()"
#Index by those columns
meanIndex <- grep("mean()", names(data))
stdIndex <- grep("std()", names(data))
Index <- sort(c(meanIndex, stdIndex))
data <- data[ ,c(1,2,Index)]

#Create tidy data by grouping, then summarizing
ordered.data <- group_by(data, SubjectID, Activity)
tidy.data <- summarise_each(ordered.data, funs(mean))
write.csv(tidy.data, file = "tidyData.txt", row.names = FALSE)