#Getting and cleaning data course project
#The purpose of the project is to collect, work with, and clean a data set
#The goal is to prepare tidy data

#Download the data for the project to my local storage
#Unzip the folder

#Save the data to dataPath

dataPath <- "UCI HAR Dataset"

#read the training data to trainSubject, trainValues and trainActivity
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data to testSubjects, testValues and testActivity
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)


# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

#Question 1
#Merges the training and the test sets to create one data set.

humanActivity <- rbind(
  cbind(trainSubjects, trainValues, trainActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainSubjects, trainValues, trainActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")


#Question 2

#Extract only the measurements on the mean and standard deviation for each measurement

# determine columns of data set to keep based on column name...
saveColumn <- grepl("subject|activity|mean|std", colnames(humanActivity))

# ... and keep data in these columns only
humanActivity <- humanActivity[, saveColumn]


#Question 3
#Use descriptive activity names to name the activities in the dataset
##############################################################################

# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


#Question 4
#Appropriately label the data set with descriptive variable names

# get column names
humanActivityColName <- colnames(humanActivity)

# remove special characters
humanActivityColName <- gsub("[\\(\\)-]", "", humanActivityColName)

# expand abbreviations and clean up names
humanActivityColName <- gsub("^f", "frequencyDomain", humanActivityColName)
humanActivityColName <- gsub("^t", "timeDomain", humanActivityColName)
humanActivityColName <- gsub("Acc", "Accelerometer", humanActivityColName)
humanActivityColName <- gsub("Gyro", "Gyroscope", humanActivityColName)
humanActivityColName <- gsub("Mag", "Magnitude", humanActivityColName)
humanActivityColName <- gsub("Freq", "Frequency", humanActivityColName)
humanActivityColName <- gsub("mean", "Mean", humanActivityColName)
humanActivityColName <- gsub("std", "StandardDeviation", humanActivityColName)

# correct typo
humanActivityColName <- gsub("BodyBody", "Body", humanActivityColName)

# use new labels as column names
colnames(humanActivity) <- humanActivityColName


#Question 5
#Create a second, independent tidy set with the average of each variable 
#for each activity and each subject

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)