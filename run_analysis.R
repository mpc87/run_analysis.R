# https://rpubs.com/MegaManZero/683722
#https://github.com/wdluft/getting-and-cleaning-data-week-4-project/blob/main/run_analysis.R

# Download the unzipped file first and save the zipped file to local document
# library:
library(dplyr)
library(data.table)

#Read data
feature_Names <- read.table("./UCI HAR Dataset/features.txt")
activity_Labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

subject_Train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity_Train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
features_Train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)

subject_Test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity_Test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
features_Test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Merges the training and the test sets to create one data set.

merge_subject <- rbind(subject_Train, subject_Test)
merge_activity <- rbind(activity_Train, activity_Test)
merge_features <- rbind(features_Train, features_Test)

colnames(merge_features) <- t(feature_Names[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

complete <- cbind(features,activity,subject)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_sd <- grep(".*Mean.*|.*Std.*",names(complete), ignore.case = T)

column_mean_sd <- complete[,mean_sd]

#Uses descriptive activity names to name the activities in the data set
column_mean_sd$Activity <- factor(column_mean_sd$Activity)
for (i in 1:6){
  column_mean_sd$Activity[column_mean_sd$Activity == i] <- as.character(activity_Labels[i,2])
}

#Appropriately labels the data set with descriptive variable names. 
column_mean_sd <- gsub("^f", "frequencyDomain", column_mean_sd)
column_mean_sd <- gsub("^t", "timeDomain", column_mean_sd)
column_mean_sd <- gsub("Acc", "Accelerometer", column_mean_sd)
column_mean_sd <- gsub("Gyro", "Gyroscope", column_mean_sd)
column_mean_sd <- gsub("Mag", "Magnitude", column_mean_sd)
column_mean_sd <- gsub("Freq", "Frequency", column_mean_sd)
column_mean_sd <- gsub("mean", "Mean", column_mean_sd)
column_mean_sd <- gsub("std", "StandardDeviation", column_mean_sd)


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
column_mean_sd$Subject <- as.factor(column_mean_sd$Subject)
column_mean_sd <- data.table(column_mean_sd)

subject_ <- aggregate(. ~Subject + Activity, column_mean_sd, mean)
subject_mean <- subject_mean[order(subject_mean$Subject,subject_mean$Activity),]

write.table(subject_mean, file = "R_data.txt", row.names = FALSE) 
