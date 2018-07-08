# Getting-and-Cleaning-Data-Project
Repo for the "Getting and Cleaning Data" course project

## Script
This repo contains a single script "run_analysis.R". The code may also be found below (short description of the script is found below the script):

```{r eval=FALSE}

#Clear environment and load libraries
rm(list=ls())
library(dplyr)
library(data.table)
library(rlang)

#Set working directory
wd <- "C:\\Users\\PC\\Dropbox\\Programming\\Data Science course\\WD"
setwd(wd)

#Read subjects, features and activity labels
subjects_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep="", stringsAsFactors = F)
subjects_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep="", stringsAsFactors = F)
features <- read.table("./UCI HAR Dataset/features.txt", sep="", stringsAsFactors = F)[,2]
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", sep="", stringsAsFactors = F)

#Read and combine training sets
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt", sep="", stringsAsFactors = F)
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
train <- cbind(subjects_train,train_y,train_x)

#Read and combine test sets
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt", sep="", stringsAsFactors = F)
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")
test <- cbind(subjects_test,test_y,test_x)

#Name sets
names(activity_labels) <- c("Activity_code", "Activity")
names(train) <- c("Subject", "Activity_code", features)
names(test) <- c("Subject", "Activity_code", features)

#Join sets and labels
train <- inner_join(activity_labels, train, by = "Activity_code")
test <- inner_join(activity_labels, test, by = "Activity_code")

#Merge training and test set
full_set <- merge(train, test, all = TRUE)

#Select means and standard deviations as well as activity related variables
selected_set <- full_set %>% select(matches('Activity|Subject|mean\\(\\)|std\\(\\)'))


##################################################
##################################################
######Tidy dataset with averages by activity######
##################################################
##################################################

#Select relevant features
selected_features <- features[grep('mean\\(\\)|std\\(\\)', features)]

#Declare dataframe for storing feature means by activity
feature_means <- data.frame(num=rep(NA, nrow(activity_labels)*nrow(as.data.frame(unique(full_set$Subject)))))

#Calculate means by activity and append to dataframe for each feature
for (j in 1:length(selected_features)){
  feature_means[[j]] <- 
    selected_set %>% 
    group_by(Activity_code, Subject) %>% 
    summarize(mean = mean(!!sym(selected_features[j]))) %>% 
    arrange(Activity_code, Subject) %>% 
    ungroup() %>% 
    select(mean)
}

subject_activity <- 
  left_join(activity_labels,
            selected_set %>% distinct(Activity_code, Subject),
            by = "Activity_code") %>% 
            arrange(Activity_code, Subject)

#Rename columns
mean_names <- paste(selected_features, "mean", sep="_")
feature_means <- do.call(data.frame, feature_means)
names(feature_means) <- mean_names

#Finalize tidy dataset with labels
means_by_activity <- cbind(subject_activity, feature_means)

#Export dataset
write.table(means_by_activity, "./UCI_tidy_dataset.txt", row.names = FALSE)

```

The script contains comments explaining each step, but here is a summary of the script, in order of execution:
  - All relevant files are read: Subjects, features, activities as well as the respective x- and y sets (training and test)
  - The test and training sets are labeled with the relevant activities and activity codes, and the sets are combined to 1 set
  - A final "selected set" is created, selecting only the means and standard deviations of each feature
  - Based on the "selected set" the mean is calculated by Activity and Subject, looping over each feature
    - The dataset is finalized with renaming of columns and labeling with activity and subject
