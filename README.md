# Getting-and-Cleaning-Data-Project
Repo for the "Getting and Cleaning Data" course project

## Script
This repo contains a single script "run_analysis.R". The code may also be found below:

```{r eval=FALSE}

#Clear environment and load libraries
rm(list=ls())
library(dplyr)
library(data.table)
library(rlang)

#Set working directory
wd <- "C:/Users/Peter.skot/Desktop"
setwd(wd)

#Read subjects, features and activity labels
subjects_train <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", sep="", stringsAsFactors = F)
subjects_test <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", sep="", stringsAsFactors = F)
features <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/features.txt", sep="", stringsAsFactors = F)[,2]
activity_labels <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", sep="", stringsAsFactors = F)

#Read and combine training sets
train_x <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", sep="", stringsAsFactors = F)
train_y <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
train <- cbind(subjects_train,train_y,train_x)

#Read and combine test sets
test_x <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", sep="", stringsAsFactors = F)
test_y <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
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

## Codebook
### Original UCI HAR Dataset codebook (features_info)
  
Feature selection

=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

- tBodyAcc-XYZ
- tGravityAcc-XYZ
- tBodyAccJerk-XYZ
- tBodyGyro-XYZ
- tBodyGyroJerk-XYZ
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag
- fBodyAcc-XYZ
- fBodyAccJerk-XYZ
- fBodyGyro-XYZ
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

- mean(): Mean value
- std(): Standard deviation
- mad(): Median absolute deviation 
- max(): Largest value in array
- min(): Smallest value in array
- sma(): Signal magnitude area
- energy(): Energy measure. Sum of the squares divided by the number of values. 
- iqr(): Interquartile range 
- entropy(): Signal entropy
- arCoeff(): Autorregresion coefficients with Burg order equal to 4
- correlation(): correlation coefficient between two signals
- maxInds(): index of the frequency component with largest magnitude
- meanFreq(): Weighted average of the frequency components to obtain a mean frequency
- skewness(): skewness of the frequency domain signal 
- kurtosis(): kurtosis of the frequency domain signal 
- bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
- angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

- gravityMean
- tBodyAccMean
- tBodyAccJerkMean
- tBodyGyroMean
- tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'

### Addition to the codebook

The final, tidy dataset "UCI_tidy_dataset.txt", contains some adjustments to variable names and introduction of new variables:

- Activity_code: An integer between 1 and 6 denoting each activity, corresponding to the integers in "activity_labels.txt"
- Activity: The relevant activity, corresponding to the 6 activities in "activity_labels.txt"
- \_mean: The average of the feature in question, calculated by activity and subject for each mean() and std() feature.
  For example: tBodyAcc-mean()-X\_mean corresponds to the average value of the mean value for tBodyAcc-Z.

