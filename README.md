==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

##The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

# HOW THE SCRIPT WORKS
setwd("C:/r")   ## Set your working directory

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  ##Download your
destFile <- "CourseDataset.zip"                                                                  ## file if
if (!file.exists(destFile)){                                                                     ## it is not
  download.file(URL, destfile = destFile, mode='wb')                                             ## yet downloaded
}

if (!file.exists("./UCI_HAR_Dataset")){             ## unzip your file
  unzip(destFile)                                   ## if not yet unziped
}

setwd("./UCI HAR Dataset")     ## Set your working directory to "UCI HAR Dataset"
                               ## for you to read the "train" and "test" directory/folder 

library(data.table)   ##load Packeges install it first if
library(dplyr)        ##not yet installed

TestActivity <- read.table("./test/y_test.txt", header = F)      ##Read y_test.txt as Activity
TestFeatures <- read.table("./test/X_test.txt", header = F)      ##X_test.txt as Features 
TestSubject <- read.table("./test/subject_test.txt", header = F) ##sujext_test.txt as suject from Test Directory/folder

TrainActivity <- read.table("./train/y_train.txt", header = F)       ##Read  y_train.txt as Activity
TrainFeatures <- read.table("./train/X_train.txt", header = F)       ##X_train.txt as Features
TrainSubject <- read.table("./train/subject_train.txt", header = F)  ##sujext_train.txt as Suject from train Directory/folder

ActivityLabels <- read.table("./activity_labels.txt", header = F)   ##Read activity_labels.txt

FeaturesNames <- read.table("./features.txt", header = F)    ##Read features.txt as FeaturesName

FeaturesData <- rbind(TestFeatures, TrainFeatures) ##Combining test and train to produce FeaturesData
SubjectData <- rbind(TestSubject, TrainSubject)    ##SubjectData
ActivityData <- rbind(TestActivity, TrainActivity) ##ActivityData

names(ActivityData) <- "ActivityN"                   ##Renaming colums in ActivityData 
names(ActivityLabels) <- c("ActivityN", "Activity")  ##& ActivityLabels dataframes


Activity <- left_join(ActivityData, ActivityLabels, "ActivityN")[, 2]  ##Get factor of Activity names

names(SubjectData) <- "Subject"     ##Rename SubjectData columns

names(FeaturesData) <- FeaturesNames$V2 ##Rename FeaturesData columns using columns from FeaturesNames

DataSet <- cbind(SubjectData, Activity,FeaturesData) ##Create one large Dataset with only  SubjectData,  Activity,  FeaturesData as its variables


subFeaturesNames <- FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)] ####Create New datasets by extracting
DataNames <- c("Subject", "Activity", as.character(subFeaturesNames))                ## only the measurements on the mean 
DataSet <- subset(DataSet, select=DataNames)                                         ## and standard deviation for each measurement


names(DataSet)<-gsub("^t", "time", names(DataSet))               ##Rename the columns
names(DataSet)<-gsub("^f", "frequency", names(DataSet))          ## of the large dataset 
names(DataSet)<-gsub("Acc", "Accelerometer", names(DataSet))     ##using more descriptive 
names(DataSet)<-gsub("Gyro", "Gyroscope", names(DataSet))        ## activity names
names(DataSet)<-gsub("Mag", "Magnitude", names(DataSet))         ##
names(DataSet)<-gsub("BodyBody", "Body", names(DataSet))         ##

SecondDataSet<-aggregate(. ~Subject + Activity, DataSet, mean)                       ##Create a second, independent tidy data set with the t
SecondDataSet<-SecondDataSet[order(SecondDataSet$Subject,SecondDataSet$Activity),]   ##average of each variable for each activity and each subjec

write.table(SecondDataSet, file = "tidydata.txt",row.name=FALSE)      ##Create .txt file containing the table

