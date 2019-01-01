setwd("C:/r")   ## Set your working directory

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  ##Download your
destFile <- "CourseDataset.zip"                                                                  ## file if
if (!file.exists(destFile)){                                                                     ## it is not
  download.file(URL, destfile = destFile, mode='wb')                                             ## yet downloaded
}

if (!file.exists("./UCI_HAR_Dataset")){             ##unzip your file
  unzip(destFile)                                   ##if not yet unziped
}

setwd("./UCI HAR Dataset")     ##Set your working directory to "UCI HAR Dataset"
                               ##for you to read the "train" and "test" directory/folder 

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


