## Run analysis of the UCI HAR dataset
## Please refer to the accompanying ReadMe file for an explanation of
## assumptions, rationale and approach in responding to each question
## in the course project.
  
  olddir <- getwd()
  setwd("/Users/larsplougmann/datasciencecoursera/Get and Clean")
  
  ## Download and unpack the dataset
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, destfile="./UCI.zip", method="curl")
  unzip("./UCI.zip")
  file.remove("./UCI.zip")
  
  ## create tables in R corresponding to the data files
  ## first, the training set
  setwd("./UCI HAR Dataset/train")
  subject_train <- read.table("subject_train.txt", as.is=TRUE, header=FALSE)
  y_train <- read.table("y_train.txt", as.is=TRUE, header=FALSE)
  x_train <- read.table("x_train.txt", as.is=TRUE, header=FALSE)
  ## then the the test set
  setwd("../test")
  subject_test <- read.table("subject_test.txt", as.is=TRUE, header=FALSE)
  y_test <- read.table("y_test.txt", as.is=TRUE, header=FALSE)
  x_test <- read.table("x_test.txt", as.is=TRUE, header=FALSE)
  ## also grab the lists of activities and variable definitions
  setwd("../")
  activity_labels <- read.table("activity_labels.txt", header=FALSE)
  features <- read.table("features.txt", header=FALSE, as.is=TRUE)
  setwd(olddir)
  
  ## Merge the training and the test sets to create one data set 
  ## (Q1 of the course project).
  ## To prepare, assign variable names to the vectors
  names(subject_train) <- "subject"
  names(subject_test) <- "subject"
  names(y_train) <- "activity"
  names(y_test) <- "activity"
  names(x_train) <- features[,2]
  names(x_test) <- features[,2]
  ## First combine the training and test sets into a single table each
  training_set <- cbind(subject_train,y_train,x_train)
  test_set <- cbind(subject_test,y_test,x_test)
  ## then lob the two tables together to produce a complete data set
  complete <- rbind(training_set,test_set)
  
  ## Extract only the measurements on the mean and standard deviation for each measurement. 
  ## (Q2 of the course project).
  ## Assumption: mean and standard deviation measurement are the ones with mean() or std()
  ## in the feature name
  meanindex <- grep("mean()", names(complete))
  stdindex <- grep("std()", names(complete))
  cols_of_interest <- sort(c(meanindex,stdindex))
  ## Combine colums 1 and 2 (with subject and activity) together with the columns that
  ## contain mean and std measurements.
  extract <- cbind(complete[,1:2],complete[,cols_of_interest])  
  
  ## Use descriptive activity names to name the activities in the data set
  ## (Q3 of the course project).
  library(dplyr)        ## Operations require the dplyr library
  ## The descriptive activity names are the ones in the activity_labels data table.
  ## First, convert the relevant columns to factors
  extract$activity <- factor(extract$activity, levels = 1:6)
  activity_labels[,2] <- factor(activity_labels[,2])
  ## then change the contents of the activity column by using a lookup into 
  ## the activity labels table.
  extract <- mutate(extract, activity = activity_labels[extract$activity,2])
  
  ## Introduce descriptive variable names
  ## (Q4 of the course project).
  ## Descriptive variable names are interpreted as using the names in features.txt
  ## but made R compliant by getting rid of parentheses and substituting
  ## underscores for hyphens.
  colnames(extract) <- gsub("-","_",colnames(extract))
  colnames(extract) <- gsub("\\(","",colnames(extract))
  colnames(extract) <- gsub("\\)","",colnames(extract))
  
  ## Create an independent tidy dataset with the average of each variable 
  ## for each activity and each subject
  ## (Q5 of the course project).
  ## Need tidyr library
  library(tidyr)
  ## First, convert all the measurement columns to indicators of which type of
  ## measurement was performed and store it in a measurement column.
  tidy <- extract %>% gather(measurement,value,tBodyAcc_mean_X:fBodyBodyGyroJerkMag_meanFreq)
  ## since we need to calculate averages for subjects, activities and measurements, 
  ## group by all three to produce 14220 indices
  tidy <- group_by(tidy,subject,activity,measurement)
  ## calculate averages for each combination of subject, activity and measurement
  tidy <- mutate(tidy,averages = mean(value))
  ## drop the value column to retain the 14220 averages only and filter for unique rows only
  tidy <- unique(select(tidy,-value))
  ## this produces the independent tidy data set.
  
  ## According to instructions, save the independent tidy data set to a text file using write.table
  write.table(tidy, file="tidy.txt", row.name=FALSE)
  
  
  
