###peer-graded assignment week 4###

#download file
  setwd("F:/Coursera - Data Science/Course 3/week 4")
  ifelse(file.exists("Wearable_computing.zip"),
         print("file exists"), 
         download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="Wearable_computing.zip")
         )
  library(utils)
  ifelse(file.exists("UCI HAR Dataset"),
      print("file unzipped"),
      unzip("Wearable_computing.zip")
      )
  
  setwd("F:/Coursera - Data Science/Course 3/week 4/UCI HAR Dataset")
  activity_labels <- read.table("activity_labels.txt")
  features <- read.table("features.txt")
  
  #compose training set
  setwd("F:/Coursera - Data Science/Course 3/week 4/UCI HAR Dataset/train")
  trainingset <- read.table("X_train.txt")
  Y_train <- read.table("Y_train.txt")
  subject_train <- read.table("subject_train.txt") 
  names(trainingset) <- features[,2] #adds features as column names
  trainingset$Y <-Y_train[,1] #adds Y-train as extra column
  trainingset$subjects <-subject_train[,1] #adds subject nrs as extra column
  trainingset <- cbind(trainingset[,c(563,562,1:561)]) #rearange columns to put subject & Y first

  # compose test set
  setwd("F:/Coursera - Data Science/Course 3/week 4/UCI HAR Dataset/test")
  testset <- read.table("X_test.txt")
  Y_test <- read.table("Y_test.txt")
  subject_test <- read.table("subject_test.txt")
  names(testset) <- features[,2] #adds features as column names  
  testset$Y <-Y_test[,1] #adds Y-test as extra column
  testset$subjects <-subject_test[,1] #adds subject nrs as extra column
  testset <- cbind(testset[,c(563,562,1:561)]) #rearange columns to put subject & Y first
  
#merge training and test sets
  dataset <- rbind(testset,trainingset)
  
#extract only mean and stdev measurements
  relevant_variables <- grep("mean\\(|std\\(",names(dataset)) #take columns with mean( or std( in them (avoid including meanFreq)
  relevant_variables <- c(1,2,relevant_variables) #add columns 1&2 since I want to include those as well
  dataset2 <- dataset[,relevant_variables] #keep only columns specified in vector relevant_variables
  
#use descriptive activity names
  #merge dataset2 and activity_labels based on activity code (numbers 1-6)
  dataset3<- merge(dataset2,activity_labels,by.x="Y",by.y="V1",all=TRUE)
  dataset3<- dataset3[,c(2,69,3:68)] #remove the column with activity code (now replaced by activity) and rearange columns
  
#use descriptive variable names
  names(dataset3)[2] <- "activity" #in columnnames, replace 'V2' with 'activity'
  names(dataset3)<- sub("^t","time_",names(dataset3)) #in columnnames, replace prefix 't' with'time_'
  names(dataset3)<- sub("^f","frequencydomainsignal_",names(dataset3))#in columnnames, replace prefix 'f' with 'frequencydomainsignal_'
  names(dataset3)<- sub("\\()","",names(dataset3)) #in columnnames, leave out '()'
  names(dataset3)<- sub("BodyBody","Body",names(dataset3)) #fix the repetition 'BodyBody' in some of the columnnames
  names(dataset3)<- gsub("-","_",names(dataset3)) #substitute '-' with underscore
  dataset3<-dataset3[order(dataset3$subjects, dataset3$activity),] #order rows on subjects followed by activity, which seems natural
  
#create a second, independent tidy data set with the averages per activity and per subject
  library(dplyr)
  dataset3<-group_by_at(dataset3,.vars = c("subjects","activity")) 
  tidydata<-summarize_at(dataset3,.vars = names(dataset3)[3:68],.funs = "mean")
  
 #write dataset to a txt file
  setwd("F:/Coursera - Data Science/Course 3/week 4")
  write.table(tidydata,"tidydata_Course3_week4.txt",row.name=FALSE)
  