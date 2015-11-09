#--Filename: run_analysis.R
#--Purpose: Merges the training and the test sets to create one data set.
#	Extracts only the measurements on the mean and standard deviation for each measurement. 
#	Uses descriptive activity names to name the activities in the data set
#	Appropriately labels the data set with descriptive variable names. 
#	From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
library(plyr)
library(data.table)

#function: getTestFile gets test file
getTestFile<- function(filename, header=FALSE){
	url<-paste("./UCI HAR Dataset/test/", filename ,sep="/")
	data<- read.table(url,header)
}
#function: getTrainFile gets train file
getTrainFile<- function(filename, header=FALSE){
  url<-paste("./UCI HAR Dataset/train/", filename ,sep="/")
	data<- read.table(url,header)
}

#function: run_analysis is the main function
run_analysis <- function(){
  #--Step1: Merges the training and the test sets to create one data set.
  # test data
	xtest <- getTestFile("X_test.txt")
	ytest <- getTestFile("y_test.txt")
	subjecttest <- getTestFile("subject_test.txt")

	#train data
	xtrain <- getTrainFile("X_train.txt")
	ytrain <- getTrainFile("y_train.txt")
	subjecttrain <- getTrainFile("subject_train.txt")

	#merged data as one set
	#append rows of x values, y values, subject values
	x_data <- rbind(xtrain, xtest)
	y_data <- rbind(ytrain, ytest)
	subject_data <- rbind(subjecttrain, subjecttest)
	
	#--- Step2: Extract only the measurements on the mean and standard deviation for each measurement
	
	#find ids of mean and std from features
	features <- read.table("./UCI HAR Dataset/features.txt")
	mean_and_std_features_ids <- grep("-(mean|std)\\(\\)", features[, 2])
	
	#append ids to x columns
	x_data <- x_data[, mean_and_std_features_ids]
	names(x_data) <- features[mean_and_std_features_ids, 2]
	
	#-- Step 3:Use descriptive activity names to name the activities in the data set
	
	activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
	#find names in y_data by activities ids, replace the names
	y_data[, 1] <- activities[y_data[, 1], 2]
	#change the y_data column name
	names(y_data) <- "activity"
	
	#-- Step 4:Appropriately label the data set with descriptive variable names
	
	#set subject column name
	names(subject_data) <- "subject"
	
	#build a single data set
	combined_data <- cbind(y_data , subject_data, x_data)

	#sort by activity and subject
	attach(combined_data)
	combined_data_sorted <- combined_data[order(activity,subject),]
	detach(combined_data)

	#-- Step 5:Create a second, independent tidy data set with the average of each variable for each activity and each subject
	
	dataTable <- data.table(combined_data_sorted)
	tidy_data <-dataTable[,lapply(.SD,mean),by="activity,subject"]
	write.table(tidy_data, "tidy_data.txt", sep=";", row.names = FALSE)
	
}
