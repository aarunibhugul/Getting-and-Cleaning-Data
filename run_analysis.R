
#########################################################################################
#########################################################################################
##
run_analysis <- function() {
  
  if (!("reshape2" %in% rownames(installed.packages())) ) {
    stop ("Please install required package: reshape2!\n")
  } 
  
  ## Load the required package: reshape2
  library(reshape2)
  

## ------------Question 1.Merging the training and the test sets to create one data set----------------------##

 
  
# load the data sets and then merge the data sets

  traindata <- read.table("./train/X_train.txt")
  testdata  <- read.table("./test/X_test.txt")
  joindata  <- rbind(traindata, testdata) 
  

  
#------------------read and merge label-data set pair------------------#
  trainlabel <- read.table("./train/y_train.txt")
  testlabel  <- read.table("./test/y_test.txt")
  joinlabel  <- rbind(trainlabel, testlabel)
  

  
#------------- merging subject-data set pair -------------####
  trainsubject <- read.table("./train/subject_train.txt")
  testsubject  <- read.table("./test/subject_test.txt")
  joinsubject  <- rbind(trainsubject, testsubject)
  

  

##---------------Question 2. Extracting the measurements on the mean and standard deviation of each measurement--------##

  
  features <- read.table("features.txt") ##read/load the file

  
  ## locate column names with "-mean()" or "-std()" in any rows in column 2
  meanstdindex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  

  ## only select those columns/measurements on the mean and standard deviation
  ## make a new data frame called joindatanew
  joindatanew <- joindata[, meanstdindex] 
  
  ## cross-check dimensions: (rows, columns)
  dim(joindatanew)  ##(10299,66)
  
  ## put the column names into the new data frame joindatanew
  colnames(joindatanew) <- features[meanstdindex, 2] 
  
  ## remove the bad characters such as "()", "-" in colnames, also lower the case if possible
  ##   in order to avoid any unnecessary errors in later analysis
  ##
  colnames(joindatanew) <- gsub("\\(|\\)", "", colnames(joindatanew)) 
  colnames(joindatanew) <- gsub("-", ".", colnames(joindatanew))
  colnames(joindatanew) <- tolower(colnames(joindatanew))

#-----------Question 3: Using descriptive activity to name the activities in the data set.
  
  cat("\n")
  cat("Step3: Uses descriptive activity names to name the activities in the data set.\n")
  
  ## Firstly, it is necessary to load/read the file containing full activity names
  activity <- read.table("activity_labels.txt")
  
  ## Remove bad characters such as "_" and also lower case in the activity names/row names
  activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
  
  ## Create a new activitylabel vector containing descriptive activity names with
  ## the length of rows of joinlabel[, 1] 
  activitylabel <- activity[joinlabel[, 1], 2]
  
  ## Replace the 'activity numbers' to descriptive activity names in joinlabel data frame
  joinlabel[, 1] <- activitylabel 
  
  ## Give a column name to the column in the joinlabel data frame (one column data frame)
  colnames(joinlabel) <- "activity"
  

#------------Question 4: Appropriately labels the data set with descriptive activity names.

  
  ##  Firstly, give a column name to the column of the joinsubject data frame (one column data frame)
  colnames(joinsubject) <- "subject"
  
  ##  Combine three working dataframes (joinsubject, joinlabel and joindatanew) into one 
  ##  single data frame via command cbind
  
  cleandata <- cbind(joinsubject, joinlabel, joindatanew)
  
  ## Cross-check dimensions: (nrows, ncolumns)
  dim(cleandata) ## (10299    68)
  
  
  ##  Optional: produce an output file that is stored the the above 'merged data set with labels'
  ##            in case it will be useful.
  
  write.table(cleandata, "combinedcleandata.txt")
  

#------------Question 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

  ## Reshape the data: generate skinny data via melt function
  ## Melt the cleadata data set before decasting
  meltdfrm <- melt(cleandata, id=c("activity", "subject"))
  
  ## Cast the melt dataset based on the average of each variable for each activity and each subject
  tidydfrm <- dcast(meltdfrm, activity + subject ~ variable, mean)
  
  ## Create a file containing the tidy data set
  write.table(tidydfrm, "tidy_average_data.txt", row.names = F, col.names= T, sep = "\t")
  
  cat("")
  cat("DONE: a tidy data file has been created in the working directory!\n")
  cat("")
  workdone <- "TRUE"
  
} #The End
