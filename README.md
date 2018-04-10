## Data Science Specialization course series: Getting and Cleaning Data Course Project
>
>### Aim
>
>The purpose of this project is to demonstrate course students' ability to collect,
>work with, and clean a data set. The goal is to prepare tidy data that can be used
>for later analysis. Students will be graded by their peers on a series of yes/no questions
>related to the project. Students will be required to submit:
>
>(1) a tidy data set as described below, 
>
>(2) a link to a Github repository with your script for performing the analysis, 
>
>(3) a code book that describes the variables, the data, and any transformations 
>or work that you performed to clean up the data called CodeBook.md. Students should also
>include a README.md in the repo with your scripts. This repo explains how all of the 
>scripts work and how they are connected.  
>
>------------------------------------------------------------------------------------
>### Project Data Resouces
>
>One of the most exciting areas in all of data science right now is wearable computing
>- see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing
>to develop the most advanced algorithms to attract new users. The data linked to from
>the course website represent data collected from the accelerometers from the Samsung
>Galaxy S smartphone. A full description is available at the site where the data was
>obtained: 
>
>http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
>
>Here are the data for the project: 
>
>https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
>
>-------------------------------------------------------------------------------------
>### Instructions
>
>Students should create one R script called run_analysis.R that does the following. 
>
>* Merges the training and the test sets to create one data set.
>
>* Extracts only the measurements on the mean and standard deviation for each measurement. 
>
>* Uses descriptive activity names to name the activities in the data set.
>  Appropriately labels the data set with descriptive activity names. 
>  Creates a second, independent tidy data set with the average of each variable
>  for each activity and each subject. 
>
>Please upload a tidy data set according to the instructions in the project description.
>Please upload your data set as a separate file (do not cut and paste a dataset directly
>into the text box, as this may cause errors saving your submission). 
>
>Please submit a link to a Github repo with the code for performing your analysis.
>The code should have a file run_analysis.R in the main directory that can be run 
>as long as the Samsung data is in your working directory. The output should be the 
>tidy data set you submitted for part 1. You should include a README.md in the repo 
>describing how the script works. 
>

=======================================================================================
### Aim: R script to reproduce the tidy data set

### Obtain the raw data sets and put them in the current working directory

     
   + download the raw data from the following website:
      https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
      
   + using the following R command to download the data:
   
              >setInternet2(TRUE)   
              >url_proj <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
              >download.file(urlproj, destfile="Dataset.zip", mode="wb")
      
   + unzip the raw data sets
      
              >unzip("Dataset.zip")
      

        
### Create a tidy_data	file via a R script named run_analysis.R
      
 * Preparation:  data sets and script
    + The data sets and run_analysis.R must be in the working directory.
           It is based on one of the requirements this project: The code should have 
	   a file run_analysis.R in the main directory that can be run as long as the Samsung
	   data is in your working directory. (see "Instructions and Requirements")
      
     + The input raw data for the run_analys.R are:
        
               ./train/X_train.txt, ./train/y_train.txt, subject_train.txt;
               ./test/X_test.txt, ./test/y_test.txt,  subject_test.txt;
               ./activity_labels.txt, ./features.txt
        
     + The output tidy data created from run_analysis.R are:
     
               ./tidy_average_data.txt (180 rows)
               ./combinedcleaningdata.txt (optional)
        
* Working of  run_analysis.R works via Rstudio
    + usage:
    
               > source("run_analysis.R") ## load the script
               > run_analysis() ##run the script
       
    + There are 5 main steps in run_analysis.R to process the raw data sets and create the tidy data set.
    
  
         Step 1: Merging the training and the test data frame to create one data frame :
              Load and read the input sets; merge 3 pairs data set (e.g.: X_train.txt, X_test.txt;
              y_train.txt, y_test.txt; subject_train.txt, subject_text.txt) into three single data set
              using "rbind" function.
            
         Setp 2: Extracting the measurements of the mean and standard deviation for each measurement.
              This step is mainly done by the "grep" function by providing the key search patterns
              "-mean\\(\\)|-std\\(\\)".
          
               
         Step-3: Uses descriptive activity names to name the activities in the data set.
              This step is mainly to produce a one-column data frame called "joinlabel" containing
              descriptive activity names.
            
         Step-4: Appropriately labels the data frame with descriptive activity names.
              The step is to combine three joined data frames into one data frame called
              "cleandata". 
              
            
         Step-5: Creates a second, independent tidy data set with the average of each variable
              for each activity and each subject.
              

            
===================================================================================================
