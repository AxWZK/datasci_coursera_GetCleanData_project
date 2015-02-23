#------ created by AxWZK
#------ 10-FEB-2015
#------
#------ Coursera Get & Clean Data course project -----
#
#------ run_analysis.R 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.
#------

#------- The Setup -------
##-- Define files, libraries, and working directories
library(dplyr)
setwd("/home/axwzk314/R/datasciencecoursera/GetAndCleanData")
if (!file.exists("data")) { dir.create("data")}

##--- Fetching my raw materials
projectdataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(projectdataURL, 
              destfile="Project_Dataset.zip", 
              method="curl")

unzip("Project_Dataset.zip", exdir="./data")

##--Reading Data such that it is useable
dat.features <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
dat.activity <- read.table("./data/UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)

###--- Test data set
dat.test.X   <- read.table("./data/UCI HAR Dataset/test/X_test.txt", fill=TRUE)
dat.test.y   <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
dat.test.sub <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

###--- Training data set
dat.train.X   <- read.table("./data/UCI HAR Dataset/train/X_train.txt", fill=TRUE)
dat.train.y   <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
dat.train.sub <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
#------

#------1. Merges the training and the test sets to create one data set. ---
##-- Build up our initial 'Big Data' sets, on each for TEST and TRAIN
##-- The data sets are organized with the Subject ID in column 1, 
##-- The activity class in column 2, and the measurements in all
##-- subsequent columns

###--- Test Data
colnames(dat.test.X) <- c(dat.features$V2) ## This line solves step 4.
                                           ## See line 99 for more details

dat.test.sub <- rename(dat.test.sub, SUBJECT_ID = V1)
dat.test.y   <- rename(dat.test.y, ACTIVITY = V1)

dat.test.big <- cbind(dat.test.sub, 
                      dat.test.y, 
                      dat.test.X)

###--- Train Data
colnames(dat.train.X) <- c(dat.features$V2) ## This line solves step 4.
                                            ## See line 99 for more details

dat.train.sub <- rename(dat.train.sub, SUBJECT_ID = V1)
dat.train.y   <- rename(dat.train.y, ACTIVITY = V1)

dat.train.big <- cbind(dat.train.sub, 
                       dat.train.y, 
                       dat.train.X)

# --- Final assembly of big massive data set of awesomeness
dat.all.big <- rbind(dat.test.big, 
                     dat.train.big)

#------ 2. Extracts only the measurements on the mean and standard deviation 
#------    for each measurement.
##-- Define index to extract only mean() and std() data and maintain the
##-- first two columns with subject ID and activity class then apply it to the big 
##-- data set and assign that to a temp dataset variable

keepColumns <- suppressWarnings(grepl("mean()",
                                      names(dat.all.big)))
keepColumns[suppressWarnings(grepl("std()",
                                   names(dat.all.big)))]=TRUE
keepColumns[1:2] <- TRUE

##-- Use index to extract data we want
dat.thin <- dat.all.big[keepColumns]

#------ 3. Uses descriptive activity names to name the activities in the data set
##-- Perfectly good descriptive activity names are provided in the activity_lables.txt
##-- file. I am using them to change the int values in column$ACTIVITY to match the 
##-- descriptive terms provided
self <- dat.temp$ACTIVITY # a little variable assignment I sometimes use to make downstream
                          # code clearer to rear

##-- FOR loop to make all the changes
for(items in dat.activity$V1){
  dat.thin$ACTIVITY[self==items]=dat.activity$V2[items]
}

#------ 4. Appropriately labels the data set with descriptive variable names. 
##-- See lines 51 & 62. Perfectly good variable descriptors were supplied in the 
##-- features.txt file. I simply applied those to the TEST & TRAIN data sets before combining

#------ 5. From the data set in step 4, creates a second, independent tidy data set with the 
#------    average of each variable for each activity and each subject.

###--- dataframe with the SUBJECT ID and ACTIVITY in a nice "smallest to largest" order
###--- and yes, I know this code is painfuly inefficient. I was short on time and had
###--- opt for brute force
dat.tidy <- distinct(select(dat.thin, 
                            SUBJECT_ID, 
                            ACTIVITY))
dat.tidy <- arrange(dat.tidy, 
                    SUBJECT_ID, 
                    ACTIVITY)

###--- going to use the old names as they are still perfectly good descriptors
lon <- names(dat.thin)[-(1:2)]

###--- First FOR loop that goes through each line in the tidy data set
for(j in 1:nrow(dat.tidy)){
  # set up some variables to make things at least a little
  # easier to read later on
  tsub = dat.tidy$SUBJECT_ID[j]
  tact = dat.tidy$ACTIVITY[j]
  
  # the secondary FOR loop where each element is more or less
  # calculated individually.  Yep, slow and clumsy, but it works
  for(items in 1:length(lon)){
    dat.tidy[j,lon[items]]<-mean(filter(dat.thin, 
                                        SUBJECT_ID==tsub, 
                                        ACTIVITY==tact)[,lon[items]])
  }
}
###--- Spitting out my answer
colnames(dat.tidy)<-make.names(names(dat.tidy))
write.table(dat.tidy, file="tidy_data.txt", row.name=FALSE)
