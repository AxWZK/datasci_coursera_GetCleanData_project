# README for run_analysis.R
NOTE: The code itself is reasonably well documented Many of the details in this README file can be found within the code itself.

## The Setup
The first part of the script Define files, libraries, and working directories Fetching my raw materials.

## Part 1. Merges the training and the test sets to create one data set. ---
Build up our initial 'Big Data' sets, on each for TEST and TRAIN. The data sets are organized with the `SUBJECT_ID` in column 1, the `ACTIVITY` in column 2, and the measurements in all subsequent columns. The data columns are assigned DESCRIPTIVE NAMES per Step 4 of the project, based on the ables outlined in `features.txt`

    ###--- Test Data
    colnames(dat.test.X) <- c(dat.features$V2) ## This line solves step 4.
                                               ## See line 99 for more details
                                              
Once a test & training version are created, a simple `rbind()` is used to combine them.

## #2. Extracts only the measurements on the mean and standard deviation for each measurement.
Define index called `keepColumns` to extract only mean() and std() data and maintain the first two columns with subject ID and activity class then apply it to the big data set and assign that to a temp dataset variable. Then use this `keepColumns` index to extract data we want

    dat.thin <- dat.all.big[keepColumns] # dat.thin becomes our thinned out dataset

## 3. Uses descriptive activity names to name the activities in the data set
Perfectly good descriptive activity names are provided in the `activity_lables.txt` file. I am using them to change the `int` values in `dat.thin$ACTIVITY` to match the descriptive terms provided.

    ##-- FOR loop to make all the changes
    for(items in dat.activity$V1){
      dat.thin$ACTIVITY[self==items]=dat.activity$V2[items]
  
## 4. Appropriately labels the data set with descriptive variable names. 
Perfectly good variable descriptors were supplied in the `features.txt` file. I simply applied those to the TEST & TRAIN data sets before combining. See details in section 1 of this document or See lines 51 & 62 of `run_analysis.R`

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Statred by creating a dataframe with the unique `SUBJECT_ID` and `ACTIVITY` pairings in a nice "smallest to largest" order. From there I struggled for a bit trying to nail down a clean and efficient way of filling the rest of the tidy dataset with the requested info. I ran out of time so ended up opting for brute force FOR loops. Its crude but it worked and at 1:30am I was ready t be done.

