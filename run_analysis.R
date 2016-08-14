##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Pallavi Karan
## 8/14/2016

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

##The dataset includes the following files:
## =========================================
##  - 'README.txt'
##  - 'features_info.txt': Shows information about the variables used on the feature vector.
##  - 'features.txt': List of all features.
##  - 'activity_labels.txt': Links the class labels with their activity name.
##  - 'train/X_train.txt': Training set.
##  - 'train/y_train.txt': Training labels.
##  - 'test/X_test.txt': Test set.
##  - 'test/y_test.txt': Test labels.
##The following files are available for the train and test data. Their descriptions are equivalent. 

# 1. Merge the training and the test sets to create one data set.

features     = read.table('D:\\UCI HAR Dataset\\features.txt',header=FALSE)
activityL_labels = read.table('D:\\UCI HAR Dataset\\activity_labels.txt',header=FALSE)

X_train       = read.table('D:\\UCI HAR Dataset\\train\\x_train.txt',header=FALSE)
y_train       = read.table('D:\\UCI HAR Dataset\\train\\y_train.txt',header=FALSE)
subject_train = read.table('D:\\UCI HAR Dataset\\train\\subject_train.txt',header=FALSE) 

X_test       = read.table('D:\\UCI HAR Dataset\\test\\X_test.txt',header=FALSE)
y_test       = read.table('D:\\UCI HAR Dataset\\test\\y_test.txt',header=FALSE)
subject_test = read.table('D:\\UCI HAR Dataset\\test\\subject_test.txt',header=FALSE)

# Assigin column names to the data imported above
colnames(activityL_labels)  = c('activityId','activityType')

colnames(X_train)        = features[,2]
colnames(y_train)        = "activityId"
colnames(subject_train)  = "subjectId"

colnames(X_test)        = features[,2] 
colnames(y_test)        = "activityId"
colnames(subject_test)  = "subjectId"

# cCreate the final training set by merging 
train.data = cbind(y_train,subject_train,X_train)

# cCreate the final test set by merging 
test.data = cbind(y_test,subject_test,X_test)

# Combine training and test data to create a final data set
final.data = rbind(train.data,test.data)

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(final.data)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logical.vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logical.vector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset finalData table based on the logicalVector to keep only desired columns
final.data = final.data[logical.vector ==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the final.data set with the acitivity_labels table to include descriptive activity names
final.data = merge(final.data,activityL_labels,by='activityId',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(final.data)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassigning the new descriptive column names to the final.data set
colnames(final.data) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, final.data.Activity.labels  without the activityL_labels column
final.data.Activity.labels  = final.data[,names(final.data) != 'activityL_labels']

# Summarizing the final.data.Activity.labels  table to include just the mean of each variable for each activity and each subject
tidy.data    = aggregate(final.data.Activity.labels [,names(final.data.Activity.labels ) != c('activityId','subjectId')],by=list(activityId=final.data.Activity.labels $activityId,subjectId = final.data.Activity.labels $subjectId),mean)

# Merging the tidyData with activityL_labels to include descriptive acitvity names
tidy.data    = merge(tidy.data,activityL_labels,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidy.data, 'D:\\UCI HAR Dataset\\tidyDataSet.txt',row.names=TRUE,sep='\t')
