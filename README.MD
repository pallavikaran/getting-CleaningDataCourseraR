Getting and Cleaning Data Project
Author: Pallavi Karan

Repo for the submission of the course project for the Johns Hopkins Getting and Cleaning Data course.

Overview

This project serves to demonstrate the collection and cleaning of a tidy data set that can be used for subsequent analysis. A full description of the data used in this project can be found at The UCI Machine Learning Repository

The source data for this project can be found here.

Making Modifications to This Script

1. Download and unzip dataset (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) in a folder and change the URL in run_analysis.R as per the unzipped location on your machine's drive in line 33, 34,36,37,38,40,41 and 42. Also modify line 118 to the location you want to save the cleaned dataset to.
2. Run run_analysis.R

Project Summary

The following is a summary description of the project instructions

You should create one R script called run_analysis.R that does the following. 1. Merges the training and the test sets to create one data set. 2. Extracts only the measurements on the mean and standard deviation for each measurement. 3. Uses descriptive activity names to name the activities in the data set 4. Appropriately labels the data set with descriptive activity names. 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
