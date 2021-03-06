# GettingAndCleaningData
Course Project: Getting and Cleaning Data

## Summary
The script `run_analysis.R` takes data generated by the Accelerometer and Gyroscope embedded in a Samsung Galaxy S II and summarizes each of the measurements.  The measurements are summarized by both mean and standard deviation and grouped by both the subject user and the activity.  The device was worn on the subjects waist.

## Code Book
Please see "CODEBOOK.R" in the main repository for the description on all the variables summarized within the `subject` and `activity` variables.

## Source and Background of the raw data
Below is the link to the data set:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Below is some more information on how the raw data was generated:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Units
The 'activity' variable are any one of 6 activities. (walking, walking upstairs, walking downstairs, sitting, standing, laying)
The 'subject' variable are observed anywhere from 1 to 30 for each of the 30 subjects.
Data for all other measurements for this data set were taken by both the phones Accelerometer and Gyroscope.
The acceleration signal from the smartphone accelerometer are in standard gravity units 'g'.
The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.

## How the script works
The script first loads the `dplyr` and `tidyr` libraries.

Seven of the necessary raw data sets are read in.
>"X_train.txt"
>"y_train.txt"
>"X_test.txt"
>"y_test.txt"
>"features.txt"
>"subject_train.txt"
>"subject_test.txt"

The `activity` observations are then changed to use more descriptive language and the `activity` and `subject` observations are then merged with the corresponding measurements.

The training and test data sets are then merged and the 'mean' and 'standard deviation' of the specific measurements are selected.

The measurements are grouped by both activity and subject and measurements averaged to that level.

## Tidy Data Output
The data is then written to a text file named "output.txt" using `write.table()`
