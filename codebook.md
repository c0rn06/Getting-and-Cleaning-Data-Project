# The run_analysis.R script does the following:
Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the 
  average of each variable for each activity and each subject.




# Variables:

x_test, y_test, subject_test, x_train, y_train, and subject_train contain the data from the files
subject, x, and y merge the data
all is the data merged together
features contains the names of the x data set
activity_labels contains the factor names for the activity variable
filter is the filter applied to the column names to get only the means or std columns
sub_features is the vector the results in applying the filter
dt is the data table that results from from subsetting with the filter, activity and subject columns
dt2 is the separate data table that contains the averaged that get stored in tidydata.txt


the column names have been modified to be decriptive enough to be self explanitory.
