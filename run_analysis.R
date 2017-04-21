
  
  library(dplyr)
  
  #  1) Read in and merge the training and test datasets
  
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  features <- read.table("UCI HAR Dataset/features.txt")
  
  subject <- rbind(subject_test, subject_train)
  x <- rbind(x_test, x_train)
  y <- rbind(y_test, y_train)
  
  names(subject) <- "subject"
  names(y) <- "activity"
  names(x) <- features[,2]
  
  all <- cbind(subject, y)
  all <- cbind(all, x)
  
  
  #  2) Extracts only the measurements on the 
  #     mean and std for each measurement
  
  activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  
  filter <- grep("mean\\(\\)|std\\(\\)", features[,2])
  sub_features <- features$V2[filter]
  
  sub_features_names <- c(as.character(sub_features), "subject", "activity")
  
  dt <- subset(all, select = sub_features_names)
  
  #  3) Uses decriptive activity names to name the activities in the data set
  #
  #     change to factor, and then map to activity_labels.txt
  
  dt$activity <- factor(dt$activity)
  levels(dt$activity) <- activity_labels[,2]
  
  
  
  #  4) Appropriately labels the data set with descriptive var names
  #   -   time <- t
  #   -   Accelerometer <- Acc
  #   -   body <- BodyBody
  #   ....  using gsub
  
  names(dt) <- gsub("^t", "time", names(dt))
  names(dt) <- gsub("Acc", "Accelerometer", names(dt))
  names(dt) <- gsub("Gyro", "Gyroscope", names(dt))
  names(dt) <- gsub("Mag", "Magnitude", names(dt))
  names(dt) <- gsub("^f", "frequency", names(dt))
  names(dt) <- gsub("BodyBody", "Body", names(dt))
  
  
  #  5) Create dataset with the avg of each variable for each 
  #     activity and each subject
  
  dt2 <- aggregate(. ~subject + activity, dt, mean)
  write.table(dt2, file = "tidydata", row.names = FALSE)
  
  