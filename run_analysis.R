## Data Science - Getting and Cleaning Data Cours.
## Leonardo Martin.
## Assignmnet #1.

## The purpose of this project is to demonstrate your ability to collect, work with,
## and clean a data set. The goal is to prepare tidy data that can be used for 
## later analysis. You will be graded by your peers on a series of yes/no questions related
## to the project. You will be required to submit:
## 1) a tidy data set as described below, 
## 2) a link to a Github repository with your script for performing the analysis, and
## 3) a code book that describes the variables, the data, and any transformations or
##    work that you performed to clean up the data called CodeBook.md. 
## 
## You should also include a README.md in the repo with your scripts. 
## This repo explains how all of the scripts work and how they are connected.


require(plyr)

# Directories and files
uci_hard_dir <- "UCI\ HAR\ Dataset"
feature_file <- paste(uci_hard_dir, "/features.txt", sep = "")
activity_labels_file <- paste(uci_hard_dir, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_hard_dir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_hard_dir, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_hard_dir, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_hard_dir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_hard_dir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_hard_dir, "/test/subject_test.txt", sep = "")

# Load raw data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

##################################################################
# Step 1. Merges the training and the test sets to create one data set.
##################################################################

# Binding sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

############################################################################################
# Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

###########################################################################
# Step 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]

##############################################################
# Step 4. Appropriately labels the data set with descriptive names.
##############################################################

# Remove parentheses
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
# Make syntactically valid names
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))
# Make clearer names
names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))

######################################################################################################################
# Step 5. From the data set in step 4, creates a second, independent tidy data set with 
#    the average of each variable for each activity and each subject.
######################################################################################################################

average_by_activity_by_subject = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(average_by_activity_by_subject, file = "average_by_activity_by_subject.txt")
