## read in data sets
activityLabel <- read.table("D:\\Coursera\\GettingCleaningData_JLeek\\UCI HAR Dataset\\activity_labels.txt", colClasses = c("numeric", "character"), col.names = c("ActCode", "Activity"))
features <- read.table("D:\\Coursera\\GettingCleaningData_JLeek\\UCI HAR Dataset\\features.txt", colClasses = "character")[,2]

##1 train data set
train.folder <- "D:\\Coursera\\GettingCleaningData_JLeek\\UCI HAR Dataset\\train"
train.set <- "X_train.txt"
train.label <- "y_train.txt"
train.subj <- "subject_train.txt"
train <- read.table(paste(train.folder, train.set, sep = "\\"), colClasses = "numeric")
dim(train)
names(train) <- features
trainSubj <- read.table(paste(train.folder, train.subj, sep = "\\"), colClasses = "character", col.names = "subj")
unique(trainSubj)
trainLabel <- read.table(paste(train.folder, train.label, sep = "\\"), colClasses = "numeric", col.names = "ActCode")

trainf <- cbind(trainSubj, trainLabel, train)
trainf$SetName <- "train"

##2 test data set
test.folder <- "D:\\Coursera\\GettingCleaningData_JLeek\\UCI HAR Dataset\\test"
test.set <- "X_test.txt"
test.label <- "y_test.txt"
test.subj <- "subject_test.txt"
test <- read.table(paste(test.folder, test.set, sep = "\\"), colClasses = "numeric")
names(test) <- features
testSubj <- read.table(paste(test.folder, test.subj, sep = "\\"), colClasses = "character", col.names = "subj")
testLabel <- read.table(paste(test.folder, test.label, sep = "\\"), colClasses = "numeric", col.names = "ActCode")

testf <- cbind(testSubj, testLabel, test)
testf$SetName <- "test"

##task1: Merges the training and the test sets to create one data set.
## Created data set by joining the two data sets with SetName to distinguish "training" and "test"

OneDataSet <- rbind(trainf, testf)
unique(OneDataSet[order(as.numeric(OneDataSet$subj)),]$subj)

##task2: Extracts only the measurements on the mean and standard deviation for each measurement. 
selectedCols <- grepl("mean\\(\\)|std\\(\\)|subj|actcode|setname", names(OneDataSet), ignore.case = T)
#sum(selectedCols)
ExtractedDataSet <- OneDataSet[,selectedCols]
#str(ExtractedDataSet)

##task3: Uses descriptive activity names to name the activities in the data set
##task4: Appropriately labels the data set with descriptive activity names.
ActLabelledDataSet <- merge(ExtractedDataSet, activityLabel, by = "ActCode", all = T)
#head(ActLabelledDataSet[,c("ActCode", "Activity")])
#tail(ActLabelledDataSet[,c("ActCode", "Activity")])

##task5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
str(ActLabelledDataSet)
TriaxialSignal <- grepl("subj|Activity|t(.*)mean(.*)[XYZ]", names(ActLabelledDataSet))
TriaxialSignalDataSet <- ActLabelledDataSet[,TriaxialSignal]

library(reshape)
a <- melt(TriaxialSignalDataSet, id.var = c("subj", "Activity"), variable_name = "signal", na.rm = T)
a$TriAxialSignal <- factor(paste(substr(as.character(a$signal), 1,8), 
                                 substr(as.character(a$signal), nchar(as.character(a$signal)), nchar(as.character(a$signal))), 
                                 sep = ":"))
b <- cast(a, subj + Activity ~ TriAxialSignal, mean)
fdata <- b[order(as.numeric(b$subj)),]
fdata
write.table(fdata, "fdata.txt", quote = F, row.names = F, sep = ",")
