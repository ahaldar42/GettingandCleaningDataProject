library("data.table")

################################################################
#1.Merges the training and the test sets to create one data set.
################################################################

#Set working directory to the location of the Dataset
setwd("C:/Users/ahaldar/Desktop/Coursera")
path       <-getwd()
dir_path   <-file.path(path,"UCI HAR Dataset")
files_list <- list.files(dir_path, recursive = TRUE)
files_list

# Read data from files-extract as data frames
Activity_Type <- read.table(file.path(dir_path,"activity_labels.txt"),header=FALSE) 
Features     <- read.table(file.path(dir_path,"features.txt"),header=FALSE)
colnames(Activity_Type)  <- c('ActivityId','ActivityType')
setnames(Features, names(Features), c("FeatureNumber", "FeatureName"))

#Subject  : Each row identifies the subject who performed the activity
#y_TrainAct: Read activity files whether 1..6 ie walking/sleeping etc.
#x_TrainAct: Contains the measurements V1-V561

# Read train set files - extract as data frames
Subject_Train<- read.table(file.path(dir_path,"train","subject_train.txt"))
X_TrainAct   <- read.table(file.path(dir_path,"train", "X_train.txt"))     
Y_TrainAct   <- read.table(file.path(dir_path,"train", "Y_train.txt"))     
# Assign column names to the train data imported above
colnames(Y_TrainAct)     <- "ActivityId"
colnames(Subject_Train)  <- "SubjectId"
colnames(X_TrainAct)     <- Features[,2] 
# Create the final training set by merging (via cbind) yTrain, subjectTrain, and xTrain
Train_Set <- cbind(Y_TrainAct,Subject_Train,X_TrainAct) #ActivityId, SubjectId, V1-V561
dim(Train_Set) # 7352  563

# Read test set files - extract as data frames
Subject_Test<- read.table(file.path(dir_path,"test","subject_test.txt"))
X_TestAct   <- read.table(file.path(dir_path,"test", "X_test.txt"))     
Y_TestAct   <- read.table(file.path(dir_path,"test", "Y_test.txt"))     
# Assign column names to the test data imported above
colnames(Y_TestAct)    <- "ActivityId"
colnames(Subject_Test) <- "SubjectId"
colnames(X_TestAct)    <- Features[,2] 
# Create the final test set by merging (via cbind) xTest, yTest and subjectTest 
Test_Set <- cbind(Y_TestAct,Subject_Test,X_TestAct) #ActivityId, SubjectId, V1-V561
dim(Test_Set) # 2947 563

# Combine training and test data via rbind to create a final data set
Final_Dataset <- rbind(Train_Set,Test_Set) #10299 obs of 563 variables
dim(Final_Dataset) #10299 563

############################################################################################
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
############################################################################################

# Create a logicalVector with TRUE values for Id, Mean & StdDev columns and FALSE for others
Logical_1 <- grepl("-std..|-mean..", Features$FeatureName) & !(grepl("-meanF..", Features$FeatureName))#Don't want meanFreq
Logical_2   <- c(T,T,Logical_1)#Accounting for ActivityId and SubjectId

# Subset finalData table based on the logicalVector to keep only desired columns
Final_Dataset       <- data.table(Final_Dataset)
Final_DataSubSet <- subset(Final_Dataset,,select=Logical_2)
dim(Final_DataSubSet) #10299 68

############################################################################################
# 3. Use descriptive activity names to name the activities in the data set
############################################################################################

# Merge the Final Dataset with the acitivityType table to include descriptive activity names
Final_Dataset <- merge(Final_DataSubSet,Activity_Type,by='ActivityId',all.x=TRUE) #finalData$activityType
dim(Final_Dataset) #10299 69

# Tidying up colnames
colNames  <- colnames(Final_Dataset) 
for (i in 1:length(colNames)) 
{
        colNames[i] <- gsub("\\()","",colNames[i])#getting rid of ()
        colNames[i] <- gsub("-std","StdDev",colNames[i])
        colNames[i] <- gsub("-mean","Mean",colNames[i])
        colNames[i] <- gsub("^(t)","Time",colNames[i]) 
        colNames[i] <- gsub("^(f)","Freq",colNames[i])
        colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
}
colnames(Final_Dataset) <- colNames

#####################################################################################################################
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#####################################################################################################################

# Taking out ActivityType column,as we will average over the rest of teh columns
Logical_3 <- (names(Final_Dataset) != 'ActivityType')
Final_Dataset_1  <- subset(Final_Dataset,,select=Logical_3)

# Summarizing the table to include the mean of each variable for each activity and each subject
Logical_4<-names(Final_Dataset_1) != c('ActivityId','SubjectId')
X <-subset(Final_Dataset_1,,select=Logical_4)

activityId <- Final_Dataset_1$ActivityId
subjectId  <- Final_Dataset_1$SubjectId
#Finally computing the required Tidy Data set
Tidy_Data   <- aggregate(x=X,
                        by=list(activityId,subjectId),
                        FUN=mean)

#Group.1=ActivityId
#Group.2=SubjectId
setnames(Tidy_Data, old=c("Group.1","Group.2"), new=c("ActivityId", "SubjectId"))

#Including the ActivityType column to TidyData 
Tidy_Data    <- merge(Activity_Type,Tidy_Data,by='ActivityId',all.x=TRUE)
dim(Tidy_Data) #180 69

# Export the tidyData set with row.names=FALSE as per instructions
write.table(Tidy_Data, './TidyData.txt',row.names=FALSE,sep='\t')
