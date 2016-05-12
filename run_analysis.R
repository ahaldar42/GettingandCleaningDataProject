library("data.table")

################################################################################
#1.Merges the training and the test sets to create one data set.
################################################################################

#Set working directory to the location of the Dataset
setwd("C:/Users/ahaldar/Desktop/Coursera")
path       <-getwd()
dir_path   <-file.path(path,"UCI HAR Dataset")
files_list <- list.files(dir_path, recursive = TRUE)
files_list

# Read in the data from files
activityType <- read.table(file.path(dir_path,"activity_labels.txt"),header=FALSE) #imports activity_labels.txt
features     <- read.table(file.path(dir_path,"features.txt"),header=FALSE)#imports features.txt
colnames(activityType)  <- c('ActivityId','ActivityType')

#Subject  : Each row identifies the subject who performed the activity
#yTrainAct: Read activity files whether 1..6 ie walking/sleeping etc.
#xTrainAct: Contains the measurements V1-V561

# Read in the train data from files
SubjectTrain<- read.table(file.path(dir_path,"train","subject_train.txt"))#reads subject_train.txt
xTrainAct   <- read.table(file.path(dir_path,"train", "X_train.txt"))     #reads x_train.txt
yTrainAct   <- read.table(file.path(dir_path,"train", "Y_train.txt"))     #reads y_train.txt

# Assign column names to the train data imported above
colnames(yTrainAct)     <- "ActivityId"
colnames(SubjectTrain)  <- "SubjectId"
colnames(xTrainAct)     <- features[,2] 


# Create the final training set by merging (via cbind) yTrain, subjectTrain, and xTrain
TrainData <- cbind(yTrainAct,SubjectTrain,xTrainAct) #ActivityId, SubjectId, V1-V561


# Read in the test data from files
SubjectTest<- read.table(file.path(dir_path,"test","subject_test.txt"))#reads subject_test.txt
xTestAct   <- read.table(file.path(dir_path,"test", "X_test.txt"))     #reads x_test.txt
yTestAct   <- read.table(file.path(dir_path,"test", "Y_test.txt"))     #reads y_test.txt

# Assign column names to the test data imported above
colnames(yTestAct)    <- "ActivityId"
colnames(SubjectTest) <- "SubjectId"
colnames(xTestAct)    <- features[,2] 


# Create the final test set by merging (via cbind) xTest, yTest and subjectTest 
TestData <- cbind(yTestAct,SubjectTest,xTestAct) #ActivityId, SubjectId, V1-V561

# Combine training and test data via rbind to create a final data set
FinalData <- rbind(TrainData,TestData) #10299 obs of 563 variables


#############################################################################################
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
#############################################################################################

setnames(features, names(features), c("FeatureNum", "FeatureName"))

# Create a logicalVector with TRUE values for Id, Mean & StdDev columns and FALSE for others
LogicalVector1 <- grepl("-std..|-mean..", features$FeatureName) & !(grepl("-meanF..", features$FeatureName))#Don't want meanFreq
LogicalVector   <- c(T,T,LogicalVector1)#Accounting for ActivityId and SubjectId

# Subset finalData table based on the logicalVector to keep only desired columns
FinalData       <- data.table(FinalData)
FinalDataSubSet <- subset(FinalData,,select=LogicalVector)

################################################################################
# 3. Use descriptive activity names to name the activities in the data set
################################################################################

# Merge the finalData set with the acitivityType table to include descriptive activity names
FinalData <- merge(FinalDataSubSet,activityType,by='ActivityId',all.x=TRUE) #finalData$activityType

# Updating the colNames vector to include the new column names after merge
colNames  <- colnames(FinalData) 

# Cleaning up the variable names acc to Tidy Data guidelines 
for (i in 1:length(colNames)) 
{
        colNames[i] <- gsub("\\()","",colNames[i])#getting rid of ()
        colNames[i] <- gsub("-std","StdDev",colNames[i])
        colNames[i] <- gsub("-mean","Mean",colNames[i])
        colNames[i] <- gsub("^(t)","Time",colNames[i]) #time to Time
        colNames[i] <- gsub("^(f)","Freq",colNames[i]) #freq to Freq
        colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
}
# Renaming the columns  
colnames(FinalData) <- colNames

######################################################################################################################
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
######################################################################################################################

# Create a new table "FinalDataNoActivityType" w/o ActivityType column, as we will avg the rest
LogicalVector2           <- (names(FinalData) != 'ActivityType')
FinalDataNoActivityType  <- subset(FinalData,,select=LogicalVector2)

# Summarizing the table to include the mean of each variable for each activity and each subject
LogicalVector4<-names(FinalDataNoActivityType) != c('ActivityId','SubjectId')
X <-subset(FinalDataNoActivityType,,select=LogicalVector4)

activityId <- FinalDataNoActivityType$ActivityId
subjectId  <- FinalDataNoActivityType$SubjectId
TidyData   <- aggregate(x=X,
                        by=list(activityId,subjectId),
                        FUN=mean)

#Group.1=ActivityId
#Group.2=SubjectId
setnames(TidyData, old=c("Group.1","Group.2"), new=c("ActivityId", "SubjectId"))

# Combining the activityType col to TidyData 
TidyData    <- merge(activityType,TidyData,by='ActivityId',all.x=TRUE)

# Export the tidyData set 
write.table(TidyData, './TidyData.txt',row.names=FALSE,sep='\t')
