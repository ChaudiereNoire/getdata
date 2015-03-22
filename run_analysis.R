run_analysis <- function(BuildRawFile = FALSE ) {
  ## Merges the training and the test sets to create one data set.
  ##
  ## Extracts only the measurements on the mean and standard deviation for each measurement.
  ##
  ## Uses descriptive activity names to name the activities in the data set
  ##
  ## Appropriately labels the data set with descriptive variable names. 
  ##
  ## From the data set in step 4, creates a second, independent tidy data set 
  ##    with the average of each variable for each activity and each subject.
  ##
  ##For each record in the dataset is provided: 
  ## - Triaxial acceleration from the accelerometer (total acceleration) and 
  ##    the estimated body acceleration. 
  ## - Triaxial Angular velocity from the gyroscope. 
  ## - A 561-feature vector with time and frequency domain variables. 
  ## - Its activity label. 
  ## - An identifier of the subject who carried out the experiment.
  ##
  ## Citation: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
  ## A Public Domain Dataset for Human Activity Recognition Using Smartphones. 
  ## 21th European Symposium on Artificial Neural Networks, Computational Intelligence 
  ## and Machine Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013.
  
  library(data.table)
  
  ## set file and directory locations
  dirPath <- paste(getwd(), "/UCI HAR Dataset/", sep = "")
  trainPath <- paste( getwd(), "/UCI HAR Dataset/train/", sep = "")
  testPath <- paste( getwd(), "/UCI HAR Dataset/test/", sep = "")
  
  ##  Shows information about the variables used on the feature vector.
  featureInfoFile <- paste( dirPath, "features_info.txt", sep = "" )

  ## List of all features.
  featureFile <- paste(dirPath, "features.txt", sep = "")

  ## Links the class labels with their activity name.
  activityLabelsFile <- paste(dirPath, "activity_labels.txt", sep = "")
  
  ## Training set.
  trainSetFile <- paste(trainPath, "X_train.txt", sep = "" )

  ## Training labels.
  trainLabelsFile <- paste(trainPath, "y_train.txt", sep = "")

  ## Test set.
  testSetFile <- paste(testPath, "X_test.txt", sep = "" )
  
  ## Test labels.
  testLabelsFile <- paste(testPath, "y_test.txt", sep = "")

  ## Each row identifies the subject who performed the activity for 
  ## each window sample. Its range is from 1 to 30. 
  trainSubjectFile <- paste(trainPath, "subject_train.txt", sep = "" ) 
  testSubjectFile <- paste(testPath, "subject_test.txt", sep = "" ) 
  
  ##  The signal data from the smart phones.  Each row in the data represents 2.56 seconds
  ##  of data, with readings made 50 times each second.
  
  ## The acceleration signal from the smartphone accelerometer X axis in 
  ## standard gravity units 'g'. Every row shows a 128 element vector. 
  trainTotalAccXFile <- paste(trainPath, "Inertial Signals/total_acc_x_train.txt", sep = "" ) 
  trainTotalAccYFile <- paste(trainPath, "Inertial Signals/total_acc_y_train.txt", sep = "" ) 
  trainTotalAccZFile <- paste(trainPath, "Inertial Signals/total_acc_z_train.txt", sep = "" ) 
  testTotalAccXFile <- paste(testPath, "Inertial Signals/total_acc_x_test.txt", sep = "" ) 
  testTotalAccYFile <- paste(testPath, "Inertial Signals/total_acc_y_test.txt", sep = "" ) 
  testTotalAccZFile <- paste(testPath, "Inertial Signals/total_acc_z_test.txt", sep = "" ) 
  
  ##- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal
  ## obtained by subtracting the gravity from the total acceleration. 
  trainBodyAccXFile <- paste(trainPath, "Inertial Signals/body_acc_x_train.txt", sep = "" ) 
  trainBodyAccYFile <- paste(trainPath, "Inertial Signals/body_acc_y_train.txt", sep = "" ) 
  trainBodyAccZFile <- paste(trainPath, "Inertial Signals/body_acc_z_train.txt", sep = "" ) 
  testBodyAccXFile <- paste(testPath, "Inertial Signals/body_acc_x_test.txt", sep = "" ) 
  testBodyAccYFile <- paste(testPath, "Inertial Signals/body_acc_y_test.txt", sep = "" ) 
  testBodyAccZFile <- paste(testPath, "Inertial Signals/body_acc_z_test.txt", sep = "" ) 

  ##  The angular velocity vector measured by the gyroscope for each window sample. 
  ##  The units are radians/second. 
  trainBodyGyroXFile <- paste(trainPath, "Inertial Signals/body_gyro_x_train.txt", sep = "" ) 
  trainBodyGyroYFile <- paste(trainPath, "Inertial Signals/body_gyro_y_train.txt", sep = "" ) 
  trainBodyGyroZFile <- paste(trainPath, "Inertial Signals/body_gyro_z_train.txt", sep = "" ) 
  testBodyGyroXFile <- paste(testPath, "Inertial Signals/body_gyro_x_test.txt", sep = "" ) 
  testBodyGyroYFile <- paste(testPath, "Inertial Signals/body_gyro_y_test.txt", sep = "" ) 
  testBodyGyroZFile <- paste(testPath, "Inertial Signals/body_gyro_z_test.txt", sep = "" ) 
  
  ## Read Inertial Signal data into memory
  trainTotalAccX <- data.table(read.table(trainTotalAccXFile))
  trainTotalAccY <- data.table(read.table(trainTotalAccYFile))
  trainTotalAccZ <- data.table(read.table(trainTotalAccZFile))
  testTotalAccX <- data.table(read.table(testTotalAccXFile))
  testTotalAccY <- data.table(read.table(testTotalAccYFile))
  testTotalAccZ <- data.table(read.table(testTotalAccZFile))
  trainBodyAccX <- data.table(read.table(trainBodyAccXFile))
  trainBodyAccY <- data.table(read.table(trainBodyAccYFile))
  trainBodyAccZ <- data.table(read.table(trainBodyAccZFile))
  testBodyAccX <- data.table(read.table(testBodyAccXFile))
  testBodyAccY <- data.table(read.table(testBodyAccYFile))
  testBodyAccZ <- data.table(read.table(testBodyAccZFile))
  trainBodyGyroX <- data.table(read.table(trainBodyGyroXFile))
  trainBodyGyroY <- data.table(read.table(trainBodyGyroYFile))
  trainBodyGyroZ <- data.table(read.table(trainBodyGyroZFile))
  testBodyGyroX <- data.table(read.table(testBodyGyroXFile))
  testBodyGyroY <- data.table(read.table(testBodyGyroYFile))
  testBodyGyroZ <- data.table(read.table(testBodyGyroZFile))
  
  ## Read labels into memory
  trainLabel1 <- data.frame(read.table(trainLabelsFile))
  testLabel1 <- data.frame(read.table(testLabelsFile))
  
  ## Read activity labels into memory
  activityLabels <- data.frame(read.table(activityLabelsFile))
  
  ## Merge labels to tidy up values
  trainLabels <- merge(trainLabel1,activityLabels,by="V1")
  setnames(trainLabels,"V1","label")
  setnames(trainLabels,"V2","activity")
  testLabels <- merge(testLabel1,activityLabels,by="V1")
  setnames(testLabels,"V1","label")
  setnames(testLabels,"V2",'activity')
    
  ## Read subjects into memory and set column name to "subject"
  trainSubjects <- data.table(read.table(trainSubjectFile))
  setnames(trainSubjects,"V1","subject")
  testSubjects <- data.table(read.table(testSubjectFile))
  setnames(testSubjects,"V1","subject")
  
  ## Read data sets into memory
  trainSet <- data.frame(read.table(trainSetFile))
  testSet <- data.frame(read.table(testSetFile))
    
  ## Read features into memory and create pretty names
  feature <- data.frame(read.table(featureFile))
  feature$V2 <- sub(")","",feature$V2)
  feature$V2 <- sub("\\(","",feature$V2)
  feature$V2 <- sub("\\,","-",feature$V2)
  
  columnNumbers <- feature[grep(("mean|std"), feature[1:561,]$V2),]$V1
  columnNames <- data.frame(feature[grep(("mean|std"), feature[1:561,]$V2),])
  
  
  ## Combine test and training data sets
  
  ## First, get the requested columns of data 
  testColumnNames <- colnames(testSet)
  chosenColumns <- testColumnNames[columnNumbers]
  testData1 <- testSet[,chosenColumns]
  colnames(testData1) <- columnNames$V2
  trainData1 <- trainSet[,chosenColumns]
  colnames(trainData1) <- columnNames$V2
  
  ##  Next, bind the identifiers
  testData <- cbind(testSubjects,testLabels,testData1)
  trainData <- cbind(trainSubjects,trainLabels,trainData1)
  
  ## Finally, merge the two sets and remove label column
  cleanData <- rbind(testData,trainData)
  cleanData$label <- NULL
  
  ## Write out resulting data set
  write.table(cleanData,"getdata-project-data",row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
  
  ## Create a new table averaging by subect and activity
  independentTidySet <- ddply(cleanData, c("subject", "activity"), colwise(mean))
  write.table(independentTidySet,"getdata-project-tidydata",row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
  
  ## The followind code is not part of the project assignment.  I was using it for some timing
  ## experiments.

  
  if ( BuildRawFile ) {
    ## Merge the sensor reads into one file, a data matrix of 128 rows 
    ## for each row of original sensor values.  Include subject and
    ## activity labels.
    mergedData <- matrix(, nrow = (nrow(trainSubject) * 128) + (nrow(testSubject) * 128), ncol = 13)
    colnames(mergedData) <- c("Index", "Division","Subject","Activity",
                    "TotalAccX","TotalAccY","TotalAccZ",
                    "BodyAccX","BodyAccY","BodyAccZ",
                    "BodyGyroX","BodyGyroY","BodyGyroZ")
  

    for (i in 1:nrow(trainSubject))  {
      for (j in 1:128) {
          k <- ((i - 1) * 128) + j
          mergedData[k,1] <- k 
          mergedData[k,2] <- "Train" 
          mergedData[k,3] <- trainSubject[i,1] 
          mergedData[k,4] <- trainLabels[i,1] 
          mergedData[k,5] <- trainTotalAccX[i,j] 
          mergedData[k,6] <- trainTotalAccY[i,j] 
          mergedData[k,7] <- trainTotalAccZ[i,j] 
          mergedData[k,8] <- trainBodyAccX[i,j] 
          mergedData[k,9] <- trainBodyAccY[i,j] 
          mergedData[k,10] <- trainBodyAccZ[i,j] 
          mergedData[k,11] <- trainBodyGyroX[i,j] 
          mergedData[k,12] <- trainBodyGyroY[i,j] 
          mergedData[k,13] <- trainBodyGyroZ[i,j] 
      }
      lastTrain <- k    
    }
    for (i in 1:nrow(testSubject))  {
      for (j in 1:128) {
        k <- ((i - 1) * 128) + j + lastTrain
        mergedData[k,1] <- k 
        mergedData[k,2] <- "Test" 
        mergedData[k,3] <- testSubject[i,1] 
        mergedData[k,4] <- testLabels[i,1] 
        mergedData[k,5] <- testTotalAccX[i,j] 
        mergedData[k,6] <- testTotalAccY[i,j] 
        mergedData[k,7] <- testTotalAccZ[i,j] 
        mergedData[k,8] <- testBodyAccX[i,j] 
        mergedData[k,9] <- testBodyAccY[i,j] 
        mergedData[k,10] <- testBodyAccZ[i,j] 
        mergedData[k,11] <- testBodyGyroX[i,j] 
        mergedData[k,12] <- testBodyGyroY[i,j] 
        mergedData[k,13] <- testBodyGyroZ[i,j] 
      }
      lastTest <- k    
    }
    write.csv(mrgData, file = "mrgData.csv",row.names = FALSE )
  }
}
