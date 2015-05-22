run_analysis <- function() {
  ## By Judit Pagès Vives
  
  ## Reading the name of the columns
  dtHead <- read.csv("UCI HAR Dataset/features.txt", header = FALSE, colClasses = "character", sep=" ")  
  vHead <- c(dtHead[,2])
  

  
  ########################################################################################
  ####  Load Test Data
  ########################################################################################  
  
  dtSub <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names=1, colClasses = "character")  
  colnames(dtSub)[1] <-"IdSubject"
  dtAct <- read.table("UCI HAR Dataset/test/Y_test.txt", header = FALSE, col.names=1, colClasses = "character")    
  colnames(dtAct)[1] <-"IdActivity"
  
  
  dtIni <- read.csv("UCI HAR Dataset/test/X_test.txt", header = FALSE, colClasses = "character")  
  dtMeasure <-strsplit(gsub('  ',' ',as.character(dtIni[,1])),' ')
  dtMeasureEnd <-do.call("rbind", dtMeasure)  
  
  
  ## Remove the first column because it has blanks
  dtMeasureEnd <- subset( dtMeasureEnd, select = c(2:562) )
  dtMeasureEnd[,1:561] <- sapply(dtMeasureEnd[,1:561], as.numeric)
  colnames(dtMeasureEnd)[1:561] <- vHead
  
  
  ## Merge the values of subject_test.txt,Y_test.txt and X_test.txt 
  dtTest <- merge(dtSub, dtAct, by=0, all=FALSE, sort=FALSE)
  dtTest <- merge(dtTest[,2:3], dtMeasureEnd, by=0, all=FALSE, sort=FALSE)
  

  ########################################################################################
  ####  Load Train Data
  ########################################################################################

  dtSubTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names=1, colClasses = "character")  
  colnames(dtSubTrain)[1] <-"IdSubject"
  dtActTrain <- read.table("UCI HAR Dataset/train/Y_train.txt", header = FALSE, col.names=1, colClasses = "character")  
  colnames(dtActTrain)[1] <-"IdActivity"


  dtIniTrain <- read.csv("UCI HAR Dataset/train/X_train.txt", header = FALSE, colClasses = "character")  
  dtMeasureTrain <-strsplit(gsub('  ',' ',as.character(dtIniTrain[,1])),' ')
  dtMeasureEndTrain <-do.call("rbind", dtMeasureTrain)  

  
  ## Remove the first column because it has blanks
  dtMeasureEndTrain <- subset( dtMeasureEndTrain, select = c(2:562) )
  dtMeasureEndTrain[,1:561] <- sapply(dtMeasureEndTrain[,1:561], as.numeric)
  colnames(dtMeasureEndTrain)[1:561] <- vHead


  ## Merge the values of subject_train.txt,Y_train.txt and X_train.txt 
  dtTrain <- merge(dtSubTrain, dtActTrain, by=0, all=FALSE, sort=FALSE)
  dtTrain <- merge(dtTrain[,2:3], dtMeasureEndTrain, by=0, all=FALSE, sort=FALSE)



  ## Add the two data Sets
  dtAll<-rbind(dtTest,dtTrain)
  
  
  ## Searching the colums of mean and std
  listColumns <- c("IdSubject","IdActivity")
  listColumns<- append (listColumns, vHead[grep("mean",vHead)])
  listColumns<- append (listColumns, vHead[grep("std",vHead)])
  
  
  dtAllSubset <- dtAll [,listColumns]
  dtAllSubset[,3:81] <- sapply(dtAllSubset[,3:81], as.numeric)
  
  ## Aggregating data by IdSubject, IdActivity
  dtRes<-aggregate(dtAllSubset[,3:81], dtAllSubset[,1:2],FUN=mean, na.rm=TRUE)  

  
  ## Adding descriptive names for Activities
  dtRes[dtRes$IdActivity=="1",2]<-"WALKING"
  dtRes[dtRes$IdActivity=="2",2]<-"WALKING_UPSTAIRS"
  dtRes[dtRes$IdActivity=="3",2]<-"WALKING_DOWNSTAIRS"
  dtRes[dtRes$IdActivity=="4",2]<-"SITTING"
  dtRes[dtRes$IdActivity=="5",2]<-"STANDING"
  dtRes[dtRes$IdActivity=="6",2]<-"LAYING"
  
  dtRes
  
}
