library(reshape2)
library(dplyr)
# load files
feature     <- read.table('./features.txt');
feature     <-as.character(feature[,2]);
activity    <- read.table('./activity_labels.txt',header = FALSE);
subtrain    <- read.table('./train/subject_train.txt',header = FALSE);
activity    <- as.character(activity)
train       <- read.table('./train/x_train.txt',header = FALSE);
trainlabel  <- read.table('./train/y_train.txt',header = FALSE);
test        <- read.table('./test/x_test.txt',header = FALSE);
subtest     <- read.table('./test/subject_test.txt',header = FALSE);
testlabel   <- read.table('./test/y_test.txt',header = FALSE);
train       <-tbl_df(train)
test        <-tbl_df(test)
subtrain    <-tbl_df(subtrain)
subtest     <-tbl_df(subtest)
trainlabel  <-tbl_df(trainlabel)
testlabel   <-tbl_df(testlabel)
train2       <-mutate(train,group = "train")
test2        <-mutate(test,group = "test")
# step 1: merge train and test
colnames(train2)[1:length(colnames(train2))-1]<-feature;
colnames(test2)[1:length(colnames(test2))-1] <-feature;
colnames(subtrain)<-"Subject";
colnames(subtest)<-"Subject";
colnames(trainlabel)<-"Activity";
colnames(testlabel)<-"Activity";
ntrain      <- cbind(subtrain,trainlabel,train2);
ntest       <- cbind(subtest,testlabel,test2);
data        <- rbind(ntrain,ntest);

# step 2: extract data about mean and std
B           <-  grep(".*mean.*|.*std.*", feature)# Find features with "mean" or "std" in their names
b           <-  grep(".*Freq.*",feature) # Find features with "meanFreq" in their names. WE DONT NEED THESE FEATURE. THEY ARE NOT MEAN.
a           <-  setdiff(B,b) # Kill features with "meanFreq"
dataWanted  <-  data[,c(1,2,a+2,length(colnames(data)))] # Extract data we want.
featureWanted<- feature[a]
dataWanted    <- arrange(dataWanted,Subject,Activity);

# step 3: rename the activity names
dataWanted$Activity   <-gsub("1","Walking",dataWanted$Activity)
dataWanted$Activity   <-gsub("2","WalkingUp",dataWanted$Activity)
dataWanted$Activity   <-gsub("3","WalkingDown",dataWanted$Activity)
dataWanted$Activity   <-gsub("4","Sitting",dataWanted$Activity)
dataWanted$Activity   <-gsub("5","Standing",dataWanted$Activity)
dataWanted$Activity   <-gsub("6","Laying",dataWanted$Activity)

# step 4: rename the variables using descriptive activity names
featureWanted<- gsub("[()]","",featureWanted)#Kill ()
featureWanted<- gsub("-mean","Mean",featureWanted)
featureWanted<- gsub("-std","StdDev",featureWanted)
featureWanted<- gsub("^(t)","Time",featureWanted)
featureWanted<- gsub("^(f)","Freq",featureWanted)
featureWanted<- gsub("AccMag","AccMagnitude",featureWanted)
featureWanted<- gsub("JerkMag","JerkMagnitude",featureWanted)
featureWanted<- gsub("GyroMag","GyroMagnitude",featureWanted)
colnames(dataWanted)<-c("Subject","Activity",featureWanted,"group")
#Additional Step: narrow the dataframe
dataMelted <- melt(dataWanted, id = c("Subject", "Activity"), measure.vars = featureWanted)

# step 5: Obtain dataset for mean
actmean  <- dcast(dataMelted, Activity~ variable, mean)
colnames(actmean)[1]<- "sort"
submean  <- dcast(dataMelted, Subject ~ variable, mean)
colnames(submean)[1]<- "sort"
datamean <- rbind(actmean,submean)
write.table(datamean,"tidydataset.txt",row.names = FALSE)
