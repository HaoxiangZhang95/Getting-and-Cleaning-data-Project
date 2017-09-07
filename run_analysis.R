library(reshape2)

# load files
feature     <- read.table('./features.txt');
feature[,2] <-as.character(feature[,2]);
activity    <- read.table('./activity_labels.txt',header = FALSE);
subtrain    <- read.table('./train/subject_train.txt',header = FALSE);
activity    <- as.character(activity)
train       <- read.table('./train/x_train.txt',header = FALSE);
trainlabel  <- read.table('./train/y_train.txt',header = FALSE);
test        <- read.table('./test/x_test.txt',header = FALSE);
subtest     <- read.table('./test/subject_test.txt',header = FALSE);
testlabel   <- read.table('./test/y_test.txt',header = FALSE);

# step 1: merge train and test
ntrain    <- cbind(subtrain,trainlabel,train);
ntest     <- cbind(subtest,testlabel,test);
colnames(ntest)<-c("Subject","Activity",feature)
colnames(ntrain)<-c("Subject","Activity",feature)
data        <- rbind(ntrain,ntest);

# step 2: extract data about mean and std
B           <-  grep(".*mean.*|.*std.*", feature[,2])# Find features with "mean" or "std" in their names
b           <-  grep(".*Freq.*",feature[,2]) # Find features with "meanFreq" in their names. WE DONT NEED THESE FEATURE. THEY ARE NOT MEAN.
a           <-  setdiff(B,b) # Kill features with "meanFreq"
dataWanted  <-  data[,c(1,2,a+2)] # Extract data we want.
featureWanted<- feature[a,2]

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
colnames(dataWanted)<-c("Subject","Activity",featureWanted)
#Additional Step: narrow the dataframe
dataMelted <- melt(dataWanted, id = c("Subject", "Activity"), measure.vars = featureWanted)
dataMelted$Subject<-as.character(dataMelted$Subject)

# step 5: Obtain dataset for mean
actmean  <- dcast(dataMelted, Activity~ variable, mean)
colnames(actmean)[1]<- "sort"
submean  <- dcast(dataMelted, Subject ~ variable, mean)
colnames(submean)[1]<- "sort"
datamean <- rbind(actmean,submean)
write.table(datamean,"tidydataset.txt",row.names = FALSE)
