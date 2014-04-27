### Step 1 set up the paths. Assume folder "UCI HAR Dataset" with data is in the working directory
base_dir<-getwd()
features_dir=paste0(base_dir,"/UCI HAR Dataset")
train_dir=paste0(base_dir,"/UCI HAR Dataset/train")  
test_dir=paste0(base_dir,"/UCI HAR Dataset/test")


### Step 2 getting the list of features, activities and create a lookup for the activities
setwd(features_dir)
features=array(read.table("features.txt")[,2])
activity=read.table("activity_labels.txt")
names(activity)<-c("ActivityId","Activity")
lookup<-activity$Activity
lookup<-as.character(lookup)
names(lookup)<-activity$ActivityId


### Step 3 initialize some variables 
means_pos<-vector()
stds_pos<-vector()
means_names<-vector()
stds_names<-vector()


### Step 4 extract the fieldnames and indexes of the means and standard deviations
for(x in seq(1,length(features))){
  if(any(grep('mean()',features[x],fixed=T))){
    means_pos<-c(means_pos,x)
    means_names<-c(means_names,features[x])
  }else if (any(grep('std()',features[x],fixed=T))){
    stds_pos<-c(stds_pos,x)
    stds_names<-c(stds_names,features[x]) }
}

### Step 5 read the test data
setwd(test_dir)
test_data<-read.table("X_test.txt")
sub_test_means<-test_data[,means_pos]
names(sub_test_means)<-means_names
sub_test_stds<-test_data[,stds_pos]
names(sub_test_stds)<-stds_names


### Step 6 read the test activities and add a column with the names of the activities
test_activities<-read.table("Y_test.txt")
names(test_activities)<-"ActivityId"
test_activities$Activity<-lookup[test_activities$ActivityId]

### Step 7 read the test subjects
test_subjects<-read.table("subject_test.txt")
names(test_subjects)<-"SubjectId"


### Step 8 combine the mean-dataset with the standard deviation dataset and 
### add three columns "subjectId, ActivityId, Activity" to the resulting data set
test_subjects<-cbind(test_subjects,test_activities)
tot_test<-cbind(test_subjects, sub_test_means)
tot_test<-cbind(tot_test, sub_test_stds)


### Step 9 read the training data
setwd(train_dir)
train_data<-read.table("X_train.txt")
sub_train_means<-train_data[,means_pos]
names(sub_train_means)<-means_names
sub_train_stds<-train_data[,stds_pos]
names(sub_train_stds)<-stds_names


### Step 10 read the training activities and add a column with the names of the activities
train_activities<-read.table("Y_train.txt")
names(train_activities)<-"ActivityId"
train_activities$Activity<-lookup[train_activities$ActivityId]


### Step 11 read the training subjects
train_subjects<-read.table("subject_train.txt")
names(train_subjects)<-"SubjectId"



### Step 12 combine the mean-dataset with the standard deviation dataset and 
### add three columns "subjectId, ActivityId, Activity" to the resulting data set
train_subjects<-cbind(train_subjects,train_activities)
tot_train<-cbind(train_subjects, sub_train_means)
tot_train<-cbind(tot_train, sub_train_stds)

### Step 13 combine both test and training data set
tot_data<-rbind(tot_train,tot_test)


### Step 14 prepare to calculate the average(s)
### initialize some variables
ers<-NULL
header<-NULL

### Step 15 loop through 30 test subjects
for (x in sort(unique(tot_data$SubjectId))) {
### loop through the 6 activities  
  for (y in sort(unique(activity[,1]))) {
### get the subset of 1 activity    
    temp_set<-subset(tot_data,tot_data[,c(1)]==x&tot_data[,c(2)]==y)   
### calculate the mean of the subset    
    avg<-apply(temp_set[,4:69],2,mean)
### create a dataframe with the three columns "subjectId, ActivityId, Activity" 
    header<-rbind(header, temp_set[1,1:3])
### add the row to the result data set
    ers<-rbind(ers,avg)
  }
  
}


### Step 16 clean up some memory consuming variables
tot_data<-NULL
test_data<-NULL
temp_set<-NULL
train_data<-NULL
tot_test<-NULL
tot_train<-NULL

### Step 17 put the columns "subjectId, ActivityId, Activity" before the average(s)
end_rs<-cbind(header,ers)



### Step 18 create output file
setwd(base_dir)
write.table(end_rs,'Tidy_data_final.txt', row.names=F)
