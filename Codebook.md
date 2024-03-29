{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Courier New;}}
{\*\generator Msftedit 5.41.21.2510;}\viewkind4\uc1\pard\f0\fs22 Step 1\par
\par
4 variables 'base_dir', 'features_dir', 'train_dir' and 'test_dir' are created and they hold the path where the input files are placed.\par
\par
Step 2\par
\par
Getting the features and activities and create a lookup for the activities\par
\par
Step 3\par
\par
Initialize some variables:\par
\par
means_pos\tab : contains the indexes of the mean variables\par
stds_pos   : contains the indexes of the standard deviation variables\par
means_names : contains the names of the mean variables\par
stds_names  : contains the names of the standard deviation variables\par
\par
Step 4\par
\par
Extract the indexes and column names of columns with "mean()" or "std()" in it's name.\par
The indexes are saved in variables "means_pos" and "stds_pos".\par
The column names are save in variables "means_names" and "stds_names".\par
\par
\par
Step 5 \par
\par
Read the data of the test data file ("X_test.txt")and save them in variable test_data.\par
Make a subset of test_data with only the mean values and save them to variable sub_test_means.\par
Make a subset of test_data with only the standard deviation values and save them to variable sub_test_stds.\par
\par
Step 6\par
\par
Read the data of the test activity file ("Y_test.txt")and save them in variable test_activities.\par
Create an extra column with the activity name that belongs to the activitiy id. Rename the columns to "ActivityId' and "Activity".\par
\par
Step 7\par
\par
Read the data of the subjects file ("subject_test.txt") and save them in variable test_subjects. Rename the column to "SubjectId"\par
\par
Step 8\par
\par
Combine the mean dataset and the standard deviation dataset together and save the result in variable tot_test.\par
Add three columns "subjectId, ActivityId, Activity" (content of test_subjects and test_activies) to the resulting data set.\par
\par
Step 9\par
\par
\par
Read the data of the train data file ("X_train.txt")and save them in variable train_data.\par
Make a subset of train_data with only the mean values and save them to variable sub_train_means.\par
Make a subset of train_data with only the standard deviation values and save them to variable sub_train_stds.\par
\par
\par
Step 10 \par
\par
Read the data of the train activity file ("Y_train.txt")and save them in variable train_activities.\par
Create an extra column with the activity name that belongs to the activitiy id. Rename the columns to "ActivityId' and "Activity".\par
\par
\par
Step 11 \par
\par
Read the data of the subjects file ("subject_train.txt") and save them in variable train_subjects. Rename the column to "SubjectId"\par
\par
\par
Step 12\par
\par
Combine the mean dataset and the standard deviation dataset together and save the result in variable tot_train.\par
Add three columns "subjectId, ActivityId, Activity" (content of train_subject and train_activities) to the resulting data set.\par
\par
\par
Step 13\par
\par
Combine tot_train and tot_test and save them to variable tot_data\par
\par
Step 14\par
\par
Initialize two variables ers and header of later use\par
\par
\par
Step 15\par
\par
Average calculations.\par
Loop through all the 30 subjects\par
Within the (subjects) loop, loop through the 6 activities\par
Take the subset of data.\par
Calculate the averages of the subset and save them to variabel avg.\par
Use variable header to hold the SubjectId, ActivityId and Activity of this subset.\par
Add the complete row (varaible avg) of data to variabel ers.\par
\par
Step 16\par
\par
Clean up some variables with a lot of data\par
\par
\par
Step 17\par
\par
Combine the header(=identification of each row) and ers together and save them to variable end_rs\par
\par
Setp 18\par
\par
Generate the result file 'Tidy_data_final.txt'.\par
\par
}
 