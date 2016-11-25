#tidyData.csv

##The Experimental Design

This data used to create this dataset was collected from the UCI Machine Learning Repository.  The abstract outlines the experiment as follows:

"Abstract: Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors"

More information about the experiment can be found here, including a link to a video of the participants:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

##The Variables

1.  SubjectID: int [1:30] Lists the subject by their identification number
2.  Activity: factor w/6 levels Lists one of six possible activities used for the experiment
3. - 81.  num Lists the mean of a particular variable.  The variables chosen to average were any data from the raw data that pertained to means or standard deviations.  Further information about the raw data can be found in features_info.txt, which is directly from the original data set.

##Summary Choices

I chose to summarize the data in this way because this was an assignment for a class, and I was following a specific set of instructions.  This data allows you to see averages for each subject and activity type.

