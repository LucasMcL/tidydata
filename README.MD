#This document explains the basic structure of run_analysis.R

##Loading packages and downloading files
* This script primarily uses plyr and dplyr, so they must be loaded
* Package file is downloaded and unzipped using basic commands

##Reading in labels and unformatted data
* This script uses the read.table command to read in the label information
* It also uses the same command to read in the unformatted data

##Adding ID column and merging
* This script uses cbind to add the subject info, titled SubjectID as the first column of both data sets
* It then merges them and names the rest of the columns based on the features data

##Adding Activity Info
* This step is not very efficient.  Perhaps these data could have been added before the merge.
* It successfully creates another column in the merged dataset with the activity names (as factors)
* This goes into the second column

##Indexing
* This step uses the grep command to search for "mean()" and "std()" within column names.
* It then subsets the data based on that index.

##Creating the tidy dataset
* Lastly, this script uses powerful dplyr commands to summarise the data.
* Using group_by, it groups the data by SubjectID and Activity
* It then uses the summarise_each command to calculate the means accross all columns for SubjectID and Activity
