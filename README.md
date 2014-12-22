# Notes to graders

Follow comments for the program here ...

~~~
#
# Data Processing - Project
#

library(Hmisc)
library(sqldf)

rm(list = ls());

nice.name <- function(name) {
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(name), perl=TRUE);
};

raw <- read.table("activity_labels.txt", sep=" ");
act <- data.frame(id=raw$V1, activity=factor(raw$V2, levels=raw$V2));
act$desc <- nice.name(gsub("_", " ", act$activity));

raw <- read.table("features.txt", sep=" ");
findex <- grep("-mean\\(\\)|-std\\(\\)", raw$V2);
fnames=raw$V2[findex];
fnames <- gsub("\\(\\)", "", fnames);
features <- data.frame(index=findex, names=fnames);

TNF <- nrow(raw); # Total Number of Fields
raw <- NULL; # Free some memory ... (we're going to need it!)

HAR.frame <- function(set, widths, features) {

    # Compute file names for the given set (i.e. test vs train)
    s.file <- gsub("<set>", set, "<set>/subject_<set>.txt");
    y.file <- gsub("<set>", set, "<set>/y_<set>.txt");
    X.file <- gsub("<set>", set, "<set>/X_<set>.txt");
    
    # Read the subject file
    subject <- read.table(s.file, sep=" ");

    # Read the y (activity) file
    y  <- read.table(y.file, sep=" ");

    # Read X (measures) file and extract only the features wanted
    X <- read.fwf(X.file, widths=widths);
    X <- X[, features$index];

    # Set proper column names
    names(subject) <- c("subject");
    names(y) <- c("activity");
    names(X) <- features$names;

    # Construct and return data frame
    data.frame(subject, y, X)
}

FFW <- 16; # fixed field width for all fields

widths = rep(FFW, TNF);

test  <- HAR.frame( "test", widths, features);
train <- HAR.frame("train", widths, features);

# Finally, let's put this together!
full <- rbind(train, test);

# Prepare summary results
averages <- NULL;

for (s in sort(unique(full$subject))) {
    for (a in sort(unique(full$activity))) {
        data <- full[full$subject == s & full$activity == a, ];
        df <- data.frame(rbind(colMeans(data[3:68])));
        df <- data.frame(subject=s, activity=a, df);
        averages <- rbind(averages, df);
    }
}

# Save result
write.table(averages, file="averages.txt", row.names=F);
~~~
## Dataset: Human Activity Recognition Using Smartphones Dataset

This dataset is a transformation of the one found in the UCI machine learning repository
[here] (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

This dataset contains exactly 180 rows (aside from the header), one for each possible combination of
the 30 subjects and 6 activity types in the original study. Besides the columns identifying the
subject and activity, only the averages of the measurements from the original dataset corresponding 
to means and standard deviations are included.

All feature names are of the form:

<prefix><short>-<stat>-<optional axis>

where:

* <prefix> is 't' for series of measurements derived from time series data, and 'f' for measurements derived via Fourier transforms
* <short> is a short descriptive mnemonic for the feature (measurement)
* <stat> is 'mean' or 'std', corresponding to the mean or standard deviation of the original series
* <optional axis> for axial data only, is 'X', 'Y', or 'Z' corresponding to the axis

Original description
```
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
```

## Complete List of columns

The data set has 68 columns: two identify the subject and activity, the remaining 66 correspond to the selected features
Each row is uniquely identified by the first two columns: i.e. the combination subject/activity acts as a "primary key"

* Subject
* activity
* tBodyAcc-mean-X
* tBodyAcc-mean-Y
* tBodyAcc-mean-Z
* tBodyAcc-std-X
* tBodyAcc-std-Y
* tBodyAcc-std-Z
* tGravityAcc-mean-X
* tGravityAcc-mean-Y
* tGravityAcc-mean-Z
* tGravityAcc-std-X
* tGravityAcc-std-Y
* tGravityAcc-std-Z
* tBodyAccJerk-mean-X
* tBodyAccJerk-mean-Y
* tBodyAccJerk-mean-Z
* tBodyAccJerk-std-X
* tBodyAccJerk-std-Y
* tBodyAccJerk-std-Z
* tBodyGyro-mean-X
* tBodyGyro-mean-Y
* tBodyGyro-mean-Z
* tBodyGyro-std-X
* tBodyGyro-std-Y
* tBodyGyro-std-Z
* tBodyGyroJerk-mean-X
* tBodyGyroJerk-mean-Y
* tBodyGyroJerk-mean-Z
* tBodyGyroJerk-std-X
* tBodyGyroJerk-std-Y
* tBodyGyroJerk-std-Z
* tBodyAccMag-mean
* tBodyAccMag-std
* tGravityAccMag-mean
* tGravityAccMag-std
* tBodyAccJerkMag-mean
* tBodyAccJerkMag-std
* tBodyGyroMag-mean
* tBodyGyroMag-std
* tBodyGyroJerkMag-mean
* tBodyGyroJerkMag-std
* fBodyAcc-mean-X
* fBodyAcc-mean-Y
* fBodyAcc-mean-Z
* fBodyAcc-std-X
* fBodyAcc-std-Y
* fBodyAcc-std-Z
* fBodyAccJerk-mean-X
* fBodyAccJerk-mean-Y
* fBodyAccJerk-mean-Z
* fBodyAccJerk-std-X
* fBodyAccJerk-std-Y
* fBodyAccJerk-std-Z
* fBodyGyro-mean-X
* fBodyGyro-mean-Y
* fBodyGyro-mean-Z
* fBodyGyro-std-X
* fBodyGyro-std-Y
* fBodyGyro-std-Z
* fBodyAccMag-mean
* fBodyAccMag-std
* fBodyBodyAccJerkMag-mean
* fBodyBodyAccJerkMag-std
* fBodyBodyGyroMag-mean
* fBodyBodyGyroMag-std
* fBodyBodyGyroJerkMag-mean
* fBodyBodyGyroJerkMag-std
