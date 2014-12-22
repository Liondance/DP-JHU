# Notes to graders

The program was built and tested piece by piece.
It has been made pretty modular and simple.
Please follow the comments and instructions in the program below.

Running instructions:
If you wish to test the program on your own, you can download and run it in your computer.
Following the project requirements, the program assumes it is in the same directory containing 
the 'activity_labels.txt' file and the 'test' and 'train' folders from the original dataset 
stored in the UCI repository.

I recommend having at least 12GB (16GB better) of RAM to run the program in a reasonable time.
It takes less than 10 minutres in my 12GB system, requiring a few garbage collection cycles.
Less RAM may get you into a situation where the garbage collector uses too much CPU time.

~~~
#
# Data Processing - Project
#

library(Hmisc)
library(sqldf)

rm(list = ls());

#
# This function capitalizes names in a consistent manner
#
nice.name <- function(name) {
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(name), perl=TRUE);
};

#
# Load activity labels and create our activity data frame for future join
#
raw <- read.table("activity_labels.txt", sep=" ");
act <- data.frame(id=raw$V1, activity=factor(raw$V2, levels=raw$V2));
act$desc <- nice.name(gsub("_", " ", act$activity));

#
# Load feature names, extracting only the ones with 'mean()' and 'std()'
# Use the result to create the 'features' data frame
# which contains the indices and names of the selected features
#
raw <- read.table("features.txt", sep=" ");
findex <- grep("-mean\\(\\)|-std\\(\\)", raw$V2);
fnames=raw$V2[findex];
fnames <- gsub("\\(\\)", "", fnames);
features <- data.frame(index=findex, names=fnames);

TNF <- nrow(raw); # Total Number of Fields
raw <- NULL; # Free some memory now ... (we're going to need it!)

#
# This function performs equivalent data loading and transformations
# from the test and training sets
#
# @param set: 'test' or 'train' depending in the data subset
# @param widths: vector of column widths for the fixed width reader function read.fwf
# @param features: data frame with the indices and names of features to extract 
#
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

widths = rep(FFW, TNF); # vector of fixed widths for the read.fwf data loader

# Load the test set
test  <- HAR.frame( "test", widths, features);

# Load trhe training set
train <- HAR.frame("train", widths, features);

# Finally, let's put both together!
full <- rbind(train, test);

# Prepare summary results
averages <- NULL;

for (s in sort(unique(full$subject))) {
    for (a in sort(unique(full$activity))) {
        data <- full[full$subject == s & full$activity == a, ];
        df <- data.frame(rbind(colMeans(data[3:68]))); # this should be done in a more agnostic way!
        df <- data.frame(subject=s, activity=a, df);
        averages <- rbind(averages, df);
    }
}

# Save result
write.table(averages, file="averages.txt", row.names=F);
~~~
