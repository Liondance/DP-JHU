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
