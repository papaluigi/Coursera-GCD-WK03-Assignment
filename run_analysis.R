#PREREQUISITES : LOAD LIBRARIES
library(data.table)
library(reshape2)

#INITIATE CONSTANTS
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
ds_file <- "Dataset.zip"
wdir <- file.path(getwd())

#LOAD DATA IN LOCAL FILESYSTEM - TO BE DONE ONCE, AND THEN NOT NECESSARY (TIME SAVING)
#Download zip file in working directory
#print(wdir)
#if (!file.exists(wdir)) {
#        dir.create(wdir)
#}
#download.file(url, file.path(wdir, ds_file))


#Extract zip file using RAR under Windows
#ds_file <- file.path(wdir, ds_file)
#exec <- file.path("C:", "Program Files", "WinRAR", "winrar.exe")
#cmd <- paste(paste0("\"", exec, "\""), "x", paste0("\"", ds_file, "\""), paste0("\"", wdir, "\""))
#system(cmd)


#LOAD DATA IN MEMORY
wdir_data <- file.path(wdir, "UCI HAR Dataset")

#Read Subjects
dtSubTrain <- fread(file.path(wdir_data, "train", "subject_train.txt"))
dtSubTest <- fread(file.path(wdir_data, "test", "subject_test.txt"))

#Read Activities
dtActTrain <- fread(file.path(wdir_data, "train", "Y_train.txt"))
dtActTest <- fread(file.path(wdir_data, "test", "Y_test.txt"))

#Read Data
fileToDataTable <- function(f) {
        df <- read.table(f)
        dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(wdir_data, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(wdir_data, "test", "X_test.txt"))

#Read Features
dtFeat <- fread(file.path(wdir_data, "features.txt"))


#Merge Subjects subsets
dtSub <- rbind(dtSubTrain, dtSubTest)
setnames(dtSub, "V1", "Subject_ID")

#Merge Activities subsets
dtAct <- rbind(dtActTrain, dtActTest)
setnames(dtAct, "V1", "Activity_ID")

#Merge Data subsets
dt <- rbind(dtTrain, dtTest)

#Merge Subjects, Activties, and Data in a single dt
#dtSub <- cbind(dtSub, dtAct)
dt_merged <- cbind(dtSub, dtAct, dt)

#Extract mean and standard deviation features IDs from the features dt
dtFeat <- dtFeat[grepl("mean\\(\\)|std\\(\\)", V2)]

#Create new feature ID column to enable matching with Data merged dt
dtFeat$featCode <- dtFeat[,paste("V", V1, sep="")]

#Subset required columns from dataset, based upon naming above
setkey(dt_merged, Subject_ID, Activity_ID)
col_sel <- c(key(dt_merged), dtFeat$featCode)
dt_sel <- dt_merged[,col_sel, with=FALSE]

#Read Activities Labels and rename columns to enbale merge based on Activity_ID
dt_Act_Names <- fread(file.path(wdir_data, "activity_labels.txt"))
setnames(dt_Act_Names, names(dt_Act_Names), c("Activity_ID","Activity_Name"))

#Merge Datasets to include Activty descriptive labels
dt_sel <- merge(dt_sel, dt_Act_Names, by="Activity_ID", all.y=TRUE)
dt_sel <- dt_sel[, !(names(dt_sel) %in% c("Activity_ID")), with=FALSE]
setkey(dt_sel, Subject_ID, Activity_Name)



#Rename features columns with features descriptive labels 
setnames(dt_sel, dtFeat$featCode, dtFeat$V2)

#Reshape dataset, defining ids which will be used for melting
dt_melt <- melt(dt_sel, id=c("Subject_ID", "Activity_Name"))

#Create tidy Dataset, casting previously melt dataset
dt_cast <- dcast(dt_melt, Subject_ID+Activity_Name ~ variable, mean)

#Write tidy dataset to file
write.table(dt_cast, "tidy_dataset.txt", row.name=FALSE)

