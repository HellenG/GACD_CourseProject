#------------------------------------------------------------------------------#
#               Course Project - Getting and Cleaning Data
#------------------------------------------------------------------------------#

#-----------------------------
#Step 1: Downloading zip file
#-----------------------------#
#Download and unzip folder
link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = link, destfile = "datasets.zip")
unzip("datasets.zip")
list.files("UCI HAR Dataset")#See the contents

#--------------------------------
#Step 2: Preparation for merging
#--------------------------------#
#Train datasets
y_train <- read.table("./train/y_train.txt") #Train labels
X_train <- read.table("./train/X_train.txt") #Train set
features <- read.table("features.txt", stringsAsFactors = FALSE)
colnames(X_train) <- features[,2] #Mean and Standard Deviation are col 1:6
subject_train <- read.table("./train/subject_train.txt")
train_group <- data.frame(Subject = subject_train[,1], Activity = y_train[,1],
                          X_train[,1:6])
#Test dataset
y_test <- read.table("./test/y_test.txt")
X_test <- read.table("./test/X_test.txt")
colnames(X_test) <- features[,2]
subject_test <- read.table("./test/subject_test.txt")
test_group <- data.frame(Subject = subject_test[,1], Activity = y_test[,1],
                        X_test[,1:6])
#Subjects by randomization group
Train <- unique(subject_train[,1]) #Total of 21 i.e. 30 * 70%
Test  <- unique(subject_test[,1])  #Total of 9  i.e. 30 * 30%

#-------------------------------------------
# Step 3: Merging the Dataset 
#------------------------------------------#
data <- rbind(train_group, test_group)
#Converting Activity to a factor variable using the lables
activity_labels <- read.table("activity_labels.txt")
data[,2] <- factor(data[,2], levels = 1:6, labels = activity_labels[,2])

#-----------------------------------------------
#Task 5: Generating a tidy dataset
#----------------------------------------------#
s <- unique(data[,1]) #Subjects or observarions
g <- c(rep("Train", 21), rep("Test", 9)) #Randomization group
n <- length(s)
new_df <- data.frame(Subject = s, Group = g)
#Averaging the three Sensor groups(x, y, z) for each activity and subject
#For Activity 1 - Walking
walking_meanX <- rep(0, n)
walking_meanY <- rep(0, n)
walking_meanZ <- rep(0, n)
walking_stdX  <- rep(0, n)
walking_stdY  <- rep(0, n)
walking_stdZ  <- rep(0, n)
#For Activity 2 - Walking-upstairs
walkingU_meanX <- rep(0, n)
walkingU_meanY <- rep(0, n)
walkingU_meanZ <- rep(0, n)
walkingU_stdX  <- rep(0, n)
walkingU_stdY  <- rep(0, n)
walkingU_stdZ  <- rep(0, n)
#For Activity 3 - Walking-downstairs
walkingD_meanX <- rep(0, n)
walkingD_meanY <- rep(0, n)
walkingD_meanZ <- rep(0, n)
walkingD_stdX  <- rep(0, n)
walkingD_stdY  <- rep(0, n)
walkingD_stdZ  <- rep(0, n)
#For Activity 4 - Sitting
Sitting_meanX <- rep(0, n)
Sitting_meanY <- rep(0, n)
Sitting_meanZ <- rep(0, n)
Sitting_stdX  <- rep(0, n)
Sitting_stdY  <- rep(0, n)
Sitting_stdZ  <- rep(0, n)
#For Activity 5 - Standing
Standing_meanX <- rep(0, n)
Standing_meanY <- rep(0, n)
Standing_meanZ <- rep(0, n)
Standing_stdX  <- rep(0, n)
Standing_stdY  <- rep(0, n)
Standing_stdZ  <- rep(0, n)
#For Activity 6 - Laying
Laying_meanX <- rep(0, n)
Laying_meanY <- rep(0, n)
Laying_meanZ <- rep(0, n)
Laying_stdX  <- rep(0, n)
Laying_stdY  <- rep(0, n)
Laying_stdZ  <- rep(0, n)
# Averages for each variable for each subject
Walking_Mean <- rep(0, n)
Walking_Std  <- rep(0, n)
WalkingUpStairs_Mean <- rep(0, n)
WalkingUpStairs_Std <- rep(0, n)
WalkingDownStairs_Mean <- rep(0, n)
WalkingDownStairs_Std <- rep(0, n)
Sitting_Mean <- rep(0, n)
Sitting_Std <- rep(0, n)
Standing_Mean <- rep(0, n)
Standing_Std <- rep(0, n)
Laying_Mean <- rep(0, n)
Laying_Std <- rep(0, n)
for(i in 1:n){
        walking <- subset(data, Subject == i & Activity == "WALKING")
        walking_meanX[i] <- mean(walking[,3])
        walking_meanY[i] <- mean(walking[,4])
        walking_meanZ[i] <- mean(walking[,5])
        walking_stdX[i]  <- mean(walking[,6])
        walking_stdY[i]  <- mean(walking[,7])
        walking_stdZ[i]  <- mean(walking[,8])
        walkingU <- subset(data, Subject == i & Activity == "WALKING_UPSTAIRS")
        walkingU_meanX[i] <- mean(walkingU[,3])
        walkingU_meanY[i] <- mean(walkingU[,4])
        walkingU_meanZ[i] <- mean(walkingU[,5])
        walkingU_stdX[i]  <- mean(walkingU[,6])
        walkingU_stdY[i]  <- mean(walkingU[,7])
        walkingU_stdZ[i]  <- mean(walkingU[,8])
        walkingD <- subset(data, Subject == i & Activity == "WALKING_DOWNSTAIRS")
        walkingD_meanX[i] <- mean(walkingD[,3])
        walkingD_meanY[i] <- mean(walkingD[,4])
        walkingD_meanZ[i] <- mean(walkingD[,5])
        walkingD_stdX[i]  <- mean(walkingD[,6])
        walkingD_stdY[i]  <- mean(walkingD[,7])
        walkingD_stdZ[i]  <- mean(walkingD[,8])
        Sitting <- subset(data, Subject == i & Activity == "SITTING")
        Sitting_meanX[i] <- mean(Sitting[,3])
        Sitting_meanY[i] <- mean(Sitting[,4])
        Sitting_meanZ[i] <- mean(Sitting[,5])
        Sitting_stdX[i]  <- mean(Sitting[,6])
        Sitting_stdY[i]  <- mean(Sitting[,7])
        Sitting_stdZ[i]  <- mean(Sitting[,8])
        Standing <- subset(data, Subject == i & Activity == "STANDING")
        Standing_meanX[i] <- mean(Standing[,3])
        Standing_meanY[i] <- mean(Standing[,4])
        Standing_meanZ[i] <- mean(Standing[,5])
        Standing_stdX[i]  <- mean(Standing[,6])
        Standing_stdY[i]  <- mean(Standing[,7])
        Standing_stdZ[i]  <- mean(Standing[,8])
        Laying <- subset(data, Subject == i & Activity == "LAYING")
        Laying_meanX[i] <- mean(Laying[,3])
        Laying_meanY[i] <- mean(Laying[,4])
        Laying_meanZ[i] <- mean(Laying[,5])
        Laying_stdX[i]  <- mean(Laying[,6])
        Laying_stdY[i]  <- mean(Laying[,7])
        Laying_stdZ[i]  <- mean(Laying[,8])
        #Getting averages of X, Y, Z for each activity's mean and std
        Walking_Mean[i] <- mean(walking_meanX[i], walking_meanY[i], walking_meanZ[i])
        Walking_Std[i]  <- mean(walking_stdX[i],  walking_stdY[i],  walking_stdZ[i]) 
        WalkingUpStairs_Mean[i] <- mean(walkingU_meanX[i], walkingU_meanY[i], walkingU_meanZ[i])
        WalkingUpStairs_Std[i]  <- mean(walkingU_stdX[i],  walkingU_stdY[i], walkingU_stdZ[i])
        WalkingDownStairs_Mean[i] <- mean(walkingD_meanX[i], walkingD_meanY[i], walkingD_meanZ[i])
        WalkingDownStairs_Std[i] <- mean(walkingD_stdX[i], walkingD_stdY[i], walkingD_stdZ[i])
        Sitting_Mean[i] <- mean(Sitting_meanX[i], Sitting_meanY[i], Sitting_meanZ[i])
        Sitting_Std[i] <- mean(Sitting_stdX[i], Sitting_stdY[i], Sitting_stdZ[i])
        Standing_Mean[i] <- mean(Standing_meanX[i], Standing_meanY[i], Standing_meanZ[i])
        Standing_Std[i] <- mean(Standing_stdX[i], Standing_stdY[i], Standing_stdZ[i])
        Laying_Mean[i] <- mean(Laying_meanX[i], Laying_meanY[i], Laying_meanZ[i])
        Laying_Std[i] <- mean(Laying_stdX[i], Laying_stdY[i], Laying_stdZ[i])
}
#A dataframe for the averages
Activity_df <- data.frame(Walking_Mean = Walking_Mean, 
                            Walking_Std = Walking_Std,
                            WalkingUpStairs_Mean = WalkingUpStairs_Mean,
                            WalkingUpStairs_Std = WalkingUpStairs_Std,
                            WalkingDownStairs_Mean = WalkingDownStairs_Mean, 
                            WalkingDownStairs_Std  = WalkingDownStairs_Mean,
                            Sitting_Mean = Sitting_Mean, 
                            Sitting_Std = Sitting_Std, 
                            Standing_Mean = Standing_Mean, 
                            Standing_Std = Standing_Std,
                            Laying_Mean = Laying_Mean, Laying_Std = Laying_Std)

#----------------------------------
#   Final Output - A tidy dataset
#--------------------------------#
tidy_df <- cbind(new_df, Activity_df)
write.table(tidy_df, file = "Tidy_Dataset.txt", sep = "\t", row.names = FALSE)

#********************            END      ***********************************#