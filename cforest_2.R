###### Only run for the first time ! #####
# install.packages("rpart")
# install.packages("randomForest")
# install.packages("rattle")
# install.package("rpart.plot")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
# install.packages("party")
###### Load library
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(rpart)
library(party)
######### Read data ##########
# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

##### Complete, add data #####
# All data, both training and test set
test$Survived<-NA
all_data = rbind(train,test);

# create a new variable, a.k.a title
# Engineered variable: Title
all_data$Name <- as.character(all_data$Name)
strsplit(all_data$Name[1], split='[,.]')
strsplit(all_data$Name[1], split='[,.]')[[1]]
strsplit(all_data$Name[1], split='[,.]')[[1]][2]
all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
all_data$Title <- sub(' ', '', all_data$Title)
all_data$Title[all_data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
all_data$Title[all_data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
all_data$Title[all_data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
all_data$Title <- factor(all_data$Title)

# Convert to a factor
all_data$Title <- factor(all_data$Title)


#Extract Cabin Num from Cabin 
all_data$Cabin <- as.character(all_data$Cabin)
all_data$CabinNum<-sapply(all_data$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
all_data$CabinNum[all_data$CabinNum == " "] <- NA
all_data$CabinNum[!is.na(all_data$CabinNum)] <- as.numeric(all_data$CabinNum[!is.na(all_data$CabinNum)])


#Categorize 1-50 as Front, 50-100 as Middle, >100 as End
all_data$CabinPos[all_data$CabinNum<50]<-'Front'
all_data$CabinPos[all_data$CabinNum>=50 & all_data$CabinNum<100]<-'Middle'
all_data$CabinPos[all_data$CabinNum>=100]<-'End'

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
all_data$Embarked[c(62, 830)] <- "S"

# # Factorize embarkment codes.
# all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# Create a new variable, a.k.a FamilySize
all_data$FamilySize <- all_data$SibSp + all_data$Parch + 1

# Engineered variable: Family
all_data$Surname <- sapply(all_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
all_data$FamilyID <- paste(as.character(all_data$FamilySize), all_data$Surname, sep="")
all_data$FamilyID[all_data$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(all_data$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
all_data$FamilyID[all_data$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
all_data$FamilyID <- factor(all_data$FamilyID)
all_data$FamilyID2 <- all_data$FamilyID
all_data$FamilyID2 <- as.character(all_data$FamilyID2)
all_data$FamilyID2[all_data$FamilySize <= 3] <- 'Small'
all_data$FamilyID2 <- factor(all_data$FamilyID2)

# Fill the age by method = "anova" since you are predicting a continuous variable.
summary(all_data$Age)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data= all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- round(predict(predicted_age, all_data[is.na(all_data$Age),]))

# Split the data back into a train set and a test set
train_new <- all_data[1:891,]
test_new <- all_data[892:1309,]
test_new$Survived <- NULL

# Train set and test set
str(train_new)
str(test_new)

######### Conditional inference for Random forest ##############
# Set seed for reproducibility
set.seed(210)

# Apply the Condition inference of Random Forest Algorithm
my_forest <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
                     data = train_new, controls=cforest_unbiased(ntree=2000, mtry=3)) 

# Make your prediction using the test set
my_prediction <- predict(my_forest, test_new, OOB=TRUE, type = "response")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
