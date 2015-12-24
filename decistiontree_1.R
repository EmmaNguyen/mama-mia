
###### Only run for the first time ! #####
# install.packages("RGtk2")
# install.packages("rpart")
# install.packages("randomForest")
# install.packages("rattle")
# install.package("rpart.plot")
# install.packages("RColorBrewer")
# install.packages("ggplot2")

###### Load library
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(ggplot2)
library(rpart)
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


# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
all_data$Embarked[c(62, 830)] <- "S"

# # Factorize embarkment codes.
# all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# Create a new variable, a.k.a family_size
all_data$family_size <- all_data$SibSp + all_data$Parch + 1

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method = "anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- round(predict(predicted_age, all_data[is.na(all_data$Age),]))

# Split the data back into a train set and a test set
train_new <- all_data[1:891,]
test_new <- all_data[892:1309,]
test_new$Survived <- NULL


# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# Create a new variable, a.k.a family_size
all_data$family_size <- all_data$SibSp + all_data$Parch + 1

# Train set and test set
str(train_new)
str(test_new)

###### Decision tree ######
# train_new and test_new are available in the workspace
str(train_new)
str(test_new)

# Create a new model `my_tree_five`
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family_size, data = train_new, method="class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_five)

# Make your prediction using `my_tree_five` and `test_new`
my_prediction <- predict(my_tree_five, test_new, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
