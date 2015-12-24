# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# # The train and test data is stored in the ../input directory
# train <- read.csv("../input/train.csv",na.strings=c('NA',''), stringsAsFactors=F)
# test  <- read.csv("../input/test.csv",na.strings=c('NA',''), stringsAsFactors=F)

#Extract Cabin Num from Cabin 
all_data$Cabin <- as.character(all_data$Cabin)
all_data$CabinNum<-sapply(all_data$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
all_data$CabinNum[all_data$CabinNum == " "] <- NA
all_data$CabinNum[!is.na(all_data$CabinNum)] <- as.numeric(all_data$CabinNum[!is.na(all_data$CabinNum)])
all_data$CabinPos<-NA

#Categorize 1-50 as Front, 50-100 as Middle, >100 as End
all_data$CabinPos[all_data$CabinNum<50]<-'Front'
all_data$CabinPos[all_data$CabinNum>=50 & all_data$CabinNum<100]<-'Middle'
all_data$CabinPos[all_data$CabinNum>=100]<-'End'

all_data<-all_data[!is.na(all_data$CabinNum),]
train$CabinPos<-factor(train$CabinPos)
library(ggplot2)
ggplot(train,aes(x=CabinNum,fill=factor(Survived)))+geom_density(alpha=0.6)+labs(x='Cabin Number')
plot(aggregate(Survived~CabinPos,train,mean))

