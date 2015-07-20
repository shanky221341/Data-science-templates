#template for data_cleaning in R based on the titanic data set 

#install the packages in the start which are missing
list.of.packages <- c("Amelia","stringr","partykit","Formula")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.ma.imperial.ac.uk/")

#set the screen display width 
howWide = 150
options(width = howWide)

#read the data
#change the names as appropriate
# use na.strings for converting the strings value in the data to the na's
train.data<-read.csv("train.csv",na.strings=c("NA" ,""))

#use str() for checking the type and th efirst few value of the data
print(str(train.data))

#Generally some classes are required to be converted into the factor
train.data$Survived<-factor(train.data$Survived)
train.data$Pclass<-factor(train.data$Pclass)

#Check the data structure again
print(str(train.data))

#is.na() function is used to check for which index of the attribute contains the NA

is.na(train.data$Age)
print(sum(is.na(train.data$Age)))

#calculating the percentage of the missing values
print((sum(is.na(train.data$Age)==TRUE)/ length(train.data$Age)))

#function to percentage of the missing values
missing<-function(x){ sum(is.na(x))/length(x)}

#finding missing percentage in all the columns
percentage_missing_all<-sapply(train.data,missing)

#printing the percentage for all  
print(percentage_missing_all)

#Important Use package Amelia to plot the missing values in the data set
require(Amelia)

missmap(train.data,main="Missng Map")

#there is also the interactive GUI AmeliaView()
#AmeliaView()

## Inputting the missing values 

#frequently use the table() function  to view the statistics of the particular column

# see the statistics of the Embarked column in the and as well as see the missing values in it. 
print(table(train.data$Embarked, useNA= "always"))

# by the statistics we can see that we have highest embarkment marked as S then we model the remaining as 'S'
train.data$Embarked[is.na(train.data$Embarked)]<-"S"

#We have to find how many mising values are there as per mr,master doctor etc
#so first convert the name into character type from the factor type 
train.data$Name<-as.character(train.data$Name)

#see the changed structure 
print(str(train.data))

#split the names by space and see their stats
table_words<-table(unlist(strsplit(train.data$Name,"\\s+")))
print(table_words)

#using stringr library macth the titles of people and see how many are missing 
library(stringr)
tb<-cbind(train.data$Age,str_match(train.data$Name,"[a-zA-Z]+\\."))
tb<-table(tb[is.na(tb[,1]),2])
print(tb)

#find the means as per the titles
mean.mr<-mean(subset(train.data,grepl(" Mr\\.",Name)& !is.na(Age),select="Age")[,1])
mean.mrs<-mean(subset(train.data,grepl( "Mrs\\.",Name)&!is.na(Age),select="Age")[,1])
mean.miss<-mean(subset(train.data,grepl( "Miss\\.",Name)&!is.na(Age),select="Age")[,1])
mean.master<-mean(subset(train.data,grepl( "Master\\.",Name)&!is.na(Age),select="Age")[,1])
mean.dr<-mean(subset(train.data,grepl( "Dr\\.",Name)&!is.na(Age),select="Age")[,1])

#assign the missing titles with their respective means
train.data$Age[grepl( "Mr\\.",train.data$Name)&is.na(train.data$Age)]<-mean.mr
train.data$Age[grepl( "Mrs\\.",train.data$Name)&is.na(train.data$Age)]<-mean.mrs
train.data$Age[grepl( "Miss\\.",train.data$Name)&is.na(train.data$Age)]<-mean.miss
train.data$Age[grepl( "Master\\.",train.data$Name)&is.na(train.data$Age)]<-mean.master
train.data$Age[grepl( "Dr\\.",train.data$Name)&is.na(train.data$Age)]<-mean.dr

#plotting  the vairiables 

#plotting passenger class
barplot(table(train.data$Pclass),main="Passenger class",names=c("first","second","third"))

#plotting Survived people statistics
barplot(table(train.data$Survived),main="Passenger Survival",names=c("Perished","Survived"))

#plottting gender data
barplot(table(train.data$Sex),main="Passenger Gender")

#histogram of the age
hist(train.data$Age,main="Passenger Age",xlab="Age")

#plotting the bar plot of the sibling passengers
barplot(table(train.data$SibSp),main="Passenger Siblings")

#plotting the passenger parch distribution
barplot(table(train.data$Parch),main="Passenger Parents and children")

#plotting the histogram of the passenger fair
hist(train.data$Fare,main="Passenger Fare",xlab="Fare")

#plotting the bar plot fot the port of embarkation
barplot(table(train.data$Embarked),main="Port of Embarkation")

#see who survived by sex
counts<-table(train.data$Survived,train.data$Sex)

#bar plot for the passenger survival by sex
barplot(counts,col = c("black","blue"),legend=c("Perished","Survived"),main="Passenger Survival by Sex")

#see who survived by pclass
counts<-table(train.data$Survived,train.data$Pclass)

#bar plot for the passenger survival by pclass
barplot(counts,col = c("black","blue"),legend=c("Perished","Survived"),main="Passenger Survival by Pclass")

#bar plot for the gender composition of each class
counts<-table(train.data$Sex,train.data$Pclass)
barplot(counts,col = c("black","blue"),legend=c("female","male"),main="Gender composition of each Pclass")

#histogram of the passengers survived and  not survived
hist(train.data$Age[train.data$Survived=="0"],col="black",breaks=seq(0,80,by=2),main="passenger age histogram",xlab = "Age")
hist(train.data$Age[train.data$Survived=="1"],col="blue",breaks=seq(0,80,by=2),add=T)

#categorizing people with different ages into different groups such as childeren below 13, youth between 13 and 19
#aduts in 20 to 65 and senior citizens are above 65
train.child<-train.data$Survived[train.data$Age<13]
child_percent_survived<-length(train.child[which(train.child==1)])/length(train.child)

train.youth<-train.data$Survived[train.data$Age >=13 & train.data$Age <25]
youth_percent_survived<-length(train.youth[which(train.youth==1)])/length(train.youth)

train.adult<-train.data$Survived[train.data$Age >=20 & train.data$Age <65]
adult_percent_survived<-length(train.adult[which(train.adult==1)])/length(train.adult)

train.senior<-train.data$Survived[train.data$Age >=65]
senior_percent_survived<- length(train.senior[which(train.senior==1)])/length(train.senior)

#plotting the charts using mosaic function

mosaicplot(train.data$Pclass~train.data$Survived,color=TRUE,main="passenger survival class",xlab="pclass",ylab="survival")

#predicting the passenger survival by using the decission tree

#function to split data into 70 percent train and 30 percent test data
#seed is for reprdducing the same thing(reproducible research)
split.data = function(data, p = 0.7, s = 666){
       set.seed(s)
       index = sample(1:dim(data)[1])
       train = data[index[1:floor(dim(data)[1] * p)], ]
       test = data[index[((ceiling(dim(data)[1] * p)) + 
                             1):dim(data)[1]], ]
       return(list(train = train, test = test))
   } 
#p is the percentage and s is the random seed value
allset<-split.data(train.data,p=0.7)
trainset<-allset$train
testset<-allset$test

require('partykit')
require('Formula')
train.ctree<-ctree(Survived~Pclass+Sex+Age+SibSp+Fare+Parch+Embarked,data=trainset)
train.ctree