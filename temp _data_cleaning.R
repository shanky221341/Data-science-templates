#template for data_cleaning in R based on the titanic data set 

#install the packages in the start which are missing
list.of.packages <- c("Amelia")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.ma.imperial.ac.uk/")

#read the data
#hange the names as appropriate
train.data<-read.csv("train.csv",na.strings=c("NA" ,""))
# use na.strings for converting the strings value in the data to the na's

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

#frequently use the table() function  to view the statistics

# see the statistics of the Embarked column in the and as well as see the missing values in it. 
print(table(train.data$Embarked, useNA= "always"))

