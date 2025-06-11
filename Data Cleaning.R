#Importing libraries
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

#Analysing data from 2007
year_2007 <- read.csv("C:\\Users\\jaime\\Desktop\\Coursework\\2007.csv.bz2")
head(year_2007,3)
tail(year_2007,3)
dim(year_2007)

#Analysing data from 2006
year_2006 <- read.csv("C:\\Users\\jaime\\Desktop\\Coursework\\2006.csv.bz2")
head(year_2006,3)
tail(year_2006,3)
dim(year_2006)

#Merging data from 2006 and 2007 to form one dataset and then analysing the dataset
dataset <- rbind(year_2006,year_2007)
head(dataset,3)
tail(dataset,3)
dim(dataset)
str(dataset)

#dropping dupicate rows
dataset <- distinct(dataset)

#Checking for null values
colSums(is.na(dataset))

#Dropping CancellationCode column
dataset <- subset(dataset, select = -c(CancellationCode))

#Further analysing columns with null values
max(dataset$DepTime , na.rm=TRUE)
max(dataset$ArrTime, na.rm=TRUE)

dataset <- filter(dataset, DepTime <2400, ArrTime <2400)
max(dataset$DepTime , na.rm=TRUE)
max(dataset$ArrTime, na.rm=TRUE)

#Checking for null values after filtering data
colSums(is.na(dataset))

#Adding a new column TotalDelay
dataset <- mutate(dataset, TotalDelay = (ArrDelay + DepDelay))


#size of the data after cleaning
dim(dataset)

#cleaned data is saved as a csv file
write.csv(dataset,"cleaned_dataset_2006_2007.csv")






