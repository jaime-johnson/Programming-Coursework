library(dplyr)
library("ggplot2")

# Importing the initial 2007 dataset
df2007 <- read.csv("C:/Users/jaime/Desktop/Coursework/2007.csv.bz2")

#Checking the balance of the diverted class
diverted_counts <- count(df2007, Diverted)
print(diverted_counts)

#checking for null values
colSums(is.na(df2007))


#dropping unwanted columns from df2007
df2007 <- subset(df2007, select = -c(Year, DepTime, ArrTime, FlightNum, TailNum,ActualElapsedTime, CRSElapsedTime, AirTime,ArrDelay, DepDelay, CancellationCode, Cancelled,UniqueCarrier, Dest, TaxiIn, TaxiOut, NASDelay,SecurityDelay))

#importing the airport dataset
airport_dataset <- read.csv("C:/Users/jaime/Desktop/excel for programming/airports.csv")

#checking for null values
colSums(is.na(airport_dataset))

#dropping unwanted columns from airport_dataset
airport_dataset <- subset(airport_dataset, select = -c(city, state, country, airport))

#assuming that the iata column in the airport dataset represtents the Orgin
airport_dataset <- rename(airport_dataset,Origin=iata)

#inner merging the df2007 and airport_dataset based on the Origin column to form the corr_dataset
corr_dataset <- inner_join(df2007,airport_dataset, by = "Origin")

head(corr_dataset)

Features <- data.frame('Month', 'DayofMonth', 'DayOfWeek', 'CRSDepTime', 'CRSArrTime','Distance','Diverted', 'lat', 'long')

