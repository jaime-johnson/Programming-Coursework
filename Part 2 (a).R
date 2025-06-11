#Importing the libraries
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)

##Importing the cleaned dataset
df <- read.csv("C:\\Users\\jaime\\Desktop\\Jaime - coursework\\cleaned_dataset_2006_2007.csv")
dim(df)
head(df)

#What are the best times to minimise delays each year?
#Assumption: Time is considered as the CRSDepTime

#Creating Time Slots
df$TimeSlots <- 
df$TimeSlots[df$CRSDepTime <400] <- "12am-4am"
df$TimeSlots[df$CRSDepTime >=400 & df$CRSDepTime < 800] <- "4am-8am"
df$TimeSlots[df$CRSDepTime >=800 & df$CRSDepTime < 1200] <- "8am-12pm"
df$TimeSlots[df$CRSDepTime >=1200 & df$CRSDepTime < 1600] <- "12pm-4pm"
df$TimeSlots[df$CRSDepTime >=1600 & df$CRSDepTime < 2000] <- "4pm-8pm"
df$TimeSlots[df$CRSDepTime >=2000 & df$CRSDepTime <=2359] <- "8pm-12pm"

#Splitting the df by years
year_2006 <- subset(df, Year== '2006' )
year_2007 <- subset(df,Year=='2007')

#finding the average total delay in 2006 for different time slots
year_2006 %>%
  group_by(TimeSlots) %>%
  summarise(AverageTotalDelay = mean(TotalDelay)) %>%
  arrange(AverageTotalDelay) %>%
  mutate(AverageTotalDelay = round(AverageTotalDelay))

#finding the average total delay in 2007 for different time slots
year_2007 %>%
  group_by(TimeSlots) %>%
  summarise(AverageTotalDelay = mean(TotalDelay)) %>%
  arrange(AverageTotalDelay) %>%
  mutate(AverageTotalDelay = round(AverageTotalDelay))


df_summary <- df %>%
  group_by(Year, TimeSlots) %>%
  summarize(AverageTotalDelay = round(mean(TotalDelay))) %>%
  ungroup()

custom_order <- c('12am-4am','4am-8am','8am-12pm','12pm-4pm','4pm-8pm','8pm-12am')

ggplot(data = df_summary) +
  geom_col(mapping = aes(x = TimeSlots , y = AverageTotalDelay , fill = factor(Year)),
           position = "dodge") + 
  ggtitle("Average Total Delay for Different Time Slots") + 
  scale_color_brewer("YlGnBu") +
  labs(fill = "Year") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", size = 1))+
  theme(plot.title = element_text(hjust = 0.5))

#finding the average total delay in 2006 for days of the week
year_2006 %>%
  group_by(DayOfWeek) %>%
  summarise(AverageTotalDelay = mean(TotalDelay)) %>%
  arrange(AverageTotalDelay) %>%
  mutate(AverageTotalDelay = round(AverageTotalDelay))

#finding the average total delay in 2007 for days of the week
year_2007 %>%
  group_by(DayOfWeek) %>%
  summarise(AverageTotalDelay = mean(TotalDelay)) %>%
  arrange(AverageTotalDelay) %>%
  mutate(AverageTotalDelay = round(AverageTotalDelay))


df_summary_dow <- df %>%
  group_by(Year, DayOfWeek) %>%
  summarize(AverageTotalDelay = round(mean(TotalDelay))) %>%
  ungroup()

ggplot(data = df_summary_dow) +
  geom_bar(mapping = aes(x= DayOfWeek , y = AverageTotalDelay , fill = factor(Year)),
           position = "dodge",stat = "identity") + 
  ggtitle("Average Total Delay for Different Time Slots") + 
  scale_fill_manual(values = c("darkorange", "darkgreen"))+
  labs(fill = "Year") +
  coord_flip()+
  theme_classic() +
  theme(panel.grid.major.x = element_line(color = "gray", size = 1))+
  theme(plot.title = element_text(hjust = 0.5))



