#Importing Libraries
library(dplyr)
library(ggplot2)

#Loading the cleaned dataset
df <- read.csv("C:\\Users\\jaime\\Desktop\\Jaime - coursework\\cleaned_dataset_2006_2007.csv")
dim(df)
head(df)

#Importing plane data
plane_data <- read.csv("C:\\Users\\jaime\\Desktop\\excel for programming\\plane-data.csv", na.strings = c("", "NA"))
dim(plane_data)
head(plane_data)
colSums(is.na(plane_data))

#Extracting only the necessary columns from   plane_data
plane_data_filtered <- select(plane_data,tailnum,year)

#Renaming Columns extracted from the plane_data
plane_data_filtered <- rename(plane_data_filtered, TailNum = tailnum, YearOfManufacture = year)
head(plane_data_filtered)

#Extracting only the necessary columns from df
df_filtered <- select (df, TailNum, TotalDelay)

#Inner Merging df_filtered and plane_data_filtered based on TailNum
plane_df_merged <- inner_join(df_filtered,plane_data_filtered, by = "TailNum")
head(plane_df_merged)
dim(plane_df_merged)
#Checking for not available values 
colSums(is.na(plane_df_merged))
#Removing the rows with not available values
plane_df_merged <- subset(plane_df_merged,YearOfManufacture != "")
str(plane_df_merged)
count(plane_df_merged, YearOfManufacture)
#Removing the rows with year of manufacture as 0 and None
plane_df_merged <- subset(plane_df_merged, YearOfManufacture != "0" & YearOfManufacture !="None")
#Changing the data type as integer for the Year of manufacture column
plane_df_merged$YearOfManufacture <- as.integer(plane_df_merged$YearOfManufacture)
str(plane_df_merged)


#plotting a scatter plots with a overlaid regression line
plane_df_merged %>%
  group_by(YearOfManufacture) %>%
  summarize(AverageTotalDelay = mean(TotalDelay)) %>%
  ggplot(aes(x=YearOfManufacture, y=AverageTotalDelay)) +
  geom_point(color = 'skyblue2') +
  geom_smooth(se = FALSE, method = "lm", color = "steelblue",linewidth = 1.5)+ 
  ggtitle("Flight Delays Based on Year of Manufacture") +
  labs(x= "Year of Manufacture", y= "Average Total Delays") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
                                    