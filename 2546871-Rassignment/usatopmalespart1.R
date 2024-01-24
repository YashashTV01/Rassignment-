# Libraries ---------------------------------------------------------------
#to perform dplyr and ggplot2 we will import library tidyverse
library(tidyverse)
#installing babynames packages to perform tasks on dataset babynames 
install.packages("babynames")
install.packages("plotly")
library(plotly)
library(babynames)

# Operations to filter required data --------------------------------------

#Filtering the dataset to consider males of USA born in year between 1900 to 1999
males<-babynames%>%
  filter(sex=="M",year>=1900,year<=1999)
#It helps to displaying the filtered dataset for males 
males
#Counts the total occurrences of each each names which is grouping by names 
count_males<-males%>%
  group_by(name)%>%
  summarise(total=sum(n))

#It helps to displaying the counts of males 
count_males

#Selecting the top three names for males based on total occurrences and it is also arrange in descending order
top_males<-count_males%>%
  top_n(3,total)%>%
  arrange(desc(total))

#It helps to display the top three names of males 
top_males

#Filtering the males dataset to include only the top three names 
filtering<-males%>%
  filter(name%in%top_males$name)

#It helps to display the filtered dataset of the top three names 
filtering


# Line Graph for the data -------------------------------------------------
#Create a line plot using ggplot to visualize the trends of the top three names over the year
ggplot(filtering, aes(x = year, y = n, color = name)) +
  geom_line() +
  labs(title = "Top three names",
       x = "Year",
       y = "Numbers",
       color = "Names")+
  theme_minimal()
ggplotly()
  
 
