# Importing Library -------------------------------------------------------
library(tidyverse)
install.packages("plotly")
library(plotly)

# Loading Data ------------------------------------------------------------
data <- read.csv("C:/Users/yasha/OneDrive/Desktop/Rass/data.csv")
view(data)
# Cleaning Data -----------------------------------------------------------
cleaned_data <- data %>%
  drop_na(new_cases, tests_done, population, testing_rate, positivity_rate) %>%
  separate(year_week, into = c("year", "week"), sep = "-W") %>%
  mutate(
    country = coalesce(country, region_name),
    country_code = coalesce(country_code, region),
    year = as.integer(year),
    week = as.integer(week)
  ) %>%
  select(-c(region, region_name))#combaining the 

view(cleaned_data)
dim(cleaned_data)
names(cleaned_data)
any(is.na(cleaned_data))
summary(cleaned_data)
str(cleaned_data)


cleaned_data <- cleaned_data %>%
  mutate(negativity_rate = 100 - positivity_rate)
view(cleaned_data)

# Downloading a Cleaned Data ----------------------------------------------

write.csv(cleaned_data, file = "cleaned_data.csv", row.names = FALSE)


# Performing dplyr on the cleaned data set --------------------------------

#grouping by country and doing sum of new cases and tests done.
grouped_country<-cleaned_data %>% 
  group_by(country) %>% 
  summarize(
    total_cases=sum(new_cases),
    total_testsdone=sum(tests_done)
    )
#use to see the group_country data frame 
view(grouped_country)

#grouping by year and calculating the total cases for years 
grouped_year<-cleaned_data %>% 
  group_by(year) %>% 
  summarize(
    total_cases=sum(new_cases)
     )
#use to see the group_year data frame
view(grouped_year)

#getting specific data if needed based on the user requirement 
getting_data<-cleaned_data %>% 
  filter(country=="Portugal",year==2022,week==20)
#use to see getting_data data frame
view(getting_data)


#mean of testing rate of each year 
testing_mean<-cleaned_data %>% 
  group_by(year) %>% 
  summarise(
    testingrate_mean=mean(testing_rate) 
  )
#use to see testing_mean data frame
view(testing_mean)

#using this we can get the top 1 country which is effected by covid-19
most_postive_rate <- cleaned_data %>%
  group_by(country) %>%
  summarise(
    positivity_rate =max(positivity_rate)
  ) %>%
  top_n(1)

# use to view the top_positivity_rate data frame
View(most_postive_rate)

most_postive_rate <- cleaned_data %>%
  group_by(country) %>%
  summarise(
    positivity_rate =max(positivity_rate)
  ) %>%
  top_n(1)

# use to view the most positivity rate data frame
View(most_postive_rate)

#filtering by year 
getting_by_yer<-cleaned_data %>% 
  filter(year==2023)

#calculating the population of country using country code in the year 2023.
#arranging by descending order 
country_code <- getting_by_yer %>%
  group_by(country, country_code) %>%
  summarise(
    population = sum(population)
  ) %>%
  arrange(desc(population))

# View the country_code data frame
View(country_code)



# Bar Plot ----------------------------------------------------------------
#graphs for cleaned data
selected_columns <- cleaned_data %>%
  select(country, negativity_rate)
group_neg<-selected_columns %>% 
  group_by(country) %>% 
  summarise(negative=sum(negativity_rate))
	
view(group_neg)

# Creating a bar plot with facets for each year
ggplot(cleaned_data, aes(x = country, y = new_cases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total COVID-19 Cases by Countries (2020-2023)", x = "Country", y = "Total Cases") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  facet_wrap(~year, scales = "free_y")
ggplotly()

your_data_summary <- cleaned_data %>%
  group_by(year) %>%
  summarise(total_new_cases = sum(new_cases), avg_testing_rate = mean(testing_rate))

# Line Graph --------------------------------------------------------------
# Visualize the trends using ggplot2
ggplot(your_data_summary, aes(x = year)) +
  geom_line(aes(y = total_new_cases, color = "Total New Cases")) +
  labs(title = "Trends in New Cases (2020-2023)", y = "Count of new cases  (%)") +
  scale_color_manual(values = c("Total  Cases" = "blue"))
ggplotly()


#specific country
specific_country <- cleaned_data %>%
  filter(country == "Croatia")

# Create a line plot for new cases over weeks for Croatia
ggplot(specific_country, aes(x = week, y = new_cases, group = year, color = as.factor(year))) +
  geom_line() +
  labs(title = "New COVID-19 Cases Over Weeks in Croatia",
       x = "Week",
       y = "New Cases",
       color = "Year") +
  theme_minimal()
ggplotly()


#For multiple country to see
# Create a line plot for new cases over weeks
ggplot(cleaned_data, aes(x = week, y = new_cases, group = year, color = as.factor(year))) +
  geom_line() +
  labs(title = "New COVID-19 Cases Over Weeks of European countries",
       x = "Week",
       y = "New Cases",
       color = "Year") +
  theme_minimal()+
  facet_wrap(~country, scales = "free")  # Separate lines for each country
ggplotly()


# Filter data for the year 2020
first_2020 <- cleaned_data %>%
  filter(year == 2020)

# Bubble Plot -------------------------------------------------------------
ggplot(cleaned_data, aes(x = country, y = positivity_rate, color =positivity_rate)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Bubble Plot: Positivity Rate vs. Country", x = "Country", y = "Positivity Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly()

# Pie chart ---------------------------------------------------------------
# Create a pie chart for 2020 year with label=country and values=negative rate
pie_chart<- plot_ly(first_2020, labels = ~country, values = ~negativity_rate, type = 'pie', textinfo = 'label+percent') %>%
  layout(title = "Total COVID-19 negative Rate by Country in 2020")
pie_chart

# Scatter plot ------------------------------------------------------------
#scatter plot for testing Rate vs new Cases
ggplot(cleaned_data, aes(x = testing_rate, y = new_cases, color = positivity_rate)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Scatter Plot: Testing Rate vs. New Cases",
       x = "Testing Rate",
       y = "New Cases",
       color = "Positivity Rate") +
  theme_minimal()
ggplotly()

# Boxplot  -----------------------------------------------------------------
#box plot using x axis= as.factor(year) and y axis=positivty 

ggplot(cleaned_data, aes(x = as.factor(year), y = positivity_rate)) +
  geom_boxplot(fill = "green", color = "gray", outlier.shape = NA) +
  labs(title = "Boxplot of Positivity Rate by Year", x = "Year", y = "Positivity Rate") +
  theme_minimal()
ggplotly()
#ggplotly() helps to convert the graph to responsive graph for all the graphs which i have mentioned it.