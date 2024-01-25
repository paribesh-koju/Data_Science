library(tidyverse)
library("scales")

cleaned_housing_data= read_csv('/Users/paribeshkoju/Desktop/Clean data/housing_cleaned.csv') 
View(cleaned_housing_data)


housing_data_chart = cleaned_housing_data %>% 
  group_by(Town,District,County,Year) %>% 
  summarise(`Average Price`= mean(Price)) %>% 
  ungroup(Town,District,County,Year) 

## Box Plot to show average prices of 2020
housing_data_chart %>% 
  filter(Year==2020) %>% # only select the year 2020
  group_by(County) %>% # to see between counties only
  ggplot(aes(x = County, y = `Average Price`, fill=County)) + 
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,300000))+ 
  geom_boxplot() + 
  labs(title="Avg House Prices-2020") 




## Bar Chart to show average prices of 2020
housing_data_chart %>% 
  filter(Year==2020) %>% # only select the year 2020
  group_by(County) %>% # to see between counties only
  ggplot(aes(x = County, y = `Average Price`, fill= County)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,300000))+ 
  labs(title = "Avg House Prices-2020") 



# Line chart to show the avg houseprices between 2019 and 2022

housing_data_2019_2022 = cleaned_housing_data %>% 
  group_by(County,Year) %>% 
  summarise(`Average Price`= mean(Price)) 


housing_data_2019_2022 %>%
  filter(Year %in% c(2019,2020,2021,2022)) %>% #filtering to show only houseprice data of 2019,2020,2021,2022
  group_by(County, Year) %>%  #grouping by county and date of transfer since we are comparing prices of counties year after year
  ggplot( aes(x = Year, y = `Average Price`, group = County, color = County)) + #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 2, color = "black") + #defining point size and color
  scale_y_continuous(limits=c(0,700000), breaks = seq(0,700000,100000), labels = label_number()) + #defining limits, breaks and setting label as number instead of sceintifc notation
  labs(title = "Avg House Prices between 2019 and 2022", #defining labels 
       x = "Year",
       y = "Average Price") 





