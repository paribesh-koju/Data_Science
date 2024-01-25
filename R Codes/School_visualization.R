library(tidyverse)

library("scales")


cleaned_school_dataset= read_csv('/Users/paribeshkoju/Desktop/Clean data/school_cleaned.csv')

district = read_csv('/Users/paribeshkoju/Desktop/Clean data/cleaned_population.csv') %>% 
  select(`ShortPC`, District) # %>% 
# rename(`Short Post Code`= `ShortPC`) #renaming to match the column name in school dataset

#Joining the district dataset into Schoo Dataset by Short Post Code
cleaned_school_dataset = cleaned_school_dataset %>% 
  left_join(district, by = "ShortPC") %>% 
  na.omit() 

## AVG ATTAINMENT SCORE OF 2021

Grouped_school_dataset = cleaned_school_dataset %>% 
  group_by(`Town`,District,County,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) %>% 
  ungroup(`Town`,District,County,`Year`) 

Grouped_school_dataset %>% 
  filter(Year==2021) %>% #filtering to show only data of 2021
  group_by(County) %>% #grouping by county since we are comparing counties only
  ggplot(aes(x = County, y = `Average Attainment Score`, fill=County)) + #setting x-axis and y-axis values
  scale_y_continuous(limits=c(0,80), breaks = seq(0,80,5))+ #setting limits and breaks
  geom_boxplot() + #specifying the type of plot we need
  labs(title="2021 Average Attainment Score By County Box Plot") #setting label for the chart

## AVG ATTAINMENT IN KENT 2020-21
Grouped_school_dataset2 <- cleaned_school_dataset %>% 
  filter(County=="Kent") %>% #filtering to show only rows with county as Kent
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 


Grouped_school_dataset2 %>%
  group_by(District, Year) %>%  #grouping by District and year since we are comparing average score of districts, year after year
  ggplot( aes(x = `Year`, y = `Average Attainment Score`, group = District, color = District)) + #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 2, color = "black") + #defining point size and color
  scale_y_continuous(limits=c(0,80), breaks = seq(0,80,5)) + #defining limits, breaks 
  labs(title = "2020-2021 Average Attainment Score Line Graph For Kent's District", #defining labels 
       x = "Year",
       y = "Average Attainment Score") 



## AVG ATTAINMENT IN SURREY FOR 2020-21
Grouped_school_dataset3 <- cleaned_school_dataset %>% 
  filter(County=="Surrey") %>% #filtering to show only rows with county as Surrey
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 


Grouped_school_dataset3 %>%
  group_by(District, Year) %>%  #grouping by District and year since we are comparing average score of districts, year after year
  ggplot( aes(x = `Year`, y = `Average Attainment Score`, group = District, color = District)) + #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 2, color = "black") + #defining point size and color
  scale_y_continuous(limits=c(0,40), breaks = seq(0,40,3)) + #defining limits, breaks 
  labs(title = "2020-2021 Average Attainment Score Line Graph For Surrey's District", 
       x = "Year",
       y = "Average Attainment Score") #defining labels 
