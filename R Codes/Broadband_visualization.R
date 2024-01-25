library(tidyverse)

library("scales")

cleaned_broadband = read_csv('/Users/paribeshkoju/Desktop/Clean data/broadband_cleaned.csv') 
cleaned_broadband

## BOX PLOT FOR AVG DOWNLOAD SPEED IN BOTH COUNTIES
cleaned_broadband %>% 
  group_by(County) %>%
  ggplot(aes(x = County, y = `Average download speed (Mbit/s)`, fill=County)) +
  scale_y_continuous(limits=c(0,40), breaks = seq(0,40,2)) +  
  geom_boxplot() + 
  labs(title="Avg Download Speed in both counties") 


## BAR CHART TO SEE THE AVG SPEED FOR KENT
cleaned_broadband%>% 
  filter(County== "KENT") %>% 
  group_by(`Town`) %>% 
  summarise(`County Average Download Speed`= mean(`Average download speed (Mbit/s)`)) %>% 
  ggplot(aes(x = `Town`, y = `County Average Download Speed`, fill=`Town`)) + 
  scale_y_continuous(limits=c(0,90), breaks = seq(0,90,5))+ 
  geom_bar(stat = "identity") + 
  labs(title="Average Download Speed Within Surrey Bar Chart") +
  coord_flip()

## BAR CHART TO SEE THE AVG SPEED FOR SURREY
cleaned_broadband%>% 
  filter(County== "SURREY") %>% 
  group_by(`Town`) %>% 
  summarise(`County Average Download Speed`= mean(`Average download speed (Mbit/s)`)) %>% 
  ggplot(aes(x = `Town`, y = `County Average Download Speed`, fill=`Town`)) + 
  scale_y_continuous(limits=c(0,90), breaks = seq(0,90,5))+ 
  geom_bar(stat = "identity") + 
  labs(title="Average Download Speed Within Surrey Bar Chart") +
  coord_flip()
