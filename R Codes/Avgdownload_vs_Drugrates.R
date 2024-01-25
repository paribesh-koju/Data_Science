library(tidyverse)

population_dataset = read_csv('/Users/paribeshkoju/Desktop/Clean data/cleaned_population.csv')
cleaned_crime_dataset= read_csv('/Users/paribeshkoju/Desktop/Clean data/cleaned_crime.csv')
cleaned_broadband_speed= read_csv('/Users/paribeshkoju/Desktop/Clean data/broadband_cleaned.csv') 

grouped_broadband_speeds = cleaned_broadband_speed %>%
  group_by(`Town`,County) %>%
  summarise(`Average download speed (Mbit/s)`= mean(`Average download speed (Mbit/s)`))

crime_dataset_drugs2 = cleaned_crime_dataset %>% 
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  right_join(population_dataset, by = "ShortPC") %>% #joining with population dataset to show district and population
  select(`ShortPC`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, `Town`, District) %>% #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Drugs") %>% #filtering to show only drug crimes of 2022
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

# GROUPING THE TOWN AND COUNTY FOR YEAR 2021
grouped_drug_crime <- crime_dataset_drugs2 %>% 
  filter(`Date of crime`=="2021") %>% 
  group_by(`Falls within`,`Town`) %>% 
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))


broadband_crime_data = grouped_broadband_speeds %>% 
  left_join(grouped_drug_crime,by="Town") %>% 
  na.omit #removing null values


#creating a linear model 
l_model = lm(data=broadband_crime_data, `Average download speed (Mbit/s)`~`Drug Offence Rate`) #this model predicts Average download speed as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 


#creating the linear model graph
ggplot(broadband_crime_data,aes(x=`Drug Offence Rate`,y=`Average download speed (Mbit/s)`)) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50,5))+ #setting limits and breaks
  geom_point(data = filter(broadband_crime_data,County=="KENT"),aes(color=c("Red"="Kent")))+ #setting color as red for Kent's data point
  geom_point(data = filter(broadband_crime_data,County=="SURREY"), aes(color=c("Blue"="Surrey"))) + #setting color as blue for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="lightgreen")+ #adding linear regression line and omitting error bands 
  labs(x="Drug Offence Rate",
       y="Average Download Speed (Mbit/s)",
       title="2021 Average Download Speed vs Drug Offence Rate",color="County") #setting labels


