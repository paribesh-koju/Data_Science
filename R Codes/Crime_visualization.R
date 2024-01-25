install.packages("fmsb") #installing this package for radar chart

library(tidyverse)

library("scales")
library(fmsb)


cleaned_crime = read_csv('/Users/paribeshkoju/Desktop/Clean data/cleaned_crime.csv') 
cleaned_population = read_csv('/Users/paribeshkoju/Desktop/Clean data/cleaned_population.csv')
cleaned_population

cleaned_crime
head(cleaned_crime)

## DRUG OFFENCE RATE FOR 2021
cleaned_crime = cleaned_crime %>% 
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% 
  group_by(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  select(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% 
  rename(`Crime Count`=n) %>%  
  right_join(cleaned_population, by = "ShortPC") %>% 
  select(`ShortPC`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, `District`) %>% 
  na.omit() %>% 
  filter(`Crime type`== "Drugs" & `Date of crime`== 2021) %>% 
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) 

cleaned_crime

#BOX PLOT TO SEE DRUG OFFENCE RATE FOR 2021
ggplot(data = cleaned_crime, aes(x = `District`, y = `Drug Offence Rate`, fill = `Falls within`)) +
  scale_y_continuous(limits=c(0,0.1), breaks = seq(0,0.1,0.005), labels = label_number()) + 
  geom_boxplot() + 
  labs(title = "2021 Drug Offence Rate By District Box Plot") +
  coord_flip() 

## VEHICE CRIME RATE FOR SEPTEMBER 2021 USING RADAR CHART
crime_dataset_vehicle = cleaned_crime %>% 
  group_by(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  select(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% 
  rename(`Crime Count`=n) %>%  
  ungroup(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  right_join(cleaned_population, by = "ShortPC") %>% 
  select(`ShortPC`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% 
  na.omit() %>% 
  filter(`Crime type`== "Vehicle crime" & `Date of crime`=="2021-09") %>%   
  mutate(`Vehicle Crime Rate` = (`Crime Count` / Population)*10000) 

radar_data<- crime_dataset_vehicle %>%
  select(District, `Vehicle Crime Rate`) %>%
  unique()  # Assuming you want unique districts


# Find the max value for scaling the radar chart
max_value <- max(radar_data$`Vehicle Crime Rate`, na.rm = TRUE)

# Create a dataframe with max values
max_row <- data.frame(District = "Max", `Vehicle Crime Rate` = max_value) %>% 
  rename(`Vehicle Crime Rate`= `Vehicle.Crime.Rate`)

# Add the max_row dataframe to the start of radar_data
radar_data <- rbind(max_row, radar_data)

# Create the radar chart
radar_chart <- radarchart(radar_data, axistype = 1,
                          pcol = c("black", rep("blue", nrow(radar_data) - 1)),
                          pfcol = c(NA, rep(rgba(0, 0, 1, 0.5), nrow(radar_data) - 1)),
                          plwd = 2)

## ROBBERY RATE CHART FOR SEPTEMBER 2021

#modifying our crime dataset to show robbery crime rate and crime count 
crime_dataset_robbery = cleaned_crime %>% 
  group_by(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  ungroup(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  right_join(cleaned_population, by = "ShortPC") %>% #joining with population dataset to show district and population
  select(`ShortPC`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Robbery" & `Date of crime`=="2021-09") %>%   #filtering to show only vehicle crimes of 2022 June
  mutate(`Robbery Crime Rate` = (`Crime Count` / Population)*10000) %>% #calculating vehicle crime rate per 10000 people
  group_by(District) %>% #grouping by district
  summarise(TotalRobberyCrimeRate = sum(`Robbery Crime Rate`)) #aggregating crime rates by District

cleaned_crime
crime_dataset_robbery


ggplot(crime_dataset_robbery, aes(x = "", y = TotalRobberyCrimeRate, fill = District)) + #defining x axis and y axis values
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "District", title = "Robbery Crime Rate by District in June 2022") #defining lables



## DRUG OFFENCE RATE PER 10k PEOPLE LINE CHART IN SEPTEMBER 2021

crime_dataset_drugs2 <-cleaned_crime %>% 
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% 
  group_by(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  select(`ShortPC`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  right_join(cleaned_population, by = "ShortPC") %>% #joining with population dataset to show district and population
  select(`ShortPC`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Drugs") %>% #filtering to show only drug crimes of 2022
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

#grouping the drug crime dataset by county and year and showing the rate for each group
Grouped_drug_crime <- crime_dataset_drugs2 %>% 
  group_by(`Falls within`,`Date of crime`) %>% 
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))

#creating line graph of average house prices from 2021-2022
Grouped_drug_crime %>%
  group_by(`Falls within`, `Date of crime`) %>%  #grouping by county and date of crime since we are comparing offence rate in counties year after year
  ggplot( aes(x = `Date of crime`, y = `Drug Offence Rate`, group = `Falls within`, color = `Falls within`)) + #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 2, color = "black") + #defining point size and color
  scale_y_continuous(limits=c(0,0.2), breaks = seq(0,0.2,0.01), labels = label_number()) + #defining limit and breaks
  labs(title = "2020-2023 Drug Offence Rate", #defining labels 
       x = "Year",
       y = "Drug Offence Rate") #setting labels
