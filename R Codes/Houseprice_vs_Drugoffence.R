library(tidyverse)


population_dataset = read_csv('/Users/paribeshkoju/Desktop/Clean data/cleaned_population.csv')
cleaned_houseprices = read_csv('/Users/paribeshkoju/Desktop/Clean data/housing_cleaned.csv') 
cleaned_crime_dataset = read_csv('/Users/paribeshkoju/Desktop/Clean data/cleaned_crime.csv') 

#grouping house prices by town and county and finding average price for each group
grouped_house_prices = cleaned_houseprices %>%
  filter(Year=="2021") %>%
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

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

grouped_drug_crime <- crime_dataset_drugs2 %>% 
  filter(`Date of crime`=="2021") %>% 
  group_by(`Falls within`,`Town`) %>% 
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))


house_price_drug_crime_data = grouped_house_prices %>% 
  left_join(grouped_drug_crime,by="Town") %>% 
  na.omit #removing null values


#creating a linear model 
l_model = lm(data=house_price_drug_crime_data, Price~`Drug Offence Rate`) #this model predicts House Price as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(house_price_drug_crime_data,aes(x=`Drug Offence Rate`,y=Price)) +
  scale_y_continuous(limits=c(0,1000000), breaks = seq(0,1000000,200000))+ #setting limits and breaks
  geom_point(data = filter(house_price_drug_crime_data,County=="KENT"),aes(color=c("Red"="Kent")))+ #setting color as red for Kent's data point
  geom_point(data = filter(house_price_drug_crime_data,County=="SURREY"), aes(color=c("Blue"="Surrey"))) + #setting color as blue for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="lightgreen")+ #adding linear regression line and omitting error bands 
  labs(x="Drug Offence Rate",
       y="Price",
       title="2021 House Prices vs Drug Offence Rate",color="County") #setting labels

