library(tidyverse)

cleaned_houseprices= read_csv('/Users/paribeshkoju/Desktop/Clean data/housing_cleaned.csv') 
cleaned_school_dataset= read_csv('/Users/paribeshkoju/Desktop/Clean data/school_cleaned.csv')

grouped_house_prices = cleaned_houseprices %>%
  filter(Year=="2021") %>%
  group_by(`Town`,County) %>%
  mutate(`Town` = tolower(`Town`)) %>% 
  summarise(Price=mean(Price))


grouped_school_dataset = cleaned_school_dataset %>%
  filter(`Year`=="2021") %>%
  group_by(`Town`,County) %>%
  mutate(Town= tolower(Town)) %>%  #converting the town from to all lowercase
  summarise(`Attainment Score`=mean(`Attainment Score`))


school_houseprice_data = grouped_school_dataset %>% 
  left_join(grouped_house_prices,by=c("Town"="Town")) %>% 
  na.omit #removing rows with null value

#creating a linear model 
l_model = lm(data=school_houseprice_data, `Attainment Score`~Price) #this model predicts Average attainment score as a function of Average house prices

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(school_houseprice_data,aes(x=Price,y= `Attainment Score`)) +
  scale_y_continuous(limits=c(0,80), breaks = seq(0,80,5))+ #setting limits and breaks
  geom_point(data = filter(school_houseprice_data,County.x=="Kent"),aes(color=c("Red"="Kent")))+ #setting color as red for Kent's data point
  geom_point(data = filter(school_houseprice_data,County.x=="Surrey"), aes(color=c("Blue"="Surrey"))) + #setting color as blye for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="lightgreen")+ #adding linear regression line and omitting error bands 
  labs(x="House Price",
       y="Attainment Score",
       title="2021 Attainment Score vs House Prices",color="County") #setting labels
