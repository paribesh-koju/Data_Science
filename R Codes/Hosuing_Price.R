library(tidyverse)
library(lubridate)


data_2019 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/pp-2019.csv")
data_2020 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/pp-2020.csv")
data_2021 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/pp-2021.csvv")
data_2022 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/pp-2022.csv")


names(data_2019)
"SURREY" %in% unique(data_2019$`GREATER MANCHESTER`)

colnames(data_2019) = c("ID","Price","Year","Postcode","Property Type","New","Ownership","Locality","SAON","Street","Village","Town","District","County","Energy_Rating","EPC")
colnames(data_2020) = colnames(data_2019)
colnames(data_2021) = colnames(data_2019)
colnames(data_2022) = colnames(data_2019)

housing_combined = rbind(data_2019,data_2020,data_2021,data_2022)
housing_combined
house_cleaned = housing_combined %>% 
  filter(County %in% c("KENT","SURREY")) %>% 
  mutate(ShortPC=substr(Postcode,1,5)) %>% 
  select(Price,Year,Postcode,Town,District,County) %>% 
  mutate(Year=substr(Year,1,nchar(Year)-6),Price=ifelse(is.na(Price),mean(Price,na.rm = TRUE),Price)) %>% 
  na.omit() %>% 
  distinct()
house_cleaned$Year = year(as.Date(house_cleaned$Year))
unique(house_cleaned$ShortPC)
house_cleaned
write_csv(house_cleaned,"/Users/paribeshkoju/Desktop/Clean data/housing_cleaned.csv")



