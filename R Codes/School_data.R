library(tidyverse)

kent_school_2018 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/School/Kent 2018-2019 School Dataset.csv")
surrey_school_2018 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/School/Surrey 2018-2019 School Dataset.csv")
kent_school_2021 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/School/Kent 2021-2022 School Dataset.csv")
surrey_school_2021 = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/School/Surrey 2021-2022 School Dataset.csv")


# CLEANING THE DATA OF BOTH COUNTIES SCHOOL FOR THE YEAR 2018-2019
kent_school_2018 = kent_school_2018 %>% 
  select(SCHNAME,ATT8SCR, PCODE,TOWN) %>%  
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, `Postcode`= PCODE,Town=`TOWN`) %>% 
  mutate('ShortPC'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%   
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  
  mutate(County = "Kent") %>% 
  mutate(Year= "2018") 

surrey_school_2018 <- surrey_school_2018 %>% 
  select(SCHNAME,ATT8SCR, PCODE,TOWN) %>%
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, `Postcode`= PCODE, Town=`TOWN`) %>% 
  mutate('ShortPC'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>% 
  mutate(County = "Surrey") %>% 
  mutate(Year= "2018")


# CLEANING THE DATA OF BOTH COUNTIES SCHOOL FOR THE YEAR 2021-22
kent_school_2021 = kent_school_2021 %>% 
  select(SCHNAME,ATT8SCR, PCODE,TOWN) %>%  
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, `Postcode`= PCODE,Town=`TOWN`) %>% 
  mutate('ShortPC'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%   
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  
  mutate(County = "Kent") %>% 
  mutate(Year= "2021") 

surrey_school_2021 <- surrey_school_2021 %>% 
  select(SCHNAME,ATT8SCR, PCODE,TOWN) %>%
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, `Postcode`= PCODE, Town=`TOWN`) %>% 
  mutate('ShortPC'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>% 
  mutate(County = "Surrey") %>% 
  mutate(Year= "2021")

cleaned_school = rbind(kent_school_2018,kent_school_2021,surrey_school_2018,surrey_school_2021)

write_csv(cleaned_school,"/Users/paribeshkoju/Desktop/Clean data/school_cleaned.csv")
