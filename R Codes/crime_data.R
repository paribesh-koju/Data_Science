library(tidyverse)

dir_2020 = "/Users/paribeshkoju/Desktop/Ordered FIle/Crime/2020"
dir_2021 = "/Users/paribeshkoju/Desktop/Ordered FIle/Crime/2021"
dir_2022 = "/Users/paribeshkoju/Desktop/Ordered FIle/Crime/2022"
dir_2023 = "/Users/paribeshkoju/Desktop/Ordered FIle/Crime/2023"

file_path_2020 = list.files(dir_2020, pattern = "*.csv$", full.names = TRUE, recursive = TRUE)
file_path_2021 = list.files(dir_2021, pattern = "*.csv$", full.names = TRUE, recursive = TRUE)
file_path_2022 = list.files(dir_2022, pattern = "*.csv$", full.names = TRUE, recursive = TRUE)
file_path_2023 = list.files(dir_2023, pattern = "*.csv$", full.names = TRUE, recursive = TRUE)


combined_crime_2020 = file_path_2020 %>%
  set_names() %>% 
  map_df(~read_csv(.x))
combined_crime_2020

combined_crime_2021 = file_path_2021 %>%
  set_names() %>% 
  map_df(~read_csv(.x))

combined_crime_2022 = file_path_2022 %>%
  set_names() %>% 
  map_df(~read_csv(.x))

combined_crime_2023 = file_path_2023 %>%
  set_names() %>% 
  map_df(~read_csv(.x))



total_set = rbind(combined_crime_2020,combined_crime_2021,combined_crime_2022,combined_crime_2023)
View(total_set)
pcode_lsoa = read_csv("/Users/paribeshkoju/Desktop/Clean data/pcode_lsoa_cleaned.csv")
View(pcode_lsoa)

# SELECTING NECESSARY THINGS FROM CRIME DATASETS OF ALL YEARS SEPARATELT
combined_crime_2020 = combined_crime_2020 %>% 
  select(Month, `Falls within`, `Crime type`, `LSOA code`) %>%
  rename(`Date of crime`= `Month`, `LSOA Code`= `LSOA code`) %>% 
  na.omit()

combined_crime_2021 = combined_crime_2021 %>% 
  select(Month, `Falls within`, `Crime type`, `LSOA code`) %>% 
  rename(`Date of crime`= `Month`, `LSOA Code`= `LSOA code`) %>% 
  na.omit()

combined_crime_2022 = combined_crime_2022 %>% 
  select(Month, `Falls within`, `Crime type`, `LSOA code`) %>% 
  rename(`Date of crime`= `Month`, `LSOA Code`= `LSOA code`) %>% 
  na.omit()

combined_crime_2023 = combined_crime_2023 %>% 
  select(Month, `Falls within`, `Crime type`, `LSOA code`) %>% 
  rename(`Date of crime`= `Month`, `LSOA Code`= `LSOA code`) %>% 
  na.omit()
pcode_lsoa


# JOINING ALL THE CRIME DATASETS FROM ALL YEARS WITH PCODE TO LSOA
combined_crime_2020 = combined_crime_2020 %>% 
  right_join(pcode_lsoa, by = "LSOA Code") %>% 
  select(`Date of crime`, `Falls within`, `Crime type`, `LSOA Code`, `Postcode`, `ShortPC`, Town)  

combined_crime_2021 = combined_crime_2021 %>% 
  right_join(pcode_lsoa, by = "LSOA Code") %>% 
  select(`Date of crime`, `Falls within`, `Crime type`, `LSOA Code`, `Postcode`, `ShortPC`, Town) 

combined_crime_2022 = combined_crime_2022 %>% 
  right_join(pcode_lsoa, by = "LSOA Code") %>% 
  select(`Date of crime`, `Falls within`, `Crime type`, `LSOA Code`, `Postcode`, `ShortPC`, Town)  
combined_crime_2023 = combined_crime_2023 %>% 
  right_join(pcode_lsoa, by = "LSOA Code") %>% 
  select(`Date of crime`, `Falls within`, `Crime type`, `LSOA Code`, `Postcode`, `ShortPC`, Town)



write_csv(total_set,"/Users/paribeshkoju/Desktop/Clean data/cleaned_crime.csv") 











