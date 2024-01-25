library(tidyverse)


p_to_lsoa = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/postcode-to-lsoa.csv")
colnames(p_to_lsoa)

cleaned_housing = read_csv("/Users/paribeshkoju/Desktop/Clean data/housing_cleaned.csv")
county_data = cleaned_housing %>% 
  select(Postcode,ShortPC,County,Town)

code_to_county = p_to_lsoa %>% 
  select(pcd7,lsoa11cd) %>% 
  rename(Postcode=pcd7,`LSOA Code`=lsoa11cd) %>% 
  right_join(county_data,by="Postcode") %>% 
  distinct()
code_to_county

write_csv(code_to_county,"/Users/paribeshkoju/Desktop/Clean data/pcode_lsoa_cleaned.csv")
