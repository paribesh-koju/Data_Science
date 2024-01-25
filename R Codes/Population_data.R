library(tidyverse)


pop_data = read_csv("/Users/paribeshkoju/Desktop/Ordered FIle/PopulationData/population_data.csv")
View(pop_data)
pop_data = pop_data %>% 
  rename(ShortPC=Postcode) %>% 
  # removing space characters and replacing it with ""
  mutate(ShortPC = gsub(" ", "", ShortPC),  
         ShortPC = if_else(nchar(ShortPC) == 5, 
                           paste0(substr(ShortPC, 1, 4), " ", substr(ShortPC, 5, 6)), 
                           paste0(substr(ShortPC, 1, 3), " ", substr(ShortPC, 4, 5)))) 


pcode_lsoa = read_csv("/Users/paribeshkoju/Desktop/Clean data/pcode_lsoa_cleaned.csv")
View(pcode_lsoa)

cleaned_population = pop_data %>% 
  right_join(pcode_lsoa,by="ShortPC") %>% 
  na.omit()

cleaned_population

write_csv(cleaned_population,"/Users/paribeshkoju/Desktop/Clean data/cleaned_population.csv")


