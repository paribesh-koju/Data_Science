library(tidyverse)
library(dplyr)

#reading csv 
housePriceData = read.csv("/Users/paribeshkoju/Desktop/Clean data/housing_cleaned.csv")

broadBandData = read.csv("/Users/paribeshkoju/Desktop/Clean data/broadband_cleaned.csv")

schoolData = read.csv("/Users/paribeshkoju/Desktop/Clean data/school_cleaned.csv")
names(schoolData)
crimeData = read.csv("/Users/paribeshkoju/Desktop/Clean data/cleaned_crime.csv")
names(broadBandData)



################################################################################################

united = full_join(housePriceData, broadBandData)
united = full_join(united, schoolData)
united = full_join(united, crimeData)

united= united %>% 
  select(Town, County, Price, `Avg.download.speed..Mbit.s.`, avgCrime, att8score2021) %>% 
  group_by(Town) %>% 
  mutate(avgHousePrice = mean(Price), avgDownloadSpeed = mean(`Avg.download.speed..Mbit.s.`),
         avgAtt8Score = mean(att8score2021), avgCrimeRate = mean(avgCrime)) %>% 
  distinct(Town, .keep_all = TRUE) %>% 
  select(-c(Price, `Avg.download.speed..Mbit.s.`, avgCrime, att8score2021)) %>% 
  replace(is.na(.), 0) 



summary(houseData$avgPrice)
#---------------------------------------------------
minVal = min(houseData$avgPrice) 
firstQuartile =  180274
secondQuartile = 246785
thirdQuartile = 336059
maxVal = max(houseData$avgPrice)

diffMinFirst = firstQuartile - minVal
diffFirstSecond = secondQuartile - firstQuartile
diffSecondThird = thirdQuartile - secondQuartile
diffMaxThird = maxVal - thirdQuartile

intervalMin = diffMinFirst / 3
intervalFirst = diffFirstSecond / 3
intervalSecond = diffSecondThird / 3
intervalThird = diffMaxThird / 2


houseScoring <- function(x){
  
  if (x >= minVal & x <= minVal+intervalMin) { 
    10
  } else if (x > minVal+intervalMin & x <= minVal+intervalMin*2) {
    9
  }else if (x > minVal+intervalMin*2 & x <= firstQuartile) {
    8
  }else if (x > firstQuartile & x <= firstQuartile+intervalFirst) {
    7
  }else if (x > firstQuartile+intervalFirst & x <= firstQuartile+intervalFirst*2) {
    6
  }else if (x > firstQuartile+intervalFirst*2 & x <= secondQuartile) {
    5
  }else if (x > secondQuartile& x <= secondQuartile+intervalSecond) {
    4
  }else if (x > secondQuartile+intervalSecond & x <= secondQuartile+intervalSecond*2) {
    3
  }else if (x > secondQuartile+intervalSecond*2 & x <= thirdQuartile) {
    2
  }else if (x > thirdQuartile & x <= thirdQuartile+intervalThird) {
    1
  }else if (x > thirdQuartile+intervalThird & x <= maxVal) {
    0
  }
  
}
names(houseData)


housePriceScore = houseData %>% 
  mutate(scoreHouse = sapply(avgPrice, houseScoring)) %>% 
  arrange(desc(scoreHouse))



#################################################################################################

################## Broadband Score #################

names(broadBandData)

summary(broadBandData$`Avg.download.speed..Mbit.s.`)


minVal = min(broadBandData$`Avg.download.speed..Mbit.s.`) 
firstQuartile = 32.86
secondQuartile = 45.03
thirdQuartile = 55.44
maxVal = max(broadBandData$`Avg.download.speed..Mbit.s.`)

diffMinFirst = firstQuartile - minVal
diffFirstSecond = secondQuartile - firstQuartile
diffSecondThird = thirdQuartile - secondQuartile
diffMaxThird = maxVal - thirdQuartile

intervalMin = diffMinFirst / 3
intervalFirst = diffFirstSecond / 3
intervalSecond = diffSecondThird / 3
intervalThird = diffMaxThird / 2


broadBandScoring <- function(x){
  
  if (x >= minVal & x <= minVal+intervalMin) { 
    0
  } else if (x > minVal+intervalMin & x <= minVal+intervalMin*2) {
    1
  }else if (x > minVal+intervalMin*2 & x <= firstQuartile) {
    2
  }else if (x > firstQuartile & x <= firstQuartile+intervalFirst) {
    3
  }else if (x > firstQuartile+intervalFirst & x <= firstQuartile+intervalFirst*2) {
    4
  }else if (x > firstQuartile+intervalFirst*2 & x <= secondQuartile) {
    5
  }else if (x > secondQuartile& x <= secondQuartile+intervalSecond) {
    6
  }else if (x > secondQuartile+intervalSecond & x <= secondQuartile+intervalSecond*2) {
    7
  }else if (x > secondQuartile+intervalSecond*2 & x <= thirdQuartile) {
    8
  }else if (x > thirdQuartile & x <= thirdQuartile+intervalThird) {
    9
  }else if (x > thirdQuartile+intervalThird & x <= maxVal) {
    10
  }
  
}


broadBandScore = broadBandData %>% 
  mutate(scoreBroadband = sapply(`Avg.download.speed..Mbit.s.`, broadBandScoring))%>% 
  arrange(desc(scoreBroadband))




#################################################################################################

################## School score #########################



names(schoolData)
view(schoolData$att8score2021)

summary(schoolData$att8score2021)

# Define quartiles and intervals
minVal <- min(schoolData$att8score2021)
firstQuartile <- 39.3500
secondQuartile <- 45.1667
thirdQuartile <- 50.9500
maxVal <- max(schoolData$att8score2021)

diffMinFirst <- firstQuartile - minVal
diffFirstSecond <- secondQuartile - firstQuartile
diffSecondThird <- thirdQuartile - secondQuartile
diffMaxThird <- maxVal - thirdQuartile

intervalMin <- diffMinFirst / 3
intervalFirst <- diffFirstSecond / 3
intervalSecond <- diffSecondThird / 3
intervalThird <- diffMaxThird / 2

# Define scoring function
schoolScoring <- function(x) {
  case_when(
    x >= minVal & x <= minVal + intervalMin ~ 10,
    x > minVal + intervalMin & x <= minVal + intervalMin * 2 ~ 9,
    x > minVal + intervalMin * 2 & x <= firstQuartile ~ 8,
    x > firstQuartile & x <= firstQuartile + intervalFirst ~ 7,
    x > firstQuartile + intervalFirst & x <= firstQuartile + intervalFirst * 2 ~ 6,
    x > firstQuartile + intervalFirst * 2 & x <= secondQuartile ~ 5,
    x > secondQuartile & x <= secondQuartile + intervalSecond ~ 4,
    x > secondQuartile + intervalSecond & x <= secondQuartile + intervalSecond * 2 ~ 3,
    x > secondQuartile + intervalSecond * 2 & x <= thirdQuartile ~ 2,
    x > thirdQuartile & x <= thirdQuartile + intervalThird ~ 1,
    x > thirdQuartile + intervalThird & x <= maxVal ~ 0,
    TRUE ~ NA_real_
  )
}

# Apply scoring function and arrange by score in descending order
schoolScore <- schoolData %>% 
  mutate(scoreSchool = schoolScoring(att8score2021)) %>% 
  arrange(desc(scoreSchool))





#################################################################################################



#################### Crime score #################


crimeData = crimeData %>%
  group_by(Town) %>% 
  select(Town, avgCrime, shortPostCode, County) %>% 
  distinct()  



summary(crimeData$avgCrime)
quantile(crimeData$avgCrime)

minVal = min(crimeData$avgCrime)
firstQuartile = 5.785714
secondQuartile = 111.659722
thirdQuartile = 11.659722
maxVal = max(crimeData$avgCrime)

diffMinFirst = firstQuartile - minVal
diffFirstSecond = secondQuartile - firstQuartile
diffSecondThird = thirdQuartile - secondQuartile
diffMaxThird = maxVal - thirdQuartile

intervalMin = diffMinFirst / 3
intervalFirst = diffFirstSecond / 3
intervalSecond = diffSecondThird / 3
intervalThird = diffMaxThird / 2



crimeScoring <- function(x){
  
  if (x >= minVal & x <= minVal+intervalMin) { 
    10
  } else if (x > minVal+intervalMin & x <= minVal+intervalMin*2) {
    9
  }else if (x > minVal+intervalMin*2 & x <= firstQuartile) {
    8
  }else if (x > firstQuartile & x <= firstQuartile+intervalFirst) {
    7
  }else if (x > firstQuartile+intervalFirst & x <= firstQuartile+intervalFirst*2) {
    6
  }else if (x > firstQuartile+intervalFirst*2 & x <= secondQuartile) {
    5
  }else if (x > secondQuartile& x <= secondQuartile+intervalSecond) {
    4
  }else if (x > secondQuartile+intervalSecond & x <= secondQuartile+intervalSecond*2) {
    3
  }else if (x > secondQuartile+intervalSecond*2 & x <= thirdQuartile) {
    2
  }else if (x > thirdQuartile & x <= thirdQuartile+intervalThird) {
    1
  }else if (x > thirdQuartile+intervalThird & x <= maxVal) {
    0
  }
}


crimeScore = crimeData %>% 
  mutate(scoreCrime = sapply(avgCrime, crimeScoring)) %>% 
  arrange(desc(scoreCrime))


#################################################################################################



combine = full_join(housePriceScore, schoolData)
combine = full_join(combine, broadBandScore)
combine = full_join(combine, crimeScore)

combine = combine %>% 
  distinct(Town, .keep_all = TRUE) %>% 
  select(Town, shortPostCode, County, scoreHouse , scoreBroadband, scoreCrime) %>% 
  replace(is.na(.), 0) %>% 
  mutate(avgScore=sum(scoreHouse, scoreBroadband, scoreCrime)/4) %>% 
  arrange(desc(avgScore))

townkent = combine %>% 
  filter(County == "KENT")


townsurrey = combine %>% 
  filter(County == "SURREY")



#################################################################################################
townkent
townsurrey

