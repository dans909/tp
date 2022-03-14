#This script will focus on analyzing traveler questionnaire data (pre-followup)
library(tidyverse)
library(readxl)

#Import SPSS data
quest1 <- read_excel('data/SPSS_Traveler_89_participants.xlsx', sheet='SPSSdata', skip = 0) 

#renaming some columns
quest1 <- quest1 %>%
  rename('no' = ...1, 'id'= ...2, 'uni_edu' = 'university_education', 'edu_lower_uni' = 
           'education lower than university', 'edu_up_uni' = 'education up to university',
         'postgrad' = 'Postgraduate education', 'no_edu_info' = 'no education information')

#remove na cases
quest1 <- na.omit(quest1)
nrow(quest1)

#convert data by travel out countries
quest_travel <- quest1 %>%
  pivot_longer(cols = mainland_China:Australia, names_to = 'destination', values_to = 'values') %>%
  filter(values!=0) %>%
  select(id, Duration_of_travel, ESBL, destination) 

quest_travel %>%
  group_by(destination) %>%
  summarize(count=n())

#based on destination counts, best regions to focus are East_Asia, EU_and_NA, 
#mainland_China and SEA; should skip EU/NA because terrain and geographically very broad

#Import country data
quest2 <- read_excel('data/traveler_country_list.xlsx', skip = 0) 
quest2 <- quest2 %>%
  rename('id' = 'THID', 'country' = 'Countries', 
         'city' = 'Cities', 
         'interchange' = 'Interchange Airport')

#Only look at countries for each ID
quest2 %>%
  group_by(id, country) %>%
  summarize()

#Eliminate interchange
country_count <- quest2 %>%
  filter(is.na(interchange) | interchange == '0') %>%
  group_by(country) %>%
  summarize(count=n())%>%
  arrange(desc(count)) %>%
  print(n=Inf)

#add regions
country_count_region <- quest2 %>%
  filter(is.na(interchange) | interchange == '0') %>%
  mutate(region=if_else(country == "Japan" | country == "Russia" | country == 'Taiwan' |
                          country == 'South Korea' | country == 'Mongolia' , "East Asia", 
                if_else(country == "Thailand" | country =='Malaysia' | country=='Singapore' | country == 'Vietnam' | country == 'Indonesia' | country =='Brunei' | country == 'Myanmar' | country == 'Philippines' | country == 'Shingapore', 'Southeast Asia', 
                if_else(country=='Mainland China', 'China', 'Others'))))

quest2 %>% group_by(id) %>% summarize(count=n())

#no. of times people visited region, note total vist count is 131, which is 18 more than expected due to participants visiting multiple regions
country_count_region %>%
  group_by(id, region) %>%
  summarize(count=n()) %>%
  group_by(region) %>%
  summarize(count=n())


#Identifying participants that visited multiple regions
country_count_region %>%
  group_by(id, region) %>%
  summarize(count=n()) %>%
  group_by(id) %>%
  summarize(duplicate = n()>1, region) %>%
  filter(duplicate=='TRUE') %>%
  group_by(id) %>%
  summarize(count=n())

