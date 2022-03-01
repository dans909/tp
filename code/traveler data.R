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
