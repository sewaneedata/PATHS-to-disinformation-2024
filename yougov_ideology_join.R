# this is the file that will join the ideology of forthright with the webtracking data
#load libs
library(tidyverse)
library(readxl)

#getting YouGov ideology column 

survey_info <- read.csv("data/YouGov/US_survey.csv")

survey_info <- survey_info %>% mutate( person_id = tolower( person_id) )

load("data/YouGov/webtracking.RData")


# Perform the left join
merged_data <- webtracking %>%
  left_join(survey_info %>% select(person_id, q12_ideology), by = "person_id")


# Select the necessary columns, including the q12 colum


yougov_ideology <- merged_data %>%
  select(everything(), q12_ideology) %>% 
  filter(iso2 == 'US') %>%  
  mutate (slant = case_when(
    q12_ideology <= 4 ~ "left",
    q12_ideology == 5 ~ "neutral",
    q12_ideology >= 6 ~ "right"),
    left = as.numeric(q12_ideology <= 4),
    neutral = as.numeric(q12_ideology == 5),
    right = as.numeric(q12_ideology >= 6))





save(yougov_ideology, file ="yougov_ideology.RData")


