library(tidyverse)
ideology <- load("data/yougov_ideology.RData")
classify_yougov <- read_csv("data/classify_yougov.csv")


yougov_join <- yougov_ideology %>% 
  select(person_id, q12_ideology, slant) %>%
  distinct() 

explore_yougov <- classify_yougov %>% 
  left_join(yougov_join, by="person_id")

# how many people in the data set?? 443
yougov_ideology %>% 
  summarise(unique_ids = n_distinct(person_id))

# how many people have visited fake news sources?
explore_yougov %>% 
  summarise(unique_ids = n_distinct(person_id))


