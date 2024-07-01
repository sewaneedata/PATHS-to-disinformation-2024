#This will join people's ideology and domain label from Forthright
#Load library + import data
library(tidyverse)
library(dplyr)
ideology <- load("data/forthright_ideology.RData")
classify <- load("data/classify_forthright.RData")


#Join dataset
forthright_ideology <- forthright_ideology %>% 
  select(member_id, Q13, slant) %>%
  distinct() 

explore_forthright <- forthright_label %>% 
  left_join(forthright_ideology, by="member_id")

#Explore dataset----------
#There are 410 people visited untrustworthy sites out of 584 (70.2%)
explore_forthright %>% 
  group_by(member_id) %>%
  distinct() %>% 
  count ()

#Distribution left/right wing
#Left wing = 146 (35.6%)
explore_forthright %>% 
  group_by(member_id, slant) %>%
  filter(slant=="left") %>% 
  distinct(member_id) %>% 
  tally ()
#Right wing = 170 (41.46%)
explore_forthright %>% 
  group_by(member_id, slant) %>%
  filter(slant=="right") %>% 
  distinct(member_id) %>% 
  tally ()
#Neutral = 93 (22.68%)
explore_forthright %>% 
  group_by(member_id, slant) %>%
  filter(slant=="neutral") %>% 
  distinct(member_id) %>% 
  tally ()

#Finding this weird guy--political side is NA, member_id=4433402
explore_forthright %>% 
  group_by(member_id, slant) %>%
  filter(!slant %in% c("left","right","neutral")) %>% 
  distinct(member_id) %>% 
  tally ()
#This guy only browse on fake source 3 times (cbsnews)
explore_forthright %>% 
  filter (member_id==4433402, label=="fake") %>% 
  group_by(domain) %>% 
  tally()
