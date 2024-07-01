#This will join people's ideology and domain label from Forthright
#Load library + import data
library(tidyverse)
ideology <- load("data/forthright_ideology.RData")
classify <- load("data/classify_forthright.RData")


#Join dataset
forthright_join <- forthright_ideology %>% 
  select(member_id, Q13, slant) %>%
  distinct() 

explore_forthright <- forthright_label %>% 
  left_join(forthright_join, by="member_id")

##Explore dataset----------
#Observations = 584
forthright_ideology %>% 
  summarise(unique_ids = n_distinct(member_id))

#People who visited untrustworthy sites = 410
explore_forthright %>% 
  summarise(unique_ids = n_distinct(member_id))

410/584*100 #70.2% of the observations visit these sites

#Political affiliation
#Total left = 207 (35.4% of 584)
forthright_ideology %>% 
  filter(slant=='left') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Left visits = 146 (35.6% of 410)
explore_forthright %>% 
  filter(slant=='left') %>% 
  summarise(unique_ids = n_distinct(member_id))

146/207*100 #70.5% of people from left wing visit untrustworthy sites

#Total right = 231 (39.55% of 584)
forthright_ideology %>% 
  filter(slant=='right') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Right visits = 170 (41.46% of 410)
explore_forthright %>% 
  filter(slant=='right') %>% 
  summarise(unique_ids = n_distinct(member_id))

170/231*100 #73.59% of people from right wing visit untrustworthy sites

#Total neutral = 144 (24.7% of 584)
forthright_ideology %>% 
  filter(slant=='neutral') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Neutral visits = 93 (22.68% of 410)
explore_forthright %>% 
  filter(slant=='neutral') %>% 
  summarise(unique_ids = n_distinct(member_id))

93/144*100 #64.58% of people who are neutral visit untrustworthy sites

#Finding this weird guy--political is NA, member_id=4433402
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


