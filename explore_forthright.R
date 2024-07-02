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

#Who visited disinformation site the most? ---member_id=6974012, visit=62253, right wing
explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(member_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)
#Using pretty much the same code, also found that there's a person who only visited site (1), but this person is super conservative (10)

#Where did this person go? Majority is questionable sources--61415 visits
explore_forthright %>% 
  filter(member_id==6974012) %>% 
  group_by(label) %>% 
  tally()
#What sites? Majority is dailymail.co.uk--60934 visits
explore_forthright %>% 
  filter(member_id==6974012, label=="questionable sources") %>% 
  group_by(site_name, domain, ref_media, other) %>% 
  tally()

#Top 10 sites get the most visit? dailymail.co.uk--62273 visits (1)
explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(label, domain, site_name) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#Distribution of type of sites that neutral affiliation visited? Majority of them visit right bias sites--1219
explore_forthright %>% 
  filter(slant=="neutral", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()
#Where did they go? Majority is rd.com--471 visits
explore_forthright %>% 
  filter(slant=="neutral", label=="right bias") %>% 
  group_by(domain, site_name) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits))

#Who visit disinformation site the most in left wing? --member_id=5491081, visits=13869
explore_forthright %>% 
  filter(!is.na(label), slant=="left") %>% 
  group_by(member_id) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(1)
#Comparing: the person from right wing visits dis site 4x more than this one

#Where did this person go? --democraticunderground.com, visits=13845
explore_forthright %>% 
  filter(member_id==5491081, !is.na(label)) %>% 
  group_by(label, domain, site_name) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#Distribution of type of sites that left affiliation visited? Majority is fake--14648 visits
explore_forthright %>% 
  filter(slant=="left", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()
#Where they go? democraticunderground--13851 visits
explore_forthright %>% 
  filter(slant=="left", label=="fake") %>% 
  group_by(domain) %>% 
  tally()
#Distribution of type of sites that left affiliation visited? Majority is fake--66102 visits
explore_forthright %>% 
  filter(slant=="right", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()

##Experiment more-----------------------------
#install.packages('readxl')
library(readxl)
screener_data <- read_excel("data/FORTHRIGHT/305021 - Consumer Digital Pilot - Screener Raw Data.xlsx")

explore_forthright <- explore_forthright %>%
  left_join(screener_data %>% select(member_id, Q8r5, Q9r3, Q12r3, Q12r4), by = "member_id")

#How do you usually discern factually correct information in the media from information that is false? I rely on my gut feeling, and my own knowledge on the subject (5=Always). Results: right=36, neutral=12, left=19
explore_forthright %>% 
  filter(Q12r4==5) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#How do you usually discern factually correct information in the media from information that is false? I consult fact-checking websites in case of doubt (5=Always). Results: right=17, neutral=9, left=30
explore_forthright %>% 
  filter(Q12r3==5) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#How do you usually discern factually correct information in the media from information that is false? I consult fact-checking websites in case of doubt (5=Always). Results: right=44, neutral=20, left=16
explore_forthright %>% 
  filter(Q12r3==1) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#In your view, to be a good citizen, how important is it for a person to…Be skeptical of what the mainstream media report (5=Very important). Results: right=92, neutral=17, left=29
explore_forthright %>% 
  filter(Q9r3==5) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#I have a good knowledge of current affairs and political issues (5=Strongly agree). Results: right=46, neutral=11, left=39
explore_forthright %>% 
  filter(Q8r5==5) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()

#People who think it's very important to be skeptical and the site they visited
explore_forthright %>% 
  filter(Q9r3==5, slant=="right", !is.na(label)) %>% 
  group_by(member_id, domain, label) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#People who always consult fact-checking websites in case of doubts and the site they visited
explore_forthright %>% 
  filter(Q12r3==5, slant=="right", !is.na(label)) %>% 
  group_by(member_id, domain, label) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#Save dataset
write_csv(explore_forthright, "data/explore_forthright.csv")
