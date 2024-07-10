#This will join people's ideology and domain label from Forthright
#Load library + import data
library(tidyverse)
library(readxl)
ideology <- load("data/forthright_ideology.RData")
classify <- load("data/classify_forthright.RData")


#Join dataset
forthright_join <- forthright_ideology %>% 
  select(member_id, Q13, slant) %>%
  distinct() 

explore_forthright <- forthright_label %>% 
  left_join(forthright_join, by="member_id")

#Identify and remove unnecessary
#We remove those visits disinformation sites that are less than 4 seconds because that mean they didn't really visit the site
explore_forthright <- explore_forthright %>% 
  filter(!is.na(label), duration_seconds > 4) %>% 
  bind_rows(explore_forthright %>% filter (is.na(label)))


##Explore dataset----------
#Observations = 584
forthright_ideology %>% 
  summarise(unique_ids = n_distinct(member_id))

#Total left = 207
forthright_ideology %>% 
  filter(slant=='Left wing') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Total right = 231
forthright_ideology %>% 
  filter(slant=='Right wing') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Total neutral = 144
forthright_ideology %>% 
  filter(slant=='Neutral') %>% 
  summarise(unique_ids = n_distinct(member_id))

#Who has the most visits in this dataset? ---member_id=4723361, visit=1150602, right wing
forthright_ideology %>% 
  group_by(member_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)

#People who visited untrustworthy sites = 338
explore_forthright %>% 
  summarise(unique_ids = n_distinct(member_id))

#Who are they and their visits?
explore_forthright %>% 
  group_by(member_id) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) 

#Where did this person go? Majority is left bias--55 visits
explore_forthright %>% 
  filter(member_id==4723361) %>% 
  group_by(label) %>% 
  tally()

#What sites? Majority is cnn.com--8 visits
explore_forthright %>% 
  filter(member_id==4723361, label=="left bias") %>% 
  group_by(domain, ref_media, other) %>% 
  tally()

#Top 10 sites get the most visit? democraticunderground.com--8288 visits
explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

##Explore survey-------------
screener_data <- read_excel("data/FORTHRIGHT/305021 - Consumer Digital Pilot - Screener Raw Data.xlsx")

#Create a dataset with variables that I want to look at
explore_survey <- forthright_ideology %>% 
  #If the person visit disinformation site, mark as 1, otherwise, mark as 0
  mutate(dis=ifelse(forthright_ideology$member_id %in% forthright_label$member_id, 1, 0)) %>% 
  select(member_id, slant, dis) %>% 
  distinct()
explore_survey <- explore_survey %>% 
  left_join(screener_data %>% 
              select(member_id,
                     Q28, #Household income
                     Q25, #Birth year
                     QAGE, #Age group
                     Q26, #Gender
                     Q2, #How often do you intentionally try to avoid political news?
                     Q7, #Generally speaking, how interested are you in politics?
                     Q27, #What is the highest degree or level of school you have completed?
                     ), by="member_id") %>% 
  mutate(age=2023-Q25)

#Import dataset
member_demos <- read_excel("data/FORTHRIGHT/305021_Member_Demos.xlsx")
explore_survey <- explore_survey %>% 
  left_join(member_demos %>% select(member_id, race_id), by="member_id")

#Add age, gender, and race to explore_forthright
explore_forthright <- explore_forthright %>% 
  left_join(explore_survey %>% 
              select(age, Q26, race_id, member_id), by="member_id") %>% 
  rename(gender=Q26)

forthright_ideology <- forthright_ideology %>% 
  left_join(explore_survey %>% 
              select(age, Q26, race_id, member_id), by="member_id") %>% 
  rename(gender=Q26)

#Save dataset
save(explore_forthright, file="data/explore_forthright.RData")
save(forthright_ideology, file="data/forthright_ideology.RData")
write_csv(explore_survey, "data/forthright_survey.csv")


