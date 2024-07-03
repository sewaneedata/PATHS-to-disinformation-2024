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

#Who has the most visits in this dataset? ---member_id=4723361, visit=705275, right wing
forthright_ideology %>% 
  group_by(member_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)
#Where does this person go the most? ---skeepers, visit=572481
forthright_ideology %>% 
  filter(member_id==4723361) %>% 
  group_by(domain, site_name) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(1)

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

#Top 10 sites get the most visit? dailymail--62287 visits (1)
explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(domain) %>% 
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
#Distribution of type of sites that right affiliation visited? Majority is questionable sources--66102 visits
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
  filter(Q9r3==5, !is.na(label)) %>% 
  group_by(member_id, domain, label, slant) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#People who always consult fact-checking websites in case of doubts and the site they visited
explore_forthright %>% 
  filter(Q12r3==5, !is.na(label)) %>% 
  group_by(member_id, domain, label, slant) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#Looking at Internal/External efficacy...
sample <- explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(Q8r5, slant) %>% 
  summarise(visits=n(), .groups="drop") 
#Plot the data
ggplot(data=sample, aes(x=Q8r5, y=visits, fill=slant))+
  geom_col()+
  labs(x="I have a good knowledge of current affairs and political issues", y="Visits", fill="Political affiliation")

#Top 10 most visited site distribution
top10 <- explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(domain) %>% 
  #cbsnews has 3 NAs so we have to remove the NAs when we do calculations
  summarise(total_visits=n(), 
            left_visits=sum(slant=="left", na.rm=TRUE), 
            right_visits=sum(slant=="right", na.rm=TRUE), 
            neutral_visits=sum(slant=="neutral", na.rm=TRUE)) %>%
  arrange(desc(total_visits)) %>% 
  head(10)
#Plot the data
ggplot( top10 %>% select(-total_visits) %>% pivot_longer( left_visits:neutral_visits ) ) +
  geom_col( aes(y=domain, x=value, fill=name)  )

#Top 10 sites without top 1
explore_forthright %>% 
  filter(!is.na(label), member_id!=6974012) %>% 
  group_by(domain, site_name, label) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#Looking at Internal/External efficacy without people with the most visited
sample1 <- explore_forthright %>% 
  filter(!is.na(label), ! member_id %in% c(6974012,5491081)) %>% 
  group_by(Q8r5, slant) %>% 
  summarise(visits=n(), .groups="drop") 
ggplot(data=sample1, aes(x=Q8r5, y=visits, fill=slant))+
  geom_col()+
  labs(x="I have a good knowledge of current affairs and political issues", y="Visits", fill="Political affiliation")

#How many disinformation sites that people visited in this dataset? Majority of these sites are left bias -> questionable sources -> right bias
dis_sites<-explore_forthright %>% 
  filter(!is.na(label)) %>% 
  group_by(domain, label) %>% 
  tally()
table(dis_sites$label) #Check the label




##Explore survey-------------
#Create a dataset with variables that I want to look at
explore_survey <- forthright_ideology %>% 
  #If the person visit disinformation site, mark as 1, otherwise, mark as 0
  mutate(dis=ifelse(forthright_ideology$member_id %in% forthright_label$member_id, 1, 0)) %>% 
  select(member_id, slant, dis) %>% 
  distinct()
explore_survey <- explore_survey %>% 
  left_join(screener_data %>% 
              select(member_id, 
                     Q25, #Birth year
                     QAGE, #Age group
                     Q26, #Gender
                     Q2, #How often do you intentionally try to avoid political news?
                     Q6r1, #Interest in News about domestic or international politics
                     Q6r2, #Economy, Business and financial news
                     Q6r3, #Entertainment and celebrity news
                     Q6r4, #Arts and culture news
                     Q6r5, #Sports news
                     Q6r6, #Science and technology news
                     Q7, #Generally speaking, how interested are you in politics?
                     Q27, #What is the highest degree or level of school you have completed?
                     ), by="member_id")
#Create a new variable called "age"
explore_survey<-explore_survey %>% 
  mutate(age=2023-Q25)
#What's the mean age?
mean(explore_survey$age, na.rm = TRUE) #Big dataset--50.29725
explore_survey %>%
  filter(dis == 1) %>%
  summarise(mean_age = mean(age, na.rm = TRUE)) #People visited disinformation sites--50.73839

#Gender?
table(explore_survey$Q26) #Big: male=281, female=295, non-binary=6
explore_survey %>%
  filter(dis == 1) %>%
  group_by(Q26) %>% tally() #Dis: male=205, female=199, non-binary=5

#Distribution of age group?
table(explore_survey$QAGE) #Big: majority are 2(18-34, 149) and 6(65+, 122)
explore_survey %>%
  filter(dis == 1) %>%
  group_by(QAGE) %>% tally() #Dis: 2(102), 6(91)

#Education level?
table(explore_survey$Q27) #Big: Majority are 6 (Bachelor's degree, 179) and 3 (Some college credit, no degree, 103)
explore_survey %>%
  filter(dis == 1) %>%
  group_by(Q27) %>% tally() #Dis: 2(137), 6(70)

#Import dataset
member_demos <- read_excel("data/FORTHRIGHT/305021_Member_Demos.xlsx")
explore_survey <- explore_survey %>% 
  left_join(member_demos %>% select(member_id, race_id), by="member_id")


#Save dataset
#write_csv(explore_forthright, "data/explore_forthright.csv")