library(tidyverse)
ideology <- load("data/yougov_ideology.RData")
classify_yougov <- read_csv("data/classify_yougov.csv")

yougov_join <- yougov_ideology %>% 
  select(person_id, q12_ideology, slant) %>%
  distinct() 

explore_yougov <- classify_yougov %>% 
  left_join(yougov_join, by="person_id")

# how many people in the data set?? 370
total_people <- yougov_ideology %>% 
  filter(!is.na( slant)) %>% 
  summarise(unique_ids = n_distinct(person_id)) %>% 
  pull(unique_ids)
total_people


# how many people have visited fake news sources?
fake_news_visitors <- explore_yougov %>% 
  filter(!is.na( slant)) %>% 
  summarise(unique_ids = n_distinct(person_id)) %>% 
  pull(unique_ids)
fake_news_visitors



# 75% of people visted fake news sites
fake_news_visitors / total_people

non_fake_news_visitors <- total_people - fake_news_visitors
non_fake_news_visitors

#left vist 217
yougov_ideology %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Left wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

#Left visits = 171
explore_yougov %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Left wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

171/217*100 #78.42029% of people from left wing visit untrustworthy sites

#Total right = 126
yougov_ideology %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Right wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

#Right visits = 90
explore_yougov %>% 
  filter(!is.na(slant)) %>% 
  filter(slant=='Right wing') %>% 
  summarise(unique_ids = n_distinct(person_id))

90/126*100 #71.43% of people from right wing visit untrustworthy sites

#Total neutral = 107
yougov_ideology %>% 
  filter(slant=='Neutral') %>% 
  summarise(unique_ids = n_distinct(person_id))

#Neutral visits = 72
explore_yougov %>% 
  filter(slant=='Neutral') %>% 
  summarise(unique_ids = n_distinct(person_id))

72/107*100 #67.29 of people who are neutral visit untrustworthy sites

#Who has the most visits in this dataset? ---person_id=41773241, visits=193921, right wing
yougov_ideology %>% 
  group_by(person_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)

#Where does this person go the most? ---google, visit=16785
yougov_ideology %>% 
  filter(!is.na(domain)) %>% 
  filter(person_id=='us:41773241') %>% 
  group_by(domain, extension) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(1)

#Who visited disinformation site the most? ---person_id=us:2381143, visit=8325, right wing
explore_yougov %>% 
  filter(!is.na(label)) %>% 
  group_by(person_id, slant) %>% 
  summarise(visits=n(), .groups="drop") %>% 
  arrange(desc(visits)) %>% 
  head(1)
#Using pretty much the same code, also found that there's a person who only visited site (1), but this person is super conservative (10)

#Where did this person go? Majority is right bias--5542 visits
explore_yougov %>% 
  filter(person_id=="us:2381143") %>% 
  group_by(label) %>% 
  tally()



#Top 10 sites get the most visit? fox news--21351 visits (1)
explore_yougov %>% 
  filter(!is.na(label)) %>% 
  group_by(domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)

#Distribution of type of sites that neutral affiliation visited? Majority of them visit left bias sites--3034
explore_yougov %>% 
  filter(slant=="Neutral", !is.na(label)) %>% 
  group_by(label) %>% 
  tally()

#Where did they go? Majority is people.com--981 visits
explore_yougov %>% 
  filter(slant=="Neutral", label=="left bias") %>% 
  group_by(domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits))

#Who visit disinformation site the most in right wing? --person_id=us:2381143, visits=8325
explore_yougov %>% 
  filter(!is.na(label), slant=="Right wing") %>% 
  group_by(person_id) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(1)

#Where did this person go? --fox news, visits=2799
explore_yougov %>% 
  filter(person_id=='us:2381143', !is.na(label)) %>% 
  group_by(label, domain) %>% 
  summarise(visits=n()) %>% 
  arrange(desc(visits)) %>% 
  head(10)






##Experiment more-----------------------------
#install.packages('readxl')
library(readxl)
screener_data <- read_excel("data/FORTHRIGHT/305021 - Consumer Digital Pilot - Screener Raw Data.xlsx")

explore_forthright <- explore_forthright %>%
  left_join(screener_data %>% select(member_id, Q8r5, Q9r3, Q12r3, Q12r4), by = "member_id")

#How do you usually discern factually correct information in the media from information that is false? I rely on my gut feeling, and my own knowledge on the subject (5=Always). Results: right=30, neutral=11, left=17
explore_forthright %>% 
  filter(Q12r4==5) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#How do you usually discern factually correct information in the media from information that is false? I consult fact-checking websites in case of doubt (5=Always). Results: right=12, neutral=7, left=26
explore_forthright %>% 
  filter(Q12r3==5) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#How do you usually discern factually correct information in the media from information that is false? I consult fact-checking websites in case of doubt (5=Always). Results: right=39, neutral=14, left=12
explore_forthright %>% 
  filter(Q12r3==1) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#In your view, to be a good citizen, how important is it for a person to…Be skeptical of what the mainstream media report (5=Very important). Results: right=79, neutral=14, left=26
explore_forthright %>% 
  filter(Q9r3==5) %>% 
  group_by(slant) %>% 
  distinct(member_id) %>% 
  tally()
#I have a good knowledge of current affairs and political issues (5=Strongly agree). Results: right=33, neutral=9, left=32
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
            left_visits=sum(slant=="Left wing", na.rm=TRUE), 
            right_visits=sum(slant=="Right wing", na.rm=TRUE), 
            neutral_visits=sum(slant=="Neutral", na.rm=TRUE)) %>%
  arrange(desc(total_visits)) %>% 
  head(10)
#Plot the data
ggplot( top10 %>% select(-total_visits) %>% pivot_longer( left_visits:neutral_visits ) ) +
  geom_col( aes(y=domain, x=value, fill=name)  )

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
                     Q28, #Household income
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
table(explore_survey$Q26) 
explore_survey %>%
  filter(dis == 1) %>%
  group_by(Q26) %>% tally() 

#Distribution of age group?
table(explore_survey$QAGE) 
explore_survey %>%
  filter(dis == 1) %>%
  group_by(QAGE) %>% tally() 

#Education level?
table(explore_survey$Q27) 
explore_survey %>%
  filter(dis == 1) %>%
  group_by(Q27) %>% tally() 

#Import dataset
member_demos <- read_excel("data/FORTHRIGHT/305021_Member_Demos.xlsx")
explore_survey <- explore_survey %>% 
  left_join(member_demos %>% select(member_id, race_id), by="member_id")


#Save dataset
save(explore_forthright, file="data/explore_forthright.RData")
write_csv(explore_survey, "data/forthright_survey.csv")






















