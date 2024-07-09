
#Load library
library(tidyverse)
library(dplyr)
library(urltools)

#Import files
disinformation <- read_csv("data/disinformation.csv")
paths_to_disinformation <- read_csv("data/paths_to_disinformation.csv")

#Rename
names(disinformation) <- c("extension", #url 
                           "label", 
                           "source", 
                           "last_update", 
                           "harm_score", 
                           "type" )
#Join label
yougov_label <- paths_to_disinformation %>% 
  left_join(disinformation, by="extension") %>% 
  select(-source, -last_update, -harm_score, -type)

#Check
table(yougov_label$label)

#----------------------------#
#Import
forthright <- load("data/paths_us_forthright.RData")

#OLD BUT GOLD
# #Separate the domain and its suffix
# decomp <- suffix_extract( disinformation$domain )
# 
# disinformation2 <- disinformation %>% 
#   mutate( domain = ifelse( !is.na(decomp$domain), decomp$domain, domain ) ) %>% 
#   select(-source, -last_update, -harm_score, -type) %>%
#   distinct() 
# 
# # find duplicated sites in disinformation2
# duplicated_domains <- disinformation2 %>%
#   group_by(domain) %>% 
#   tally() %>% 
#   filter( n > 1 ) %>%
#   pull( domain )
# 
# disinformation2_no_duplicates <- disinformation2 %>%
#   filter( ! domain %in% duplicated_domains )
# 
# disinformation2_duplicates <- disinformation2 %>%
#   filter( domain %in% duplicated_domains ) %>%
#   arrange( domain )

# #Join label
# forthright_label <- paths_us_forthright %>% 
#   left_join(disinformation %>% select(label, domain), by="domain")
# 
# #Check
# table(forthright_label$label)
# 
# #Exploring data
# label_media <- forthright_label %>% 
#   group_by(ref_media, other, label, type) %>% 
#   tally
# 
# #Comparing fake news on app and website
# forthright_label %>% 
#   group_by(label, type) %>% 
#   filter(label=="fake news") %>% 
#   tally

#Check
table(forthright_label$type)

#Looking at distribution
yougov_label %>% 
  filter(!is.na(label)) %>% #this is to filter out the NAs
  group_by(label) %>% 
  tally () %>% 
  mutate(percentage_visit=n/sum(n)*100)

forthright_label %>% 
  filter(!is.na(label)) %>% #this is to filter out the NAs
  group_by(label) %>% 
  tally () %>% 
  mutate(percentage_visit=n/sum(n)*100)

#Graphing--just experiencing
dis_yougov <- yougov_label %>% 
  filter(!is.na(label)) %>%
  group_by(label) %>% 
  tally () %>% 
  mutate(percentage_visit=n/sum(n)*100)
ggplot (data=dis_yougov, aes(x=percentage_visit, y=label))+
  geom_col()

dis_forthright <- forthright_label %>% 
  filter(!is.na(label)) %>% #this is to filter out the NAs
  group_by(label) %>% 
  tally () %>% 
  mutate(percentage_visit=n/sum(n)*100)
ggplot (data=dis_forthright, aes(x=percentage_visit, y=label))+
  geom_col()

#Save dataset
write_csv(yougov_label, "data/classify_yougov.csv")
save (forthright_label, file = "data/classify_forthright.RData")
