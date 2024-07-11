
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
  left_join(disinformation %>% select( extension, label ), by="extension") 

#Check
table(yougov_label$label)

###FORTHRIGHT-------------------------
#Import
forthright <- load("data/paths_us_forthright.RData")

#Rename for forthright
names(disinformation) <- c("domain", #url 
                           "label", 
                           "source", 
                           "last_update", 
                           "harm_score", 
                           "type" )

#Join label
forthright_label <- paths_us_forthright %>% 
  left_join(disinformation %>% select(label, domain), by="domain")


####EXPERIMENT-----

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
