
# This script will clean data

# Load library
library(tidyverse)
library(readxl)

# Load dataset
disinformation <- read_csv("data/disinformation.csv") # this dataset is created from `clean_yougov` script
load("data/FORTHRIGHT/forthright_webtracking_nicole.RData")
screener_data <- read_excel("data/forthright/305021 - Consumer Digital Pilot - Screener Raw Data.xlsx")


#####PATHS TO DISINFORMATION----

# Identified people that accessed misinformation sources
filtered_us <- webtracking %>% filter( domain %in% disinformation$url )

# Identify paths of people visited misinformation sources
paths_us_forthright <- webtracking %>% filter ( member_id %in% filtered_us$member_id)

####IDEOLOGY----
forthright_ideology <- webtracking %>%
  left_join(screener_data %>% select(member_id, Q13), by = "member_id") %>% 
  # Create a new column to classify political affiliation based on Q13
  mutate(slant=case_when(Q13<=4 ~ "Left wing",
                         Q13==5 ~ "Neutral",
                         Q13>=6 ~ "Right wing", TRUE~NA))

###CLASSIFY LABEL FOR DISINFORMATION SOURCES----
names(disinformation) <- c("domain", #url 
                           "label", 
                           "source", 
                           "last_update", 
                           "harm_score", 
                           "type" )

# Join label
forthright_label <- paths_us_forthright %>% 
  left_join(disinformation %>% select(label, domain), by="domain")

# Save dataset
save(paths_us_forthright, file="data/paths_us_forthright.RData")
save(forthright_ideology, file ="data/forthright_ideology.RData")
save (forthright_label, file = "data/classify_forthright.RData")


