
# This script will combine disinformation sources that is provided to us and clean YouGov's dataset

#Load library
library(tidyverse)
library(readxl)

###COMBINE DISINFORMATION SOURCES-----

disinformation <- read_csv("data/disinformation_domains_clean.csv")

#Remove Italian domains (butac, bufale, bufalopedia)
disinformation <- disinformation %>% 
  filter (!source %in% c("butac", "bufale", "bufalopedia"))

#W3P media
wp3_list_media <- readxl::read_excel("data/LISTS/WP3_list_media.xlsx")

#Clean data
wp3_list_media <- wp3_list_media %>% 
  rename (url=`Media outlet`) %>% 
  mutate (url=tolower(url))

#Remove duplicates but there's no duplicate (0)
disinformation <- disinformation %>% 
  filter(!url %in% wp3_list_media$url)

#WP3 alternative media
wp3_list_alternative_media <- readxl::read_excel("data/LISTS/WP3_list_alternativemedia.xlsx")

#Clean data
wp3_list_alternative_media <- wp3_list_alternative_media %>% 
  rename (url=`Media`)

#Checking data--(1) repeat
idx<-which(wp3_list_alternative_media$url %in% disinformation$url) #this show which line(s) already have in the other dataset
#Remove the repeat found in line 32
wp3_list_alternative_media <- wp3_list_alternative_media [-idx,]

#Append, bind row
disinformation <- bind_rows(disinformation, wp3_list_alternative_media %>% select(url))

#Save disinformation sources
write_csv(disinformation, "data/disinformation.csv") 

###CLEAN YOUGOV-----
# this is an edited version of the Webtracking dataset that Dr. Rudd gave to us
load("data/YouGov/yougov_webtracking.RData")
#A cleaned CSV provided to us by Dr. Rudd
#paths_to_disinformation <- read.csv("data/paths_to_disinformation.csv")

#Filter USA
webtracking <- webtracking %>% 
  filter(iso2=="US")

#Identified people that accessed misinformation sources
filtered_us <- webtracking %>% filter (extension %in% disinformation$url )

#Identify paths of people visited misinformation sources
paths_us <- webtracking %>% filter ( person_id %in% filtered_us$person_id )

###CLASSIFY LABEL FOR DISINFORMATION SOURCES----
#Rename
names(disinformation) <- c("extension", #url 
                           "label", 
                           "source", 
                           "last_update", 
                           "harm_score", 
                           "type" )
#Join label
yougov_label <- paths_us %>% 
  left_join(disinformation %>% select( extension, label ), by="extension") 

####IDEOLOGY----
#getting YouGov ideology column 
survey_info <- read.csv("data/YouGov/US_survey.csv")
survey_info <- survey_info %>% mutate( person_id = tolower( person_id) )
# Perform the left join
merged_data <- webtracking %>%
  left_join(survey_info %>% select(person_id, q12_ideology), by = "person_id")


yougov_ideology <- merged_data %>%
  select(everything(), q12_ideology) %>% 
  filter(iso2 == 'US') %>%  
  mutate (slant = case_when(
    q12_ideology <= 4 ~ "Left wing",
    q12_ideology == 5 ~ "Neutral",
    q12_ideology >= 6 ~ "Right wing"),
    left = as.numeric(q12_ideology <= 4),
    neutral = as.numeric(q12_ideology == 5),
    right = as.numeric(q12_ideology >= 6))


#Save data
write_csv( paths_us, "data/paths_to_disinformation.csv")
write_csv(yougov_label, "data/classify_yougov.csv")
save(yougov_ideology, file ="yougov_ideology.RData")






