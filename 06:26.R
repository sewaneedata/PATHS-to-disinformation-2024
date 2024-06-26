#Load library
library(tidyverse)

disinformation <- read_csv("data/disinformation_domains_clean.csv")

#Remove Italian domains (butac, bufale, bufalopedia)
disinformation <- disinformation %>% 
  filter (!source %in% c("butac", "bufale", "bufalopedia"))
#Checking
table(disinformation$source)

#Check with W3P media
wp3_list_media <- readxl::read_excel("data/LISTS/WP3_list_media.xlsx")

#Clean data
wp3_list_media <- wp3_list_media %>% 
  rename (url=`Media outlet`) %>% 
  mutate (url=tolower(url))

#Remove duplicates but there's no duplicate (0)
disinformation <- disinformation %>% 
  filter(!url %in% wp3_list_media$url)

#Import alternative media
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

#Load webtrackings
yougov <- load("data/YouGov/webtracking.RData")

webtracking <- webtracking %>% 
  filter(iso2=="US")

#Save dataset as csv
write_csv(webtracking, "yougov_webtrack.csv")

#Load csv
us_webtrack <- read_csv("data/yougov_webtrack.csv")

us_webtrack <- us_webtrack %>% 
  mutate(person_id=substr(person_id, 4, 12))
