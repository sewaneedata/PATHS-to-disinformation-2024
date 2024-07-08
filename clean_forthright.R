library(tidyverse)
library(urltools)
# need this -> install.packages("urltools")

# read disinformation
disinformation <- read_csv("data/disinformation.csv")

#Load forthright (long)
load("data/FORTHRIGHT/forthright_webtracking_nicole.RData")

#####OLD BUT GOLD-----
#Separate the domain and its suffix
#decomp <- suffix_extract( disinformation$url )

#disinformation2 <- disinformation %>% 
  #mutate( domain = ifelse( !is.na(decomp$domain), decomp$domain, url ) ) %>% 
  # aa.com.tr gets the domain aa but in forthright aa is for american airlines
  # therefore when we join by domain we join two aas that do not match
  # the line below changes the domain of aa.com.tr to aa.tr to avoid this issue
 # mutate( domain = ifelse( decomp$domain == 'aa', 'aa.tr', decomp$domain))
#####-----

#Identified people that accessed misinformation sources
filtered_us <- webtracking %>% filter( domain %in% disinformation$url )

#Identify paths of people visited misinformation sources
paths_us_forthright <- webtracking %>% filter ( member_id %in% filtered_us$member_id)

#Save as RData
save(paths_us_forthright, file="data/paths_us_forthright.RData")
