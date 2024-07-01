library(tidyverse)
library(urltools)
# need this -> install.packages("urltools")

# read disinformation
disinformation <- read_csv("data/disinformation.csv")

#Load forthright (long)
forthright <- load("data/FORTHRIGHT/forthright_webtracking.RData")

#Separate the domain and its suffix
decomp <- suffix_extract( disinformation$url )

disinformation2 <- disinformation %>% 
  mutate( domain = ifelse( !is.na(decomp$domain), decomp$domain, url ) )

##Identified people that accessed misinformation sources
filtered_us <- webtracking %>% filter( domain %in% disinformation2$domain )

#Identify paths of people visited misinformation sources
paths_us_forthright <- webtracking %>% filter ( member_id %in% filtered_us$member_id)

#Save as RData
save(paths_us_forthright, file="data/paths_us_forthright.RData")
