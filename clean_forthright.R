library(tidyverse)
library(urltools)
# need this -> install.packages("urltools")

# read disinformation
disinformation <- read_csv("data/disinformation.csv")

# load forthright (long)
forthright <- load("data/forthright/forthright_webtracking.RData")

decomp <- suffix_extract( disinformation$url )

disinformation2 <- disinformation %>% 
  mutate( domain = ifelse( !is.na(decomp$domain), decomp$domain, url ) )


filtered_us <- webtracking %>% filter( domain %in% disinformation2$domain )


paths_us_forthright <- webtracking %>% filter ( member_id %in% filtered_us$member_id)


write_csv( paths_us_forthright, "paths_us_forthright.csv")
