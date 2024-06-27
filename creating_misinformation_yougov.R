library(tidyverse)



us_webtrack <- read_csv("data/yougov_webtrack.csv")


#finding the ones that are misinformation
idx <- which( us_webtrack$domain %in% disinformation_domains_clean$url)


#joing 
clean_yougov <- us_webtrack[idx,]


#creating a csv with only the misinformation information
write_csv(clean_yougov, "misinformation_yougov.csv")


#cleaning fothright now
load("data/forthright/forthright_webtracking.RData")

view(forthright)
