library(tidyverse)
library(data.table)
d
load('~/Documents/ScraperWhoScored/TDLXG/teamcodes.rda') 
filenames <- list.files("~/Documents/ScraperWhoScored/Eredivisie/21:22/Events", pattern=glob2rx("*.csv"), full.names=TRUE)
data <- rbindlist(lapply(filenames,fread,colClasses=c(PlayerId="numeric",
                                                      TeamId = "numeric",
                                                      minute = "numeric",
                                                      second = "numeric",
                                                      expandedMinute = "numeric")),
                  fill=TRUE) %>% 
  left_join(fread("~/Documents/ScraperWhoScored/Eredivisie/21:22/Players/merged.csv",header=TRUE) %>%
              filter(Name != "Name") %>%
              mutate(ID = as.numeric(ID)) %>% select(-1) %>% 
              unique() %>% 
              drop_na() %>% 
              mutate(PlayerId = ID),
                   by = c("PlayerId")) %>%
  mutate(PlayerId = Name) %>%
  left_join(TeamCodes %>% 
              mutate(TeamId=as.numeric(X1)) %>%
              select(TeamId,X2) ,by="TeamId") %>%
  mutate(TeamId = X2) %>% unique()


