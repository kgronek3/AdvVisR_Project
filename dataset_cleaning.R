#### Dataset loading and cleaning script ####
library(tidyverse)

# Sea ice extent Dataset
# https://nsidc.org/data/geophysical-measurements
# https://nsidc.org/data/g02135/versions/3

ice_north <- read.csv("./data/N_seaice.csv", sep = ",") %>% 
    unite("date", Year:Day, sep = "-", remove = TRUE) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    select(-Source.Data, -Missing) %>% 
    filter(!is.na(Extent))


head(ice_north)
tail(ice_north)
    
ice_south <- read.csv("./data/S_seaice.csv", sep = ",") %>% 
    unite("date", Year:Day, sep = "-", remove = TRUE) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    select(-Source.Data, -Missing) %>% 
    filter(!is.na(Extent))

head(ice_south)
tail(ice_south)

ggplot(data = ice_north) +
    geom_line(aes(x = date, y = Extent))

ggplot(data = ice_south) +
    geom_line(aes(x = date, y = Extent))
