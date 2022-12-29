#### Dataset loading and cleaning script ####
library(tidyverse)
library(openxlsx)

# Sea ice extent Dataset
# https://nsidc.org/data/geophysical-measurements
# https://nsidc.org/data/g02135/versions/3

ice_north <- read.csv("./data/N_seaice.csv", sep = ",") %>% 
    unite("date", Year:Day, sep = "-", remove = TRUE) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    select(-Source.Data, -Missing) %>% 
    filter(!is.na(Extent))
    
ice_south <- read.csv("./data/S_seaice.csv", sep = ",") %>% 
    unite("date", Year:Day, sep = "-", remove = TRUE) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    select(-Source.Data, -Missing) %>% 
    filter(!is.na(Extent))


# Daily Surface Melt Extent ####
# http://nsidc.org/greenland-today/greenland-today-data-and-analysis-tools/


# Land (1910-2022 and 1880-2022 for global temp) Ocean temperatures (1880-2022)
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/southAmerica/land/all/11/1880-2022

# temperature of the ocean - Global
to_global <- read.csv("./data/temp_global_ocean.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

plot(x = to_global$date, y = to_global$Global_Ocean, type = "l")

# land temperatures for 6 continents + global land temperature
tl_global <- read.csv("./data/temp_global_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

tl_africa <- read.csv("./data/temp_africa_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

tl_NA <- read.csv("./data/temp_NA_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

tl_SA <- read.csv("./data/temp_SA_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

tl_oceania <- read.csv("./data/temp_oceania_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

tl_asia <- read.csv("./data/temp_asia_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

tl_asia <- read.csv("./data/temp_asia_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

tl_europe <- read.csv("./data/temp_europe_land.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")


land_temps <- left_join(tl_global,tl_africa, by = 'date') %>%
    left_join(tl_NA,by = "date") %>% 
    left_join(tl_SA,by = "date") %>% 
    left_join(tl_oceania,by = "date") %>% 
    left_join(tl_asia,by = "date") %>% 
    left_join(tl_europe,by = "date")


remove(tl_africa, tl_NA, tl_SA, tl_oceania, tl_asia, tl_europe, tl_global)




