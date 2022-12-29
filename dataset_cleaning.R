#### Dataset loading and cleaning script ####
library(tidyverse)
library(openxlsx)

# Sea ice extent Dataset ####
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

ice_melt <- read.xlsx("./data/greenland-daily-melt.xlsx", sheet = 1) %>% 
    select(-X47,-X48) %>% 
    pivot_longer(!c(month,day), names_to = "year", values_to = "IceMelt") %>% 
    mutate(month2 = case_when(month == "March" ~ "03",
                              month == "April" ~ "04",
                              month == "May" ~ "05",
                              month == "June" ~ "06",
                              month == "July" ~ "07",
                              month == "August" ~ "08",
                              month == "September" ~ "09",
                              month == "October" ~ "10"),
           day2 = case_when(day < 10 ~ paste0("0",day),
                            day >= 10 ~ as.character(day))) %>% 
    mutate(date = paste0(year,"-",month2,"-",day2)) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d") %>% 
    select(date,IceMelt) %>% 
    arrange(date)


# Land (1910-2022 and 1880-2022 for global temp) Ocean temperatures (1880-2022) ####
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/southAmerica/land/all/11/1880-2022

# temperature of the ocean - Global
ocean_temps <- read.csv("./data/temp_global_ocean.csv", sep = ",") %>% 
    mutate(date = paste0(substr(.$Year, 1, 4),"-",substr(.$Year, 5, 6),"-","01")) %>% 
    select(3,2) %>% 
    mutate_at("date", as.Date, format = "%Y-%m-%d")

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

# Greenhouse gas emissions data of countries (1750-2017) in tonnes ####
# https://www.kaggle.com/datasets/srikantsahu/co2-and-ghg-emission-data

# make this dataset be the backup

# ghg <- read.csv("./data/emission_data.csv", sep = ",") %>% 
#     mutate(total_emissions = rowSums(across(where(is.numeric)), na.rm=TRUE))
# head(ghg)

# Greenhouse gas emissions data of countries (1990-2019) in tonnes ####
# https://ourworldindata.org/greenhouse-gas-emissions

# Total greenhouse gas emissions by country
ghg_countries <- read.csv("./data/ghg_emissions_countries.csv", sep = ",") %>% 
    select(-Code) %>% 
    rename(GHG_emissions = Total.including.LUCF) %>% 
    pivot_wider(names_from = Entity, values_from = GHG_emissions)

# Total greenhouse gas emissions by industry
ghg_sectors <- read.csv("./data/ghg_emissions_sectors.csv", sep = ",") %>% 
    filter(Entity == "World")

# Biggest CO2 celectrity polluters ####
# https://weareyard.com/insights/worst-celebrity-private-jet-co2-emission-offenders



