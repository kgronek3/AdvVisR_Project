library(janitor)
library(rnaturalearth)

source("dataset_cleaning.R")
options(scipen = 10)

# World map/Europe map of TOTAL (since industrial era) emmisions by countries ####
world <- ne_countries(scale = "medium", returnclass = "sf")
world

co2_codes <- co2_codes %>%
    pivot_longer(-Year) %>% 
    pivot_wider(names_from=Year, values_from=value) %>% 
    mutate(Total = rowSums(across(where(is.numeric)),na.rm = T)) %>%
    rename("iso_a3" = name,
           "total_co2_emissions" = Total) %>% 
    select(iso_a3, total_co2_emissions)

world <- left_join(world, co2_codes, by = "iso_a3")

format_sep <- function(x) format(x, big.mark = ' ')

# World
ggplot(data = world) +
    geom_sf(aes(fill =  total_co2_emissions)) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", guide = guide_colorbar(barwidth = 30), name = NULL, labels = format_sep) +
    coord_sf(expand = FALSE) +
    #theme(legend.position = 'right')
    theme(legend.position = 'bottom')

# Europe
ggplot(data = world) +
    # geom_sf(aes(fill = income_grp )) +
    geom_sf(aes(fill =  total_co2_emissions)) +
    # scale_fill_brewer(palette = 'YlOrBr') +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", guide = guide_colorbar(barwidth = 30), name = NULL,
                         labels = format_sep) +
    coord_sf(xlim = c(-30, 50), ylim = c(35, 70), expand = TRUE) +
    # theme(legend.position = 'right') +
    theme(legend.position = 'bottom')






