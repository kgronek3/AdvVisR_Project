library(janitor)
library(rnaturalearth)
library(lubridate)
library(viridis)
library(latex2exp)
library(cowplot)
library(gridExtra)


source("dataset_cleaning.R")
options(scipen = 10)

# Land and ocean temperature plots ####
# 1) Make plot with all regional temperatures and ocean temperatures in 
#    a grid (with global temperatures layered in a different line type,
#    for example dotted line)
# 2) Make a seasonal plot for Global temperatures with respect to different
#    seasons 


ggplot(data = ocean_temps) + 
    geom_line(aes(x = date, y = Global_Ocean))


ggplot(data = land_temps) + 
    geom_line(aes(x = date, y = across(where(is.numeric))))

land_temps %>% 
    select(-Global_Land_Temperature) %>% 
    pivot_longer(!date,names_to = "Region", values_to = "Temperature") %>% 
    ggplot(aes(x = date, y = Temperature, group = Region, color = Region)) +
    geom_line()

# Heat map of global land and ocean temperatures since 1880 ####
levels <- c("Jan",
            "Feb",
            "Mar",
            "Apr",
            "May",
            "Jun",
            "Jul",
            "Aug",
            "Sep",
            "Oct",
            "Nov",
            "Dec")

ocean_temps <- ocean_temps %>%
    mutate(year = year(date),
           month = case_when(month(date) == 1  ~ "Jan",
                             month(date) == 2  ~ "Feb",
                             month(date) == 3  ~ "Mar",
                             month(date) == 4  ~ "Apr",
                             month(date) == 5  ~ "May",
                             month(date) == 6  ~ "Jun",
                             month(date) == 7  ~ "Jul",
                             month(date) == 8  ~ "Aug",
                             month(date) == 9  ~ "Sep",
                             month(date) == 10 ~ "Oct",
                             month(date) == 11 ~ "Nov",
                             month(date) == 12 ~ "Dec")) %>% 
    add_row(date = as.Date("2022-12-01"), Global_Ocean = 0.68,
            year = 2022, month = "Dec") %>% 
    mutate(month = fct_relevel(month, levels))

T_land_temps <-  land_temps %>% 
    select(date,Global_Land_Temperature) %>% 
    mutate(year = year(date),
           month = case_when(month(date) == 1  ~ "Jan",
                             month(date) == 2  ~ "Feb",
                             month(date) == 3  ~ "Mar",
                             month(date) == 4  ~ "Apr",
                             month(date) == 5  ~ "May",
                             month(date) == 6  ~ "Jun",
                             month(date) == 7  ~ "Jul",
                             month(date) == 8  ~ "Aug",
                             month(date) == 9  ~ "Sep",
                             month(date) == 10 ~ "Oct",
                             month(date) == 11 ~ "Nov",
                             month(date) == 12 ~ "Dec")) %>% 
    add_row(date = as.Date("2022-12-01"), Global_Land_Temperature = 0.96,
            year = 2022, month = "Dec") %>% 
    mutate(month = fct_relevel(month, levels))


p1 <- ggplot(ocean_temps, aes(x = month, y = year)) + 
    geom_tile(aes(fill = Global_Ocean)) + 
    scale_fill_viridis(option = "viridis", direction = 1,
                       breaks = c(-0.5,0,0.5,0.9),
                       labels = c("-0.5°C","0°C","0.5°C","0.9°C")) + 
    labs(title = "Ocean",
         x = element_blank(),
         y = element_blank()) +
    guides(fill = guide_colorbar(title = "Temperature",
                                 barwidth = 2,
                                 barheight = 22,
                                 label.position = "left")) +
    theme_minimal() + 
    scale_y_continuous(breaks = NULL) +
    theme(panel.grid = element_blank(),
          legend.position = "left",
          axis.text.x = element_text(vjust = 13),
          plot.title = element_text(vjust = -5, hjust = 0.5))
p1

p2 <- ggplot(T_land_temps, aes(x = month, y = year)) + 
    geom_tile(aes(fill = Global_Land_Temperature)) + 
    scale_fill_viridis(option = "inferno",
                       breaks = c(-1.5,-1, 0, 1, 2, 2.5),
                       labels = c("-1.5°C","-1°C","0°C","1°C","2°C","2.5°C")) +
    labs(title = "Land",
         x = element_blank(),
         y = element_blank()) +
    guides(fill = guide_colorbar(title = "Temperature",
                                 barwidth = 2,
                                 barheight = 22)) +
    theme_minimal() + 
    scale_y_continuous(breaks = seq(1880, 2020, by = 20))+
    #scale_y_continuous(breaks = NULL) +
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(margin = unit(c(0,0.5,0,0), "cm")),
          axis.text.x = element_text(vjust = 13),
          plot.title = element_text(vjust = -5, hjust = 0.5))

p2


plot_grid(p1,p2)


grid.arrange(p1,p2,
             ncol = 2,
             widths = c(1,1.15),
             top = "World average temperatures (1880 - 2022)")


# World map/Europe map of TOTAL (since industrial era) emmisions by countries ####
# 1) Make a grid plot with world map, zoom on europe emissions and barplot below
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


# Barplot of (10) biggest polluter countries in 2021 ####
# 1) Sort bars
# 2) Add flags of countries by the appropriate bars
# 3) Change breaks on y axis
# 4) Add average person CO2 emissions over the plot
# 5) Add number of tonnes of CO2 vertically inside each bar (or over the bar)
# 6) Add title, labels and source
# 7) Add mean emissions of the world as a vertival line

co2_names <- co2_names %>% 
    filter(Year == 2021) %>% 
    data.frame()

df <- data.frame("Country" = colnames(co2_names)[-1],
                 "CO2_2021" = as.numeric(co2_names[1,])[-1]) %>% 
    arrange(desc(CO2_2021)) %>% 
    filter(Country %in% c("China", "United.States", "India", "Russia", "Japan",
                          "Iran", "Germany", "Saudi.Arabia", "Indonesia", 
                          "South.Korea"))

df

ggplot(data = df, aes(x = factor(Country), y = CO2_2021)) + 
    geom_bar(stat = "identity") + 
    coord_flip()

# Barplot of biggest polluter celebrities in 2022 ####
# 1) Sort bars
# 2) Add photos of celebrities over appropriate bars
# 3) Change breaks on y axis
# 4) Add average person CO2 emissions over the plot
# 5) Add number of tonnes of CO2 vertically inside each bar (or over the bar)
# 7) Add average person CO2 emissions per year (find source for this value)
# 6) Add title, labels and source

celebs
colnames(celebs)
ggplot(data = celebs, aes(x = factor(Celebrity.Jet),y = CO2e.tonnes.)) +
    geom_bar(stat = "identity")
    
# Decomposition of world emission over time by industry sectors ####

sectors <- ghg_sectors %>% 
    select(-Entity, -Code) %>% 
    pivot_longer(!Year, names_to = "Industry", values_to = "Emissions")

sectors

ggplot(sectors, aes(x = Year, y = Emissions, fill = Industry)) +
    geom_area(alpha = 0.7) +
    #guides(title = TeX(r'$CO_2$ Emissions'))
    ylab(TeX(r'($CO_2$ emissions (billion of tonnes) )')) +
    theme(plot.title = element_text("Table. 1 ")) # nie działa z latex2exp
    


# Total Decomposition with treemapify (14_AV_adv_plots_p1) ####

# Arctic ice sheet coverage ####

ice_north %>% 
    

ice_north %>% 
    mutate(year = year(date),
           month = as.factor(month(date))) %>% 
    ggplot(aes(x = month, y = Extent, group = year, color = year)) +
    geom_line() + 
    coord_polar(start = 2*pi/21) 
    

ice_north %>% 
    ggplot(aes(x = date, y = Extent)) +
    geom_line()

ggplot(ice_north, aes(x = date, y = Extent, color = date)) + 
    geom_path() +
    coord_polar(start = 2*pi/12) 


ice_north %>% read.csv("./data/N_seaice.csv", sep = ",") %>% 
    select(-Source.Data, -Missing) %>% 
ggplot(aes(x = Year, y = Extent, color = Day)) + 
    geom_path() +
    coord_polar(start = 2*pi/30)


