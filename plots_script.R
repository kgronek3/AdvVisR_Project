library(janitor)
library(rnaturalearth)
library(lubridate)
library(viridis)
library(latex2exp)
library(cowplot)
library(gridExtra)
library(ggtext)
library(glue)
library(grid)
library(treemapify)

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
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(margin = unit(c(0,0.5,0,0), "cm")),
          axis.text.x = element_text(vjust = 13),
          plot.title = element_text(vjust = -5, hjust = 0.5))

p2

source <- textGrob("Source: NOAA National Centers for Environmental information\n https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series",
                   hjust = 1, # text alignment
                   x = 0.99, y = 0.6, # footer positioning
                   gp = gpar(fontsize = 10, 
                             fontface = 3)) # Italics

grid.arrange(p1,p2,
             ncol = 2,
             widths = c(1,1.15),
             top = "World average temperatures (1880 - 2022)",
             bottom = source)


# World map/Europe map of TOTAL (since industrial era) emmisions by countries ####
# 1) Make a grid plot with world map, zoom on europe emissions and barplot below
world <- ne_countries(scale = "medium", returnclass = "sf")

co2_codes_2 <- co2_codes %>%
    pivot_longer(-Year) %>% 
    pivot_wider(names_from=Year, values_from=value) %>% 
    mutate(Total = rowSums(across(where(is.numeric)),na.rm = T)) %>%
    rename("iso_a3" = name,
           "total_co2_emissions" = Total) %>% 
    select(iso_a3, total_co2_emissions)

world <- left_join(world, co2_codes_2, by = "iso_a3")

format_sep <- function(x) format(x, big.mark = ' ')


# World
W <- world %>% filter(continent != "Antarctica") %>% 
    ggplot() +
    geom_sf(aes(fill =  total_co2_emissions)) +
    scale_fill_viridis(option = "inferno", trans = "sqrt", 
                       guide = guide_colorbar(barwidth = 30), 
                       name = NULL, labels = format_sep, direction = 1) +
    coord_sf(expand = FALSE) +
    theme(legend.position = 'top')
W

# Europe
eu <- world %>% filter(continent == "Europe") %>% 
    ggplot() +
    geom_sf(aes(fill =  total_co2_emissions)) +
    scale_fill_viridis(option = "inferno", trans = "sqrt",name = NULL, labels = format_sep) +
    coord_sf(xlim = c(-22, 50), ylim = c(35, 70), expand = TRUE) +
    theme(legend.position = "none")
eu

# Asia
asia <- world %>% filter(continent == "Asia") %>%
    ggplot() +
    geom_sf(aes(fill =  total_co2_emissions)) +
    scale_fill_viridis(option = "inferno", trans = "sqrt",name = NULL, labels = format_sep) +
    #coord_sf(xlim = c(40, 150), ylim = c(-10, 50), expand = TRUE) +
    theme(legend.position = "none")
asia

layout <- rbind(c(1,1,1,1),
                c(1,1,1,1),
                c(2,2,3,3),
                c(2,2,3,3))

grid.arrange(W, eu, asia,
             layout_matrix = layout)

# Barplot of (10) biggest polluter countries in 2021 ####
# 3) Change breaks on y axis and adjust mean horizontal line
# 4) Annotate horizontal line and what it equals
# 5) Add number of tonnes of CO2 vertically inside each bar (or over the bar)
# 6) Change legend title
# 

# unit -> billions of tonnes of CO2

countries_lvl <- c("China",
                   "United States", 
                   "India", 
                   "Russia", 
                   "Japan",
                   "Iran", 
                   "Germany", 
                   "Saudi Arabia", 
                   "Indonesia", 
                   "South Korea")

co2_names_2 <- co2_names %>% 
    filter(Year == 2021) %>% 
    data.frame() 

mean_emissions <- co2_names %>% filter(Year == 2021) %>% 
    pivot_longer(!Year, names_to = "Countries", values_to = "value") %>% 
    summarise(mean = mean(value,na.rm = T),
              N = n()) %>% 
    select(mean) %>% 
    mutate(mean = mean/1e9) %>% 
    pull(mean)

df <- data.frame("Country" = colnames(co2_names_2)[-1],
                 "CO2_2021" = as.numeric(co2_names_2[1,])[-1]) %>% 
    arrange(desc(CO2_2021)) %>% 
    filter(Country %in% c("China", "United.States", "India", "Russia", "Japan",
                          "Iran", "Germany", "Saudi.Arabia", "Indonesia", 
                          "South.Korea")) %>% 
    mutate(across("Country", str_replace, "United.States", "United States")) %>% 
    mutate(across("Country", str_replace, "South.Korea", "South Korea")) %>% 
    mutate(across("Country", str_replace, "Saudi.Arabia", "Saudi Arabia")) %>% 
    mutate(CO2_2021 = CO2_2021/1e9) %>% 
    mutate(Country = fct_relevel(Country, countries_lvl))

labels <- c(
    "South Korea" = "<img src='data/flags/South_Korea.png'
    width='50' /><br>*South Korea*",
    "Indonesia" = "<img src='data/flags/Indonesia.png'
    width='50' /><br>*Indonesia*",
    "Saudi Arabia" = "<img src='data/flags/Saudi_Arabia.png'
    width='50' /><br>*Saudi Arabia*",
    "Germany" = "<img src='data/flags/Germany.png'
    width='50' /><br>*Germany*",
    "Iran" = "<img src='data/flags/Iran.png'
    width='53' /><br>*Iran*",
    "Japan" = "<img src='data/flags/Japan.png'
    width='50' /><br>*Japan*",
    "Russia" = "<img src='data/flags/Russia.png'
    width='50' /><br>*Russia*",
    "India" = "<img src='data/flags/India.png'
    width='50' /><br>*India*",
    "United States" = "<img src='data/flags/USA.png'
    width='61' /><br>*USA*",
    "China" = "<img src='data/flags/China.png'
    width='50' /><br>*China*"
)

ggplot(data = df, aes(x = factor(Country), y = CO2_2021, fill = CO2_2021)) + 
    geom_bar(stat = "identity") + 
    geom_hline(yintercept = mean_emissions, linetype = "dashed",
               color = "red") + 
#    coord_flip(clip = "on") + # it doesn't work with element_markdown() <- delete this line
    scale_fill_viridis(option = "plasma") +
    scale_x_discrete(name = NULL,
                     labels = labels) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Top 10 biggest country emitters of CO2 in 2021",
         y = TeX(r'($CO_2$ emissions (billion of tonnes) )'),
         caption  = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources") + 
    guides(fill = guide_colorbar(title = "Amount")) +
    theme(#axis.text.x = element_markdown(color = "black", size = 11),
          plot.caption = element_text(face = "italic"))


# Barplot of biggest polluter celebrities in 2022 ####
# 2) Add photos of celebrities over appropriate bars
# 4) Add average person CO2 emissions over the plot
# 5) Add number of tonnes of CO2 vertically inside each bar (or over the bar)
# 7) Add average person CO2 emissions per year (find source for this value)

celebs %>% as_tibble

celeb_lvl <- c("Taylor Swift",
               "Drake", 
               "Floyd Mayweather", 
               "Jay-Z", 
               "Kim Kardashian",
               "A-Rod", 
               "Steven Spielberg", 
               "Mark Wahlberg", 
               "Blake Shelton", 
               "Jack Nicklaus")

celebs <- celebs %>% 
    rename("Celebrity.Name" = Celebrity.Jet) %>% 
    mutate(Celebrity.Name = fct_relevel(Celebrity.Name, celeb_lvl))


ggplot(data = celebs, aes(x = Celebrity.Name,y = CO2e.tonnes.)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = seq(0, 3000, by = 500),
                       limits = c(0,3500),
                       expand = c(0,0)) +
    labs(title = "Table 1. Example title",
         y = TeX(r'($CO_2$ emissions in tonnes )'),
         x = "Celebrity name",
         caption = "Source:WeAreYard\nhttps://weareyard.com/insights/worst-celebrity-private-jet-co2-emission-offenders") +
    theme(plot.caption = element_text(face = "italic"),
          #axis.text.x = element_text(vjust = 1)
          )

    
    
# Decomposition of world emission over time by industry sectors ####
# 1) add logos for each sector (?)
# 2) maybe change to percentage of total emissions and make 
#    it fill the whole area of the plot to show change of distribution over time
# 3) or Add another plot in grid with the above while keeping the existing one
sectors <- ghg_sectors %>% 
    select(-Entity, -Code) %>% 
    pivot_longer(!Year, names_to = "Industry", values_to = "Emissions") %>% 
    mutate(Emissions = Emissions/1e9)

sectors <- ghg_sectors %>% 
    select(-Entity, -Code) %>% 
    pivot_longer(!Year, names_to = "Industry", values_to = "Emissions") %>% 
    mutate(Emissions = Emissions/1e9) %>% 
    group_by(Year, Industry) %>% 
    summarise(n = sum(Emissions)) %>% 
    mutate(percentage = n /sum(n))

sectors %>% tail()

ggplot(sectors, aes(x = Year, y = Emissions, fill = Industry)) +
    geom_area(color = "black", linetype = 1, size = .25, alpha = 0.8) +
    guides(fill = guide_legend(title = "Sectors",
                               byrow = T)) +
    labs(title = "Table 1. Example title",
         y = TeX(r'($CO_2$ emissions (billion of tonnes) )'),
         caption  = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources") + 
    scale_fill_discrete(labels = c("Agriculture",
                                   "Aviation and shipping",
                                   "Buildings",
                                   "Electricity and heat",
                                   "Fugitive emissions",
                                   "Industry",
                                   "Land use change and forestry",
                                   "Manufacturing and construction",
                                   "Other fuel combustion",
                                   "Transport",
                                   "Waste")) +
    scale_x_continuous(expand = c(0,0), limits = c(1990,2019),
                       breaks = seq(1990, 2020, by = 2)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(plot.title = element_text(),
          legend.spacing.y = unit(0.2, "cm"))
    

# percentage structure over time
ggplot(sectors, aes(x = Year, y = percentage, fill = Industry)) +
    geom_area(color = "black", linetype = 1, size = .25, alpha = 0.8) +
    guides(fill = guide_legend(title = "Sectors",
                               byrow = T)) +
    labs(title = "Table 1. Example title",
         y = "Percentage (%) share of industry emissions in total emissions",
         caption  = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources") + 
    scale_fill_discrete(labels = c("Agriculture",
                                   "Aviation and shipping",
                                   "Buildings",
                                   "Electricity and heat",
                                   "Fugitive emissions",
                                   "Industry",
                                   "Land use change and forestry",
                                   "Manufacturing and construction",
                                   "Other fuel combustion",
                                   "Transport",
                                   "Waste")) +
    scale_x_continuous(expand = c(0,0), limits = c(1990,2019),
                       breaks = seq(1990, 2020, by = 2)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(plot.title = element_text(),
          legend.spacing.y = unit(0.2, "cm"))

# Total Decomposition with treemapify (14_AV_adv_plots_p1) ####
# 1) Change Colors of fill name accoriding to previous color scheme

ghg_sectors %>% select(-Entity, -Code) %>% 
    pivot_longer(-Year) %>% 
    pivot_wider(names_from = Year, values_from = value) %>% 
    mutate(Total = rowSums(across(where(is.numeric)),na.rm = T)) %>% 
    mutate(Total = Total/1e9) %>%
    mutate_at("Total", round, digits = 2) %>% 
    mutate(unit = "bln t of CO2") %>% 
    mutate(share = Total/sum(Total) * 100) %>% 
    mutate_at("share", round, digits = 2) %>% 
    mutate(percent = "%")%>% 
    select(name, Total, unit, share, percent) %>%
    mutate(across("name", str_replace, "Land.use.change.and.forestry", "Land use change\nand forestry")) %>% 
    mutate(across("name", str_replace, "Manufacturing.and.construction", "Manufacturing and construction")) %>% 
    mutate(across("name", str_replace, "Electricity.and.heat", "Electricity and heat")) %>% 
    mutate(across("name", str_replace, "Fugitive.emissions", "Fugitive emissions")) %>% 
    mutate(across("name", str_replace, "Other.fuel.combustion", "Other fuel\ncombustion")) %>% 
    mutate(across("name", str_replace, "Aviation.and.shipping", "Aviation and shipping")) %>% 
    ggplot(aes(area = Total,
               label = paste0(name,"\n",Total," ", unit,"\n", share, percent), fill = name)) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = T) + # ZASTANÓW SIĘ CZY TO ZMIENIĆ CZY NIE 
    labs(title = "Structure of total emissions by industry since 1990",
         caption = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources") +
    theme(legend.position = "none") 
    

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


