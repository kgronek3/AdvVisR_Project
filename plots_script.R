#### Beginning ####
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
library(extrafont)
library(ggpubr) # for text_grob instide grid.arrange
library(svglite)
library(RColorBrewer)
extrafont::loadfonts()

source("dataset_cleaning.R")
options(scipen = 10)


# Land and ocean temperature plots ####

theme_general <- theme(
    plot.title = element_text(color = '#4d4d4d',
                              size = 12,
                              face = 'bold',
                              family = 'Tahoma'),
    plot.subtitle = element_text(color = '#999999',
                                 size = 10,
                                 face = 'plain',
                                 family = 'Tahoma'),
    plot.caption = element_text(color = '#999999',
                                size = 12,
                                face = 'plain',
                                family = 'Tahoma'),
    axis.title = element_text(face = "plain",
                              color = "#999999",
                              family = 'Tahoma'),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(color = '#404040',
                             family = 'Tahoma'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    linewidth = 2),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(colour = "#999999",
                                    linetype = 'dashed'),
    axis.line.y = element_line(colour = "#999999",
                               linetype = "solid",
                               linewidth = 1),
    axis.line.x = element_line(colour = "#999999",
                               linetype = "solid",
                               linewidth = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(family = 'Tahoma',
                               face = "bold",
                               size = 11),
    axis.text.x = element_text(family = 'Tahoma',
                               face = "bold",
                               size = 11)
)

lims <- c(-5.5,5)

p1 <- land_temps %>% 
    filter(!is.na(Africa_Temperature)) %>% 
    ggplot(aes(x = date, y = Africa_Temperature)) +
    geom_point(color = "#62938b", alpha = 0.5) +
    labs(title = "Africa") +
    geom_smooth(se = FALSE, method = "gam", linetype = "dashed", linewidth = 1.2, color = "black") +
    scale_y_continuous(limits = lims) + 
    labs(x = element_blank()) +
    labs(y = "Temperature in Celsius") +
    theme_general +
    theme(axis.ticks.x = element_blank())

p2 <- land_temps %>% 
    filter(!is.na(Africa_Temperature)) %>% 
    ggplot(aes(x = date, y = North_America_Temperature)) +
    geom_point(color = "#7d9b49", alpha = 0.5) +
    geom_smooth(se = FALSE, method = "gam", linetype = "dashed", linewidth = 1.2, color = "black") +
    labs(title = "North America") +
    theme_general +
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) + 
    scale_y_continuous(labels = NULL,limits = lims) +
    labs(x = element_blank(), y = element_blank())

p3 <- land_temps %>% 
    filter(!is.na(Africa_Temperature)) %>% 
    ggplot(aes(x = date, y = South_America_Temperature)) +
    geom_point(color = "#858298", alpha = 0.5) +
    geom_smooth(se = FALSE, method = "gam", linetype = "dashed", linewidth = 1.2, color = "black") +
    labs(title = "South America") +
    theme_general + 
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) + 
    scale_y_continuous(labels = NULL,limits = lims) +
    labs(x = element_blank(), y = element_blank())

p4 <- land_temps %>% 
    filter(!is.na(Africa_Temperature)) %>% 
    ggplot(aes(x = date, y = Europe_Temperature)) +
    geom_point(color = "#af594f", alpha = 0.5) +
    geom_smooth(se = FALSE, method = "gam", linetype = "dashed", linewidth = 1.2, color = "black") +
    labs(title = "Europe") +
    theme_general + 
    scale_y_continuous(limits = lims) + 
    labs(x = element_blank()) +
    labs(y = "Temperature in Celsius")

p5 <- land_temps %>% 
    filter(!is.na(Africa_Temperature)) %>% 
    ggplot(aes(x = date, y = Asia_Temperature)) +
    geom_point(color = "#597b93", alpha = 0.5) +
    geom_smooth(se = FALSE, method = "gam", linetype = "dashed", linewidth = 1.2, color = "black") +
    labs(title = "Asia") +
    theme_general + 
    theme(axis.ticks.y = element_blank()) + 
    scale_y_continuous(labels = NULL, limits = lims) + 
    labs(x = element_blank(), y = element_blank())

p6 <- land_temps %>% 
    filter(!is.na(Africa_Temperature)) %>% 
    ggplot(aes(x = date, y = Oceania_Temperature)) +
    geom_point(color = "#b17d44", alpha = 0.5) +
    geom_smooth(se = FALSE, method = "gam", linetype = "dashed", linewidth = 1.2, color = "black") +
    labs(title = "Oceania") +
    theme_general + 
    theme(axis.ticks.y = element_blank()) + 
    scale_y_continuous(labels = NULL, limits = lims) + 
    labs(x = element_blank(), y = element_blank())

layout_temps <- rbind(c(1,2,3),
                      c(4,5,6))

source1 <- textGrob("Source: NOAA National Centers for Environmental information\n https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series",
                    hjust = 1,
                    x = 0.99, y = 0.6,
                    gp = gpar(fontsize = 13, fontfamily = "Tahoma", col = "#999999",
                              fontface = "italic"))


plot_1 <- grid.arrange(p1,p2,p3,p4,p5,p6,
             top = text_grob("Monthly average temperatures by continent (1910 - 2022)",
                             family = "Tahoma", size = 19,
                             face = "bold", color = "#4d4d4d"),
             layout_matrix = layout_temps,
             bottom = source1)

ggsave(file = "plots/1_plot_scatter.svg", plot = plot_1, width = 14, height = 12)


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

land_temps <-  land_temps %>% 
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


ocean_heat <- ggplot(ocean_temps, aes(x = month, y = year)) + 
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
          axis.text.y = element_text(margin = unit(c(0,0.5,0,0), "cm"),
                                     family = 'Tahoma',face = "bold", size = 11),
          axis.text.x = element_text(vjust = 12,
                                     family = 'Tahoma',face = "bold", size = 11),
          plot.title = element_text(color = '#404040', size = 19,
                                    vjust = -8, hjust = 0.5,
                                    face = "italic", family = "Tahoma"),
          axis.text = element_text(color = '#404040',
                                   family = 'Tahoma'),
          legend.title = element_text(color = "#404040",family = "Tahoma", 
                                      size = 12),
          legend.text = element_text(color = '#404040',
                                     size = 12,
                                     face = 'plain',
                                     family = 'Tahoma'))

land_heat <- ggplot(land_temps, aes(x = month, y = year)) + 
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
          axis.text.y = element_text(margin = unit(c(0,0.5,0,0), "cm"),
                                     family = 'Tahoma',face = "bold", size = 11),
          axis.text.x = element_text(vjust = 12,
                                     family = 'Tahoma',face = "bold", size = 11),
          plot.title = element_text(color = '#404040', size = 19,
                                    vjust = -8, hjust = 0.5,
                                    face = "italic", family = "Tahoma"),
          axis.text = element_text(color = '#404040',
                                   family = 'Tahoma'),
          legend.title = element_text(color = "#404040",family = "Tahoma", 
                                      size = 12),
          legend.text = element_text(color = '#404040',
                                     size = 12,
                                     face = 'plain',
                                     family = 'Tahoma'))

source2 <- textGrob("Source: NOAA National Centers for Environmental information\n https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series",
                   hjust = 1, # text alignment
                   x = 0.99, y = 0.6, # footer positioning
                   gp = gpar(fontsize = 13, fontfamily = "Tahoma", col = "#999999",
                             fontface = "italic")) # Italics

plot_2 <- grid.arrange(ocean_heat,land_heat,
             ncol = 2,
             widths = c(1,1.15),
             top = text_grob("World average temperatures heatmap (1880 - 2022)",
                             family = "Tahoma", size = 19,
                             face = "bold", color = "#4d4d4d",vjust = 2),
             bottom = source2)

ggsave(file = "plots/2_plot_heat_map.svg", plot = plot_2, width = 14, height = 12)


# World map of TOTAL (since industrial revolution) emmisions by country ####

co2_codes_2 <- 
    co2_codes %>%
    pivot_longer(-Year) %>% 
    pivot_wider(names_from=Year, values_from=value) %>% 
    mutate(Total = rowSums(across(where(is.numeric)),na.rm = T)) %>%
    rename("iso_a3" = name,
           "total_co2_emissions" = Total) %>% 
    mutate(total_co2_emissions = total_co2_emissions/1e9) %>% 
    select(iso_a3, total_co2_emissions)


world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
    left_join(co2_codes_2, by = "iso_a3")

limits_maps <- c(0.000150000,430)

plot_3 <- world %>% filter(continent != "Antarctica") %>% 
    ggplot() +
    geom_sf(aes(fill =  total_co2_emissions)) +
    scale_fill_gradientn(
        colors = c("#9DBF9E", "#FCB97D", "#A84268"),
        trans = "sqrt",
        na.value = "grey80",
        limits = limits_maps,
        name = NULL,
        breaks = c(1,2,5,10,20,30,50,100,200,300,400),
        guide = guide_colorbar(barwidth = 60,
                               ticks.linewidth = 3/.pt,
                               lablel.position = "bottom",
                               title = "Billions of tons of CO2 emissioned",
                               ticks.colour = "#4d4d4d",
                               title.position = "bottom",
                               title.hjust = 0.5)) +
    coord_sf(expand = FALSE) +
    labs(x = "Longitude", y = "Latitude", 
         title ="World map of countries total emmisions since industrial revolution",
         caption  = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources") +
    theme(legend.position = 'bottom',
          panel.grid.major = element_line(color = "#999999", 
                                          linetype = "dashed",
                                          linewidth = 0.5),
          panel.background = element_rect(fill = "#d7f4fa"),
          panel.border = element_rect(fill = NA),
          plot.title = element_text(color = '#4d4d4d',
                                    size = 27,
                                    face = 'bold',
                                    family = 'Tahoma'),
          plot.subtitle = element_text(color = '#999999',
                                       size = 10,
                                       face = 'plain',
                                       family = 'Tahoma'),
          plot.caption = element_text(color = '#999999',
                                      size = 12,
                                      face = 'plain',
                                      family = 'Tahoma'),
          axis.title = element_text(face = "plain",
                                    color = "#999999",
                                    family = 'Tahoma'),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(color = '#404040',
                                   family = 'Tahoma'),
          axis.ticks = element_blank(),
          axis.line.y = element_line(colour = "#999999", linetype = "solid",
                                     linewidth = 1),
          axis.line.x = element_line(colour = "#999999", linetype = "solid",
                                     linewidth = 1),
          axis.text.y = element_text(family = 'Tahoma',face = "bold", size = 11),
          axis.text.x = element_text(family = 'Tahoma',face = "bold", size = 11),
          legend.title = element_text(color = "#404040",family = "Tahoma", 
                                      size = 12),
          legend.text = element_text(color = '#404040',
                                     size = 12,
                                     face = 'plain',
                                     family = 'Tahoma')
          )
    
ggsave(file = "plots/3_plot_emissions_map.svg", plot = plot_3, width = 14, height = 12)


# Barplot of (10) biggest polluter countries in 2021 ####
# (units == billions of tonnes of CO2)

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
    mutate(Country = fct_relevel(Country, countries_lvl)) %>% 
    mutate(labels_co2 = paste0(as.character(round(CO2_2021, digits = 3)), " bln")) %>% 
    mutate(labels_pos = case_when(Country == "China" ~ CO2_2021 - 2,
                                  Country %in% c("United States", "India") ~ CO2_2021 - 0.8,
                                  Country %in% c("Russia", "Japan") ~ CO2_2021 + 0.9,
                                  TRUE ~ CO2_2021 + 1.2))

labels_flags <- c(
    "South Korea" = "<img src='data/flags/South_Korea.png'
    width='50' /><br>*South Korea*",
    "Indonesia" = "<img src='data/flags/Indonesia.png'
    width='50' /><br>*Indonesia*",
    "Saudi Arabia" = "<img src='data/flags/Saudi_Arabia.png'
    width='50' /><br>*Saudi Arabia*",
    "Germany" = "<img src='data/flags/Germany.png'
    width='50' /><br>*Germany*",
    "Iran" = "<img src='data/flags/Iran.png'
    width='55' /><br>*Iran*",
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

plot_4 <- ggplot(data = df, aes(x = factor(Country), 
                                y = CO2_2021, 
                                fill = factor(Country))) + 
    geom_bar(stat = "identity") + 
    geom_hline(yintercept = mean_emissions, linetype = "dotdash",
               color = "red", linewidth = 1.5) + 
    ggtext::geom_richtext(aes(x = countries_lvl, 
                              y = labels_pos, 
                              label = labels_co2),
                          fill = NA,
                          size = 8,
                          family = "Tahoma",
                          fontface = "italic",
                          label.color = NA,
                          angle = 90
                          ) +
    geom_curve(aes(x = 5.5, xend = 7.5, y = 8.8, yend = mean_emissions + 0.25),
               arrow = arrow(length = unit (0.03, "npc")),
               curvature = -0.6,
               linewidth = 1) +
    ggtext::geom_richtext(aes(x = 4, y = 9.2, 
                              label = "Mean emissions for 2021 are equal to"),
                          fill = NA,
                          size = 8,
                          family = "Tahoma",
                          fontface = "italic",
                          label.color = NA) +
    ggtext::geom_richtext(aes(x = 4, y = 8.9, 
                              label = paste0(round(mean_emissions,digits = 2),
                                             " bln of tonnes of CO2")),
                          fill = NA,
                          size = 8,
                          family = "Tahoma",
                          fontface = "italic",
                          label.color = NA) +
    scale_fill_manual(values=RColorBrewer::brewer.pal(10, "Set3")) + 
    scale_x_discrete(name = NULL,
                     labels = labels_flags) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = c(0,3,6,9,12), 
                       limits = c(0,12)) +
    labs(title = "Top 10 biggest country emitters of CO2 in 2021",
         y = TeX(r'($CO_2$ emissions (billion of tonnes) )'),
         caption  = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources") + 
    theme(plot.title = element_text(color = '#4d4d4d',
                                    size = 27,
                                    face = 'bold',
                                    family = 'Tahoma'),
          plot.subtitle = element_text(color = '#999999',
                                       size = 10,
                                       face = 'plain',
                                       family = 'Tahoma'),
          plot.caption = element_text(color = '#999999',
                                      size = 12,
                                      face = 'plain',
                                      family = 'Tahoma'),
          axis.title = element_text(face = "plain",
                                    color = "#999999",
                                    family = 'Tahoma'),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(color = '#404040',
                                   family = 'Tahoma'),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 2),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = "#999999",
                                          linetype = 'dashed'),
          axis.line.y = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          axis.line.x = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(colour = "#cccccc",
                                            linetype = 'dashed'),
          panel.grid.major.x = element_blank(),
          axis.text.y = element_text(family = 'Tahoma',
                                     face = "bold",
                                     size = 11),
          axis.text.x = element_markdown(color = "black",
                                         size = 11,
                                         family = "Tahoma",
                                         face = "bold"),
          axis.ticks.x = element_blank(),
          legend.position = "none"
)

ggsave(file = "plots/4_plot_countries.svg", plot = plot_4, width = 14, height = 12)


# Barplot of biggest polluter celebrities in 2022 ####

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

labels_celeb <- c(
    "Taylor Swift" = "*Singer*<br><img src='data/photos/Taylor_Swift.jpg'
    width='64' />",
    "Drake" = "*Rapper*<br><img src='data/photos/Drake.jpg'
    width='80' />",
    "Floyd Mayweather" = "*Boxer*<br><img src='data/photos/Floyd_Mayweather.jpg'
    width='67' />",
    "Jay-Z" = "*Rapper*<br><img src='data/photos/Jay-Z.jpg'
    width='70' />",
    "Kim Kardashian" = "*Influencer*<br><img src='data/photos/Kim_Kardashian.jpg'
    width='70' />",
    "A-Rod" = "*Baseballer*<br><img src='data/photos/A-Rod.jpg'
    width='70' />",
    "Steven Spielberg" = "*Director*<br><img src='data/photos/Steven_Spielberg.jpg'
    width='70' />",
    "Mark Wahlberg" = "*Actor*<br><img src='data/photos/Mark_Wahlberg.jpg'
    width='70' />",
    "Blake Shelton" = "*Singer*<br><img src='data/photos/Blake_Shelton.jpg'
    width='70' />",
    "Jack Nicklaus" = "*Golfer*<br><img src='data/photos/Jack_Nicklaus.jpg'
    width='70' />"
)

plot_5 <- celebs %>% 
    rename("Celebrity.Name" = Celebrity.Jet) %>% 
    mutate(Celebrity.Name = fct_relevel(Celebrity.Name, celeb_lvl)) %>% 
    mutate(labels_co2 = paste0(as.character(round(CO2e.tonnes., digits = 0)), " tonnes")) %>% 
    ggplot(aes(x = Celebrity.Name,y = CO2e.tonnes., fill = Celebrity.Name)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=RColorBrewer::brewer.pal(10, "Spectral")) + 
    # photos
    ggtext::geom_richtext(aes(x = Celebrity.Name, y = CO2e.tonnes. + 400, label = labels_celeb), 
                          fill = NA,
                          size = 5,
                          fontface = "italic",
                          label.color = NA) +
    # emission of one person in a lifetime text annotation
    ggtext::geom_richtext(aes(x = "A-Rod", y = 4000, 
                              label = "Lifetime emissions"),
                          fill = NA,
                          size = 5,
                          fontface = "italic",
                          label.color = NA) +
    # bar annotations
    ggtext::geom_richtext(aes(x = celeb_lvl,
                              y = CO2e.tonnes. - 500,
                              label = labels_co2),
                          fill = NA,
                          size = 5,
                          fontface = "italic",
                          label.color = NA,
                          angle = 90) +
    scale_y_continuous(breaks = seq(0, 4000, by = 500),
                       limits = c(0,4500),
                       expand = c(0,0)) +
    labs(title = "Top 10 biggest polluter celebrities in 2022",
         y = TeX(r'($CO_2$ emissions in tonnes )'),
         x = element_blank(),
         caption = "Source:WeAreYard\nhttps://weareyard.com/insights/worst-celebrity-private-jet-co2-emission-offenders") +
    theme(legend.position = "none",
          plot.title = element_text(color = '#4d4d4d',
                                    size = 27,
                                    face = 'bold',
                                    family = 'Tahoma'),
          plot.subtitle = element_text(color = '#999999',
                                       size = 10,
                                       face = 'plain',
                                       family = 'Tahoma'),
          plot.caption = element_text(color = '#999999',
                                      size = 12,
                                      face = 'plain',
                                      family = 'Tahoma'),
          axis.title = element_text(face = "plain",
                                    color = "#999999",
                                    family = 'Tahoma'),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(color = '#404040',
                                   family = 'Tahoma'),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 2),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = "#999999",
                                          linetype = 'dashed'),
          axis.line.y = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          axis.line.x = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(colour = "#cccccc",
                                            linetype = 'dashed'),
          panel.grid.major.x = element_blank(),
          axis.text.y = element_text(family = 'Tahoma',
                                     face = "bold",
                                     size = 11),
          axis.text.x = element_text(family = 'Tahoma',
                                     face = "bold",
                                     size = 11)
          )

ggsave(file = "plots/5_plot_celebrities.svg", plot = plot_5, width = 14, height = 12)


# Decomposition of world emission over time by industry sectors ####

sectors2 <- ghg_sectors %>% 
    select(-Entity, -Code) %>% 
    pivot_longer(!Year, names_to = "Industry", values_to = "Emissions") %>% 
    mutate(Emissions = Emissions/1e9) %>% 
    group_by(Year, Industry) %>% 
    summarise(n = sum(Emissions)) %>% 
    mutate(percentage = n /sum(n))

beg_structure <- sectors2 %>% 
    filter(Year == 1990) %>%
    arrange(desc(Industry)) %>% 
    mutate(cum_per = cumsum(percentage))

beg_structure$new <- rep(0,length(beg_structure$cum_per))
beg_structure$new[1] <- beg_structure$cum_per[1]/2     
for (i in 2:length(beg_structure$cum_per)) {
beg_structure$new[i] <- beg_structure$cum_per[i] - 
    (beg_structure$cum_per[i] - beg_structure$cum_per[i-1])/2

}

beg_structure_1 <- beg_structure %>% 
    pull(new)

beg_structure_2 <- beg_structure %>% 
    mutate(percentage = percentage * 100) %>% 
    mutate_at("percentage", round, digits = 2) %>% 
    mutate(labels_beg = paste0(percentage, " %")) %>% 
    pull(labels_beg)

end_structure <- sectors2 %>% 
    filter(Year == 2019) %>%
    arrange(desc(Industry)) %>% 
    mutate(cum_per = cumsum(percentage))

end_structure$new <- rep(0,length(end_structure$cum_per))
end_structure$new[1] <- end_structure$cum_per[1]/2     
for (i in 2:length(end_structure$cum_per)) {
    end_structure$new[i] <- end_structure$cum_per[i] - 
        (end_structure$cum_per[i] - end_structure$cum_per[i-1])/2
    
}

end_structure_1 <- end_structure %>% 
    pull(new)

end_structure_2 <- end_structure %>% 
    mutate(percentage = percentage * 100) %>% 
    mutate_at("percentage", round, digits = 2) %>% 
    mutate(labels_beg = paste0(percentage, " %")) %>% 
    pull(labels_beg)

plot_6 <- ggplot(sectors2, aes(x = Year, y = percentage, fill = Industry)) +
    geom_area(color = "black", linetype = 1, linewidth = .25, alpha = 0.8) +
    guides(fill = guide_legend(title = "Sectors",
                               byrow = T)) +
    labs(title = "CO2 emissions structure by sectors (1990-2019)",
         y = "Percentage (%) share of industry emissions in total emissions",
         x = NULL,
         caption  = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources") +
    scale_fill_manual(values=c(RColorBrewer::brewer.pal(10, "Paired"), 
                               RColorBrewer::brewer.pal(12, "Paired")[12]),
                      labels = c("Agriculture",
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
    scale_y_continuous(expand = c(0,0),
                       breaks = beg_structure_1,
                       labels = beg_structure_2,
                       sec.axis = sec_axis(trans = ~., 
                                           labels = end_structure_2,
                                           breaks = end_structure_1)) +
    theme(plot.title = element_text(color = '#4d4d4d',
                                    size = 27,
                                    face = 'bold',
                                    family = 'Tahoma'),
          plot.subtitle = element_text(color = '#999999',
                                       size = 10,
                                       face = 'plain',
                                       family = 'Tahoma'),
          plot.caption = element_text(color = '#999999',
                                      size = 12,
                                      face = 'plain',
                                      family = 'Tahoma'),
          axis.title = element_text(face = "plain",
                                    color = "#999999",
                                    family = 'Tahoma'),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(color = '#404040',
                                   family = 'Tahoma'),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 2),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = "#999999",
                                          linetype = 'dashed'),
          axis.line.y = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          axis.line.x = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(colour = "#cccccc",
                                            linetype = 'dashed'),
          panel.grid.major.x = element_blank(),
          axis.text.y = element_text(family = 'Tahoma',
                                     face = "bold",
                                     size = 12),
          axis.text.x = element_text(family = 'Tahoma',
                                     face = "bold",
                                     size = 11,
                                     angle = 45,
                                     vjust = 0.5),
          legend.spacing.y = unit(0.2, "cm")
          )

ggsave(file = "plots/6_plot_decomposition_sectors.svg", plot = plot_6, width = 14, height = 12)


# Total Decomposition with treemapify ####

plot_7 <- ghg_sectors %>% select(-Entity, -Code) %>% 
    pivot_longer(-Year) %>% 
    pivot_wider(names_from = Year, values_from = value) %>% 
    mutate(Total = rowSums(across(where(is.numeric)),na.rm = T)) %>% 
    mutate(Total = Total/1e9) %>%
    mutate_at("Total", round, digits = 2) %>% 
    mutate(share = Total/sum(Total) * 100) %>% 
    mutate_at("share", round, digits = 2) %>% 
    mutate(percent = "%")%>% 
    select(name, Total, share, percent) %>%
    mutate(across("name",
                  str_replace,
                  "Land.use.change.and.forestry",
                  "Land use change\nand forestry")) %>% 
    mutate(across("name",
                  str_replace,
                  "Manufacturing.and.construction",
                  "Manufacturing and construction")) %>% 
    mutate(across("name",
                  str_replace,
                  "Electricity.and.heat",
                  "Electricity and heat")) %>% 
    mutate(across("name",
                  str_replace,
                  "Fugitive.emissions",
                  "Fugitive emissions")) %>% 
    mutate(across("name",
                  str_replace,
                  "Other.fuel.combustion",
                  "Other fuel\ncombustion")) %>% 
    mutate(across("name",
                  str_replace,
                  "Aviation.and.shipping",
                  "Aviation and shipping")) %>% 
    ggplot(aes(area = Total,
               label = paste0(name,"\n",
                              share, percent,
                              "\n(",Total,")"), fill = name)) +
    geom_treemap() +
    geom_treemap_text(fontface = "italic",
                      colour = "white",
                      place = "centre",
                      grow = T) + 
    scale_fill_manual(values=c(RColorBrewer::brewer.pal(10, "Paired"), 
                               RColorBrewer::brewer.pal(12, "Paired")[12])) +
    labs(title = "Structure of total emissions by industry since 1990",
         caption = "Source: Our World In Data\nhttps://ourworldindata.org/co2-dataset-sources",
         subtitle = "Total number of billiones of tonnes of CO2 emissioned in parenthesis") +
    theme(legend.position = "none",
          plot.title = element_text(color = '#4d4d4d',
                                    size = 27,
                                    face = 'bold',
                                    family = 'Tahoma'),
          plot.subtitle = element_text(color = '#999999',
                                       size = 10,
                                       face = 'plain',
                                       family = 'Tahoma'),
          plot.caption = element_text(color = '#999999',
                                      size = 12,
                                      face = 'plain',
                                      family = 'Tahoma'),
          axis.title = element_text(face = "plain",
                                    color = "#999999",
                                    family = 'Tahoma'),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 15),
          axis.text = element_text(color = '#404040',
                                   family = 'Tahoma'),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 2),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = "#999999",
                                          linetype = 'dashed'),
          axis.line.y = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          axis.line.x = element_line(colour = "#999999",
                                     linetype = "solid",
                                     linewidth = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_line(colour = "#cccccc",
                                            linetype = 'dashed'),
          panel.grid.major.x = element_blank(),
          axis.text.y = element_text(family = 'Tahoma',
                                     face = "bold",
                                     size = 11),
          axis.text.x = element_text(family = 'Tahoma',
                                     face = "bold",
                                     size = 11)
)

ggsave(file = "plots/7_plot_decomposition_total.svg", plot = plot_7, width = 14, height = 12)



