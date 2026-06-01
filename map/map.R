
## Script name: ====

## Script objective: ====

## Author: Elvis F. Elli ====

## Script was created on: 2025-11-04====

## Cleaning up environment ====
rm(list=ls())

## Libraries ====
library(rstudioapi)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)

## Set working directory ====
setwd(dirname(getActiveDocumentContext()$path))

#loading dataframe

source("data.frame.R")
source("plot_theme.R")

soybean.hi.data

# World map data
world_map <- map_data("world")

# Create the map plot
ggplot() +
  # Plot world map
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "black") +
  
  # Plot research locations
  geom_point(data = soybean.hi.data, 
             aes(x = Long, y = Lat, fill = Country), size = 5,colour="black",shape=21,
             alpha = 0.8) +
  
  # Add labels for some key locations
  #geom_text_repel(data = soybean.hi.data %>% 
  #                  group_by(Country) %>% 
  #                  slice(1),
  #                aes(x = Long, y = Lat, label = Country),
  #                size = 3, box.padding = 0.5) +
  #
  scale_fill_viridis_d(name = "Country") +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  temp+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("p1.tiff",width=25,height=15,units ="cm",dpi=600,compression="lzw",bg="white")


# Additional analysis: Research by country
country_summary <- soybean.hi.data %>%
  group_by(Country) %>%
  summarise(
    Number_of_Studies = n(),
    Year_Range = paste(min(Paper_Year), "-", max(Paper_Year)),
    .groups = 'drop'
  ) %>%
  arrange(desc(Number_of_Studies))

print(country_summary)

# Research by decade
soybean.hi.data %>%
  mutate(Decade = floor(Paper_Year / 10) * 10) %>%
  count(Decade) %>%
  ggplot(aes(x = factor(Decade), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(title = "Soybean Research Publications by Decade",
       x = "Decade",
       y = "Number of Studies") +
  theme_minimal()

# Research focus areas
focus_summary <- soybean.hi.data %>%
  count(Research_Focus) %>%
  arrange(desc(n))

print(focus_summary)
