#---------------------------------
# Title: U.S. Hexbin Map (4 Levels)
# Author: Hayley Pippin
# Date: 05/04/2021
# Description: Creates a hexbin map of the U.S. filled in major type of legislative action passed in each state 
#              from 2014-2020. Uses data from table 2. 
#              Adapted from code from the R Graph Gallery: https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
#              License: https://github.com/holtzy/R-graph-gallery/blob/master/LICENSE
# Inputs: all_tables_cleaning.R script, us_states_hexgrid.geojson, state_abbreve.csv
# Outputs: U.S. hexbin map of type of state firearm legislative action passed from 2014-2020 
#---------------------------------

## ==== set up ==== ##

# set working directory to folder where firearm data csvs are stored
setwd("~/Documents/db_srbp/firearm_package_laws")
# bring variables from data cleaning script into environment
source("~/Documents/db_srbp/firearm_package_laws/all_table_cleaning.R")
# import necessary packages
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

## ==== define blank u.s. hexbin object ==== ##

# read in map object
# download object here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
hex_map <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
# reformat name data
hex_map@data = hex_map@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

## ==== add state abbreviations to map ==== ##

# create data frame out of hexgrid u.s. map object
hex_map_fortified <- tidy(hex_map, region = "google_name")
# remove rows for washington, d.c.
hex_map_fortified <- hex_map_fortified[-c(57:63), ]
# get centers of the state hexagons
centers <- cbind.data.frame(data.frame(gCentroid(hex_map, byid=TRUE), id=hex_map@data$iso3166_2))
# remove row for washington, d.c.
centers <- centers[-23, ]

# create ggplot version of u.s. hexbin map - FOR TESTING
# ggplot() +
#  geom_polygon(data = hex_map_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
#  geom_text(data=centers, aes(x=x, y=y, label=id)) +
#  theme_void() +
#  coord_map()

## ==== create "gradient" of gun laws ==== ##
# from all_table_cleaning.R script:
# -1 = firearm expansion
# 0 = firearm restriction and expansion 
# 1 = firearm restriction
# 2 = nothing

# create data frame of states and their two-letter postal abbreviations
state_abbreve <- read.csv('state_abbreve.csv', header = FALSE, col.names = c("state", "abbreve"))
# add the abbreviations as a column in table 2
table2_merge <- left_join(table2_numeric, state_abbreve, by = c("state" = "abbreve"))
# rename table 2 columns
colnames(table2_merge) <- c("abbreve", "legislative_action", "state")
# add the data from table 2 to the hexbin map dataframe
hex_map_fortified <- hex_map_fortified %>%
  left_join(. , table2_merge, by=c("id"="state")) 
# fix rhode island values - does not copy the correct way
hex_map_fortified$abbreve[267:273] <- "RI"
hex_map_fortified$legislative_action[267:273] <- 1 # mostly restrictive

# make initial chloropleth map - FOR TESTING
# ggplot() +
#  geom_polygon(data = hex_map_fortified, aes(fill =  legislative_action, x = long, y = lat, group = group)) +
#  scale_fill_gradient(trans = "log") +
#  theme_void() +
#  coord_map()

## ==== create u.s. hexbin map with legislative data ==== ##

# define binned intervals for color coding and legend
hex_map_fortified$bin <- cut_interval(hex_map_fortified$legislative_action, 
                             n = 4, 
                             labels=c("Mostly Expansive", "Both", "Mostly Restrictive", "Mostly Nothing"))

# define palette
# palette order: expansive, both, restrictive, nothing 
my_palette <- c("aquamarine4", "cornflowerblue", "darkred", "tan1")

# create map 
legislative_action_map <- ggplot() +
  geom_polygon(data = hex_map_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0.3, alpha=0.8, color = "white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=4, alpha=0.8) +
  theme_void() +
  scale_fill_manual( 
    values = my_palette, 
    name ="Type of Legislative Action",
    guide = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  ggtitle( "State-by-State Firearm Package Legislative Action from 2014-2020" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size= 20, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

legislative_action_map




