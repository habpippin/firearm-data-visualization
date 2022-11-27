#---------------------------------
# Title: U.S. States Chloropleth Map of NICS Reporting Legality 
# Author: Hayley Pippin
# Last updated: November 26, 2022
# Description: Generates chloropleth map of state mental health reporting type.
# Inputs: reporting_type_by_state.csv, us_states.zip shapefile
# Outputs: Chloropleth map of US states coded by mental health reporting legality
#---------------------------------

## ==== set up ==== ##
library(rgdal)
library(dplyr)
library(RColorBrewer)

## ==== import and prepare data ==== ##

# reporting data
# 0 = no reporting allowed
# 1 = reporting allowed but not required
# 2 = reporting required

reporting_data <- read.csv("~/Desktop/reporting_type_by_state.csv")
reporting_data <- reporting_data[-1,2:3]
colnames(reporting_data) <- c("STUSPS", "reporting_type")

# usa states shapefile
usa_states <- readOGR( 
  dsn= "~/Desktop/us_states/", 
  layer="cb_2018_us_state_500k",
  verbose=FALSE
)

# merge reporting data with shapefile data
usa_states@data <- dplyr::left_join(usa_states@data, reporting_data, by = "STUSPS")
#plot(usa_states, xlim = c(-125,-67), ylim=c(25,49))

# make reporting data numeric
usa_states@data$reporting_type <- as.numeric(usa_states@data$reporting_type)

## ==== create chloropleth map ==== ##

# prepare color palette
color_scheme <- brewer.pal(3, "RdYlGn")
color_scheme <- colorRampPalette(color_scheme)(3)

# create intervals of reporting data to color-code map
reporting_type_class <- cut(usa_states@data$reporting_type, 3)
color_scheme <- color_scheme[as.numeric(reporting_type_class)]

# generate chart
plot(usa_states, xlim = c(-125,-67), ylim=c(25,49), col = color_scheme)




