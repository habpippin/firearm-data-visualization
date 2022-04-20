#---------------------------------
# Title: Proportioned Line Chart
# Author: Hayley Pippin
# Date: 05/07/2021
# Description: Creates a proportioned line chart of the number of firearm laws passed per year from 2014 - 2020. 
#              Proportions are based on the type of firearm law that was passed that year. Uses data from table 1.
# Inputs: all_table_cleaning.R script
# Outputs: exportable proportioned line chart
#---------------------------------

## ==== set up ==== ##

# set working directory to folder where firearm data csvs are stored
setwd("~/Documents/db_srbp/firearm_package_laws")
# bring variables from data cleaning script into environment
source("~/Documents/db_srbp/firearm_package_laws/all_table_cleaning.R")
# import necessary packages
library(ggplot2)
library(tidyr)

## ==== select data and reshape table 1 ==== ##

# remove "nothing" category from table
table1_clean <- table1[-1, ]
# add table header as row 1 to get years in table
table1_clean <- rbind(names(table1_clean), table1_clean) 
# transpose table and set as data frame
table1_clean <- as.data.frame(t(table1_clean))
# remove header row
table1_clean <- table1_clean[-1, ]
# name table columns
colnames(table1_clean) <- c("Year", "Firearm Expansive", "Firearm Restrictive", "Both") 
# reshape table into 3 columns: year, type of law, number of laws
table1_clean <- pivot_longer(table1_clean, 2:4, names_to = "Type_of_Law", values_to = "Num_Laws")
# convert year and num_laws column to numeric types
table1_clean$Year <- as.numeric(table1_clean$Year)
table1_clean$Num_Laws <- as.numeric(table1_clean$Num_Laws)
# convert type_of_law column to factor type
table1_clean$Type_of_Law <- factor(table1_clean$Type_of_Law, levels = c("Firearm Expansive", "Firearm Restrictive", "Both"))

## ==== create line chart ==== ##

proportion_chart <- ggplot(table1_clean, aes(x = Year, y = Num_Laws, fill = Type_of_Law)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  # first color = expansive, second color = restrictive, third color = both
  scale_fill_manual(values = c("aquamarine4", "darkred", "cornflowerblue")) + 
  xlab("Year") +
  ylab("Number of Firearm Laws Passed") +
  labs(fill = "Type of Law") +
  theme_minimal()

# print chart to plot viewer
proportion_chart
