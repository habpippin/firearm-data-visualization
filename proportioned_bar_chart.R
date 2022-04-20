#---------------------------------
# Title: Proportioned Bar Chart
# Author: Hayley Pippin
# Date: 05/04/2021
# Description: Creates bar chart based on the number of states that passed firearm package laws, proportioned by 
#              type of package passed, for 2013 and 2014-2020. Uses data from table 5.
# Inputs: all_tables_cleaning.csv
# Outputs: bar chart proportioned by type of package law passed for 2013 and 2014-2020
#---------------------------------

## ==== set up ==== ##

# set working directory to folder where firearm data csvs are stored
setwd("~/Documents/db_srbp/firearm_package_laws")
# bring variables from data cleaning script into environment
source("~/Documents/db_srbp/firearm_package_laws/all_table_cleaning.R")

## ==== fix table data ==== ##

# change "type of package" column to factor type
# use rev() for the levels so levels are stacked from most to least number of states that enacted that package type
table5$type_of_package <- factor(table5$type_of_package, levels = rev(c("Both", "Restrictive", "Expansive", "Nothing")))

## ==== generate bar chart ==== ##

proportioned_bar_chart <- ggplot(table5, aes(x = year, y = num_states, fill = type_of_package)) +
  geom_col(position = "stack", alpha = 0.6, size = 0.5, color = "white", width = 0.5) +
  # first color = both, second color = restrictive, third color = expansive, fourth color = nothing
  scale_fill_manual(values = rev(c("cornflowerblue", "darkred", "aquamarine4", "tan1"))) +
  labs(fill = "Type of Package") +
  xlab("Year") +
  ylab("Number of States")
  theme_minimal()
  
proportioned_bar_chart
