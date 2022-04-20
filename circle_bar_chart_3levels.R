#---------------------------------
# Title: Circular Bar Chart (3 Levels)
# Author: Hayley Pippin
# Date: 05/04/2021
# Description: Creates a circular bar chart based off the number of non-packaged firearm laws states passed in 2013.
#              Color-coded based on type of law passed. Uses data from table 3. 
#              Adapted from https://r-graph-gallery.com/297-circular-barplot-with-groups.html
#              License: https://github.com/holtzy/R-graph-gallery/blob/master/LICENSE
# Inputs: all_tables_cleaning.csv
# Outputs: circular bar chart of the number of non-packed laws states passed in 2013, by type of law
#---------------------------------

## ==== set up ==== ##

# set working directory to folder where firearm data csvs are stored
setwd("~/Documents/db_srbp/firearm_package_laws")
# bring variables from data cleaning script into environment
source("~/Documents/db_srbp/firearm_package_laws/all_table_cleaning.R")
# import necessary packages
library("tidyverse")
library("dplyr")

## ==== filter and clean table 3 data ==== ##

# remove the states that did not enact laws in 2013
table3_clean <- table3[table3$type_of_law != 'Nothing',]
# convert type_of_law column to factor type
table3_clean$type_of_law <- as.factor(table3_clean$type_of_law)

## ==== add spacing between legislative type sections for chart ==== ##

# value corresponds to how many bars are used to space sections
empty_bar <- 1 
# create matrix of NA values to add spacing rows table3_clean 
to_add <- data.frame( matrix(NA, nrow = empty_bar*nlevels(table3_clean$type_of_law), ncol = ncol(table3_clean)) )
# rename columns of spacing matrix
colnames(to_add) <- colnames(table3_clean)
# add legislative type column - one type per row
to_add$type_of_law <- rep(levels(table3_clean$type_of_law), each = empty_bar)
# add the rows from the spacing matrix to the bottom of table3_clean
table3_clean <- rbind(table3_clean, to_add)
# order the table so that the rows are grouped by type of law and the NA spacing values are dispersed for spacing
table3_clean <- table3_clean %>% arrange(type_of_law)
# fix id values
table3_clean$id <- seq(1, nrow(table3_clean))

## ==== prepare chart formatting ==== ##

# create copy of table 3 data for data labels
label_data <- table3_clean
# create column of data labels ("STATE ABBREVE – NUMBER OF LAWS PASSED")
label_data$chart_labels <- paste(label_data$state, label_data$num_laws, sep = " – ")
# determine how many labels there are and which bar to assign them to
number_of_bar <- nrow(label_data)
# define angle for label
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar
# arrange so label always reads left to right
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# define lines to trace below legislative type sections
base_data <- table3_clean %>% 
  group_by(type_of_law) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

## ==== create circle chart ==== ##

circle_plot <- ggplot(table3_clean, aes(x = as.factor(id), y = as.numeric(num_laws), fill = type_of_law)) + # note that id is a factor. if x is numeric, there is some space between the first bar and the next section     
  geom_bar(stat = "identity", alpha=0.8) +
  scale_fill_manual(values = c("Restrictive" = "darkred", "Expansive" = "aquamarine4", "Both" = "royalblue3")) +
  ylim(-2,5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y = as.numeric(num_laws)+0.5, label = chart_labels, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  # add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.25, xend = end, yend = -0.25), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )

circle_plot



