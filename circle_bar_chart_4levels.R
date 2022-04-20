#---------------------------------
# Title: Circular Bar Chart (4 Levels)
# Author: Hayley Pippin
# Date: 05/06/2021
# Description: Creates a circular bar chart based off the number of non-packaged firearm laws states passed 2014-2020.
#              Color-codes based on type of law passed. Uses data from table 4. 
#              Adapted from https://r-graph-gallery.com/297-circular-barplot-with-groups.html
#              License: https://github.com/holtzy/R-graph-gallery/blob/master/LICENSE
# Inputs: all_tables_cleaning.csv
# Outputs: circular bar chart of the number of non-packed laws states passed 2014-2020, by type of law
#---------------------------------

## ==== set up ==== ##

# set working directory to folder where firearm data csvs are stored
setwd("~/Documents/db_srbp/firearm_package_laws")
# bring variables from data cleaning script into environment
source("~/Documents/db_srbp/firearm_package_laws/all_table_cleaning.R")
# import necessary packages
library("tidyverse")
library("dplyr")

## ==== reformat data table ==== ##

# add columns for legislation type to tables
firearm_restricted <- rep.int("Firearm Restrictive", 7)
table4_clean <- cbind(table4_gun_control, firearm_restricted) # creating data frame that all tables will be merged into
names(table4_clean)[3] <- "Type of Legislation" # rename column
firearm_expansive <- rep.int("Firearm Expansive", 4)
table4_gun_rights <- cbind(table4_gun_rights, firearm_expansive)
names(table4_gun_rights)[3] <- "Type of Legislation" # rename column
# creating column to distinguish between type of package for states that passed restrictive and expansive laws
both <- c("Both - Firearm Restrictive", "Both - Firearm Restrictive", "Both - Firearm Restrictive", "Both - Firearm Expansive",
          "Both - Firearm Restrictive", "Both - Firearm Expansive", "Both - Firearm Restrictive", "Both - Firearm Restrictive",
          "Both - Firearm Expansive", "Both - Firearm Restrictive", "Both - Firearm Expansive")
table4_both <- cbind(table4_both, both)
# remove "number of laws" columns
table4_both <- table4_both[, -c(2,3)]
names(table4_both)[c(2,3)] <- c("Number of Laws in Package", "Type of Legislation")
# consolidate "gun rights" and "both" tables into table4_clean
table4_clean <- rbind(table4_clean, table4_gun_rights)
table4_clean <- rbind(table4_clean, table4_both)
# convert "legislation type" column into factor type
table4_clean$`Type of Legislation` <- as.factor(table4_clean$`Type of Legislation`)

## ==== add spacing between the legislative type sections for chart ==== ##

# value corresponds to how many bars are used to space sections
empty_bar <- 1
# create matrix of NA values to add spacing rows table4_clean
to_add <- data.frame( matrix(NA, nrow = empty_bar*nlevels(table4_clean$`Type of Legislation`), ncol = ncol(table4_clean)) )
# rename columns of spacing matrix
colnames(to_add) <- colnames(table4_clean)
# add legislative type column - one type per row
to_add$`Type of Legislation` <- rep(levels(table4_clean$`Type of Legislation`), each = empty_bar)
# add the rows from the spacing matrix to the bottom of table4_clean
table4_clean <- rbind(table4_clean, to_add)
# order the table so that the rows are grouped by type of law and the NA spacing values are dispersed for spacing
table4_clean <- table4_clean %>% arrange(`Type of Legislation`)
# fix id values
table4_clean$id <- seq(1, nrow(table4_clean))

## ==== prepare chart formatting ==== ##

# create copy of table 3 data for data labels
label_data <- table4_clean
# create column of data labels ("STATE ABBREVE – NUMBER OF LAWS PASSED")
label_data$chart_labels <- paste(label_data$State, label_data$`Number of Laws in Package`, sep = " – ")
# determine how many labels there are and which bar to assign them to
number_of_bar <- nrow(label_data)
# define angle for label
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar
# arrange so label always reads left to right
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# define lines to trace below legislative type sections
base_data <- table4_clean %>% 
  group_by(`Type of Legislation`) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

## ==== create circle chart ==== ##

circle_plot <- ggplot(table4_clean, aes(x = as.factor(id), y = as.numeric(`Number of Laws in Package`), fill = `Type of Legislation`)) + # note that id is a factor. if x is numeric, there is some space between the first bar
  geom_bar(stat = "identity", alpha=0.8) +
  scale_fill_manual(values = c("Both - Firearm Restrictive" = "cornflowerblue", "Both - Firearm Expansive" = "navy", "Firearm Restrictive" = "darkred", "Firearm Expansive" = "aquamarine4")) +
  ylim(-5,15) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y = as.numeric(`Number of Laws in Package`)+1, label = chart_labels, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  # add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )

circle_plot



