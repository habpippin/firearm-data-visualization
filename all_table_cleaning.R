#---------------------------------
# Title: Data Cleaning for Firearm Study
# Author: Hayley Pippin
# Date: 05/04/2021
# Description: Imports, cleans, and arranges data tables for chart generation.
# Inputs: 7 CSVs with firearm package data: table1.csv, table2.csv, table3.csv, table4.csv, table5.csv, table6.csv,
#         table6_4levels.csv
# Outputs: the variables defined in this script will be sourced into plotting scripts to create charts
#---------------------------------

## ==== set up ==== ##

# set working directory to folder where firearm data csvs are stored
setwd("~/Documents/db_srbp/firearm_package_laws")

## ==== import and do basic cleaning for all table data ==== ##

# table 1 - type of legislative action enacted, by year (2014-2020)
table1 <- read.csv('table1.csv')
table1 <- cbind(table1, c(17,3,16,14)) # add column of missing data for 2020
colnames(table1) <- c("Legislative Action Enacted", 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# table 2 - dominant type of firearm package law enacted, by state (2014-2020)
table2 <- read.csv('table2.csv')
colnames(table2) <- c("Mostly Nothing", "Mixed", "Mostly Gun Control", "Mostly Gun Rights")
# create one column of U.S. states and one column for numeric equivalent of law type 
table2_numeric <- data.frame(c(table2[1:9, 1], table2[1:29, 2], table2[1:10, 3], table2[1:2, 4]), rep(0, length.out = 50)) 
table2_numeric[1:9, 2] <- 2 # nothing 
table2_numeric[39:48, 2] <- 1 # restriction
table2_numeric[49:50, 2] <- -1 # expansion
# 0 is both
colnames(table2_numeric) <- c("state", "legislative_action")

# table 3 - type of package law enacted in 2013
table3 <- read.csv('table3.csv')

# table 4 - number of  laws in firearm package, by state and package type
table4 <- read.csv('table4.csv')
table4_gun_control <- table4[2:8, 1:2]
colnames(table4_gun_control) <- c("State", "Number of Laws in Package")
table4_gun_rights <- table4[2:5, 3:4]
colnames(table4_gun_rights) <- c("State", "Number of Laws in Package")
table4_both <- table4[2:12, 5:8]
colnames(table4_both) <- c("State", "Number of Gun Control Laws", "Number of Gun Rights Laws", "Total Laws in Package")

# table 5 - number of states that enacted laws, by package type (2013 and 2014-2020)
table5 <- read.csv('table5.csv')

# table 6 - dominant type of legislative action enacted, by state (2013)
# table 6 (4 levels) - dominant type (including new type) of legislative action enacted, by state (2013)
table6 <- read.csv("table6.csv")
table6 <- table6[, c(1,2)]
table6_4levels <- read.csv("table6_4levels.csv")







